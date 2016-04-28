"""
Pairs Trading with O-U process
"""

import numpy as np
import pandas as pd
import pytz
import statsmodels.api as sm


def initialize(context):
    # Quantopian backtester specific variables
    set_slippage(slippage.VolumeShareSlippage(volume_limit=0.025, price_impact=0.1))
    set_commission(commission.PerShare(cost=0.005, min_trade_cost=1.0))

    # the ticker symbols
    # XOM&HD
    context.y = symbol('XOM')
    context.x = symbol('HD')

    # strategy specific variables
    context.lookback = 20  # used for regression
    context.z_window = 20  # used for zscore calculation, must be <= lookback

    context.use_hedge_ratio_lag = True
    context.hedge_ratio_lag = 2

    # trading frequency settings
    context.trade_freq = 'intraday'  # 'daily' or 'intraday'

    # run the (daily) trading logic at 15:30 (30mins before market clos)
    context.daily_run_logic_hours = 15  # run the trading logic at the hours
    context.daily_run_logic_minutes = 30  # run the trading logic at the minutes

    # intraday parameters
    context.intraday_freq = 15  # check history of every 15 mins
    context.run_trading_logic_freq = 30  # run every 30 minutes

    # Specify 'statistical filters' that the pair's spread must pass before before it will be considered for a trade
    context.stat_filter = {}
    # cointegration tests are dismissed here
    context.stat_filter['ou'] = {}
    # Half-life days, computed over 126-day (6-month) lookback window, with a requirement of the half-life being between 1 and 42-days (2-months)
    context.stat_filter['ou']['use'] = True
    context.stat_filter['ou']['lookback'] = 126
    context.stat_filter['ou']['function'] = half_life  # function defined below
    context.stat_filter['ou']['test_condition_min'] = 1.0
    context.stat_filter['ou']['test_condition_max'] = 42.0

    # define a few more global variables based on thise inputs above
    context.intraday_history_lookback = context.lookback * context.intraday_freq + 10
    context.spread = np.array([])
    context.hedge_ratio_history = np.array([])
    context.in_long = False
    context.in_short = False

    if not context.use_hedge_ratio_lag:
        # a lag of 1 means to include the most recent price in the hedge_ratio calculation
        context.hedge_ratio_lag = 1

    context.entry_z = 2.0
    context.exit_z = 0.75


# Will be called on every trade event for the securities you specify. 
def handle_data(context, data):
    if get_open_orders():
        return
    now = get_datetime()
    exchange_time = now.astimezone(pytz.timezone('US/Eastern'))

    # Only trade N-minutes before market close
    if context.trade_freq == 'daily':
        if not (
                exchange_time.hour == context.daily_run_logic_hours and exchange_time.minute == context.daily_run_logic_minutes):
            return

    if context.trade_freq == 'intraday':
        # Only run trading logic every N minutes
        if exchange_time.minute % context.run_trading_logic_freq > 0:
            return

    if context.trade_freq == 'daily':
        prices = history(context.lookback, '1d', 'price').iloc[-context.lookback::]

    if context.trade_freq == 'intraday':
        prices = history(context.intraday_history_lookback, '1m', 'price')
        indexes_at_freq = range(prices.shape[0] - 1, 0, -context.intraday_freq)
        indexes_at_freq.sort()
        prices = prices.ix[indexes_at_freq, :]

    y = prices[context.y]
    x = prices[context.x]

    try:
        hedge = hedge_ratio(y, x, add_const=True)
    except ValueError as e:
        log.debug(e)
        return

    context.hedge_ratio_history = np.append(context.hedge_ratio_history, hedge)
    # Calculate the current day's spread and add it to the running tally
    if context.hedge_ratio_history.size < context.hedge_ratio_lag:
        return

    # Grab the previous day's hedge ratio
    hedge = context.hedge_ratio_history[-context.hedge_ratio_lag]
    context.spread = np.append(context.spread, y[-1] - hedge * x[-1])
    spread_length = context.spread.size

    # reference postion of y (>0 means long position)
    reference_pos = context.portfolio.positions[context.y].amount
    reference_pos_x = context.portfolio.positions[context.x].amount

    # apply all the statistical filters to 'context.spread', if specified in initialize() 
    # can apply adf test in the beginning and add the method functions in the initialize()
    for stat_name in context.stat_filter:
        if context.stat_filter[stat_name]['use']:
            test_lookback = context.stat_filter[stat_name]['lookback']
            if spread_length < test_lookback:
                return
            test_spread = context.spread[-test_lookback:]
            test_func = context.stat_filter[stat_name]['function']
            test_value = test_func(test_spread)
            test_min = context.stat_filter[stat_name]['test_condition_min']
            test_max = context.stat_filter[stat_name]['test_condition_max']

            if stat_name == 'half_life_days':
                record(half_life=test_value)
            if (test_value < test_min) or (test_value > test_max):
                return

    # simple implementation of stop loss
    if reference_pos_x > 0 and zscore > 2.56:
        order_target(context.y, 0)
        order_target(context.x, 0)

    if reference_pos_y > 0 and zscore < -2.56:
        order_target(context.y, 0.0)
        order_target(context.x, 0.0)

    if context.spread.size > context.z_window:
        # Keep only the z-score lookback period
        spreads = context.spread[-context.z_window:]

        zscore = (spreads[-1] - spreads.mean()) / spreads.std()
        record(Z=zscore)

        if context.in_short and zscore < context.exit_z:
            order_target(context.y, 0)
            order_target(context.x, 0)
            context.in_short = False
            context.in_long = False
            return

        if context.in_long and zscore > context.exit_z:
            order_target(context.y, 0)
            order_target(context.x, 0)
            context.in_short = False
            context.in_long = False
            return

        if zscore < -context.entry_z and (not context.in_long):
            y_target_shares = 1
            x_target_shares = -hedge
            context.in_long = True
            context.in_short = False

            (y_target_pct, x_target_pct) = compute_holdings_pct(y_target_shares, x_target_shares, y[-1], x[-1])
            order_target_percent(context.y, y_target_pct)
            order_target_percent(context.x, x_target_pct)
            return

        if zscore > context.entry_z and (not context.in_short):
            # Only trade if NOT already in a trade
            y_target_shares = -1
            x_target_shares = hedge
            context.in_short = True
            context.in_long = False

            (y_target_pct, x_target_pct) = compute_holdings_pct(y_target_shares, x_target_shares, y[-1], x[-1])
            order_target_percent(context.y, y_target_pct)
            order_target_percent(context.x, x_target_pct)


def hedge_ratio(y, x, add_const=True):
    if add_const:
        x = sm.add_constant(x)
        model = sm.OLS(y, x).fit()
        return model.params[1]
    model = sm.OLS(y, x).fit()
    return model.params.values


def compute_holdings_pct(y_shares, x_shares, y_price, x_price):
    y_dollars = y_shares * y_price
    x_dollars = x_shares * x_price
    notional_dollars = abs(y_dollars) + abs(x_dollars)
    y_target_pct = y_dollars / notional_dollars
    x_target_pct = x_dollars / notional_dollars
    return (y_target_pct, x_target_pct)


def half_life(input_ts):
    # returns the [theoretical, based on OU-process equations] number of periods to expect 
    # to have to wait for the spread to mean-revert half the distance to its mean
    price = pd.Series(input_ts)
    lagged_price = price.shift(1).fillna(method="bfill")
    delta = price - lagged_price
    beta = np.polyfit(lagged_price, delta, 1)[0]
    half_life = (-1 * np.log(2) / beta)

    # calculate own threshold for entry_z
    return half_life


def set_trailing_stop(context, data):
    if context.portfolio.positions[context.x].amount:
        price = data[context.x].price
        context.stop_price_x = max(context.stop_price_x, context.stop_pct * price)
    if context.portfolio.positions[context.y].amount:
        price = data[context.y].price
        context.stop_price_y = max(context.stop_price_y, context.stop_pct * price)
