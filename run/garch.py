import numpy as np
import pytz
import statsmodels.api as sm


def initialize(context):
    # Quantopian backtester specific variables
    set_slippage(slippage.VolumeShareSlippage(volume_limit=0.025, price_impact=0.1))
    set_commission(commission.PerShare(cost=0.005, min_trade_cost=1.0))

    # the ticker symbols
    context.y = symbol('WFC')
    context.x = symbol('XOM')

    # strategy specific variables
    context.lookback = 20  # used for regression
    context.z_window = 20  # used for zscore calculation, must be <= lookback

    # simple implementation of Stop loss, stop when current price is 
    context.stop_price_x = 0.0
    context.stop_price_y = 0.0
    context.stop_pct = 0.90

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

    # define a few more global variables based on thise inputs above
    context.intraday_history_lookback = context.lookback * context.intraday_freq + 10
    context.spread = np.array([])
    context.hedge_ratio_history = np.array([])
    context.in_long = False
    context.in_short = False

    # reference postion of y (>0 means long position)
    reference_pos = context.portfolio.positions[context.y].amount
    reference_pos_x = context.portfolio.positions[context.x].amount

    context.exit_z = 0.75
    context.entry_z = 2.0


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

    # simple implementation of stop loss
    if reference_pos_x > 0 and zscore > 2.56:
        order_target(context.y, 0)
        order_target(context.x, 0)

    if reference_pos_y > 0 and zscore < -2.56:
        order_target(context.y, 0.0)
        order_target(context.x, 0.0)

        # simple implementation of simple trailing stop
    set_trailing_stop(context, data)
    if data[context.x].price < context.stop_price_x:
        order_target(context.x, 0)
        context.stop_price_x = 0
    if data[context.y].price < context.stop_price_y:
        order_target(context.y, 0)
        context.stop_price_y = 0

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

            (y_target_pct, x_target_pct) = compute_holdings_pct(y_target_shares,
                                                                x_target_shares,
                                                                y[-1], x[-1])
            order_target_percent(context.y, y_target_pct)
            order_target_percent(context.x, x_target_pct)
            return

        if zscore > context.entry_z and (not context.in_short):
            # Only trade if NOT already in a trade
            y_target_shares = -1
            x_target_shares = hedge
            context.in_short = True
            context.in_long = False

            (y_target_pct, x_target_pct) = compute_holdings_pct(y_target_shares,
                                                                x_target_shares,
                                                                y[-1], x[-1])
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


def set_trailing_stop(context, data):
    if context.portfolio.positions[context.x].amount:
        price = data[context.x].price
        context.stop_price_x = max(context.stop_price_x, context.stop_pct * price)
    if context.portfolio.positions[context.y].amount:
        price = data[context.y].price
        context.stop_price_y = max(context.stop_price_y, context.stop_pct * price)
