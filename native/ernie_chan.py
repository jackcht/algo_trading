#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Pair trading algo written by Ernie Chan.
    This algorithm computes a hedge ratio rather than just holding equal amounts of each security.
    copied from https://www.quantopian.com/posts/how-to-build-a-pairs-trading-strategy-on-quantopian
"""

import numpy as np
import pytz
import statsmodels.api as sm
from zipline.utils import tradingcalendar


def initialize(context):
    # Quantopian backtester specific variables

    # When using the FixedSlippage model, the size of your order does not
    # affect the price of your trade execution. You specify a 'spread' that
    # you think is a typical bid/ask spread to use. When you place a buy
    # order, half of the spread is added to the price; when you place a sell
    # order, half of the spread is subtracted from the price.
    set_slippage(slippage.FixedSlippage(spread=0))

    # If you don't specify a commission, your backtest defaults to $0.03 per share.
    # You can define your trading cost in either dollars per share or dollars per trade.
    set_commission(commission.PerTrade(cost=1))

    # If a ticker was reused by multiple companies, use set_symbol_lookup_date
    # ('YYYY-MM-DD') to specify what date to use when resolving conflicts.
    set_symbol_lookup_date('2014-01-01')
    context.Y = symbol('ABGB')
    context.X = symbol('FSLR')
    # set_benchmark(context.y)


    # strategy specific variables
    context.lookback = 20  # used for regression
    context.z_window = 20  # used for zscore calculation, must be <= lookback

    context.useHRlag = True
    context.HRlag = 2

    context.spread = np.array([])
    context.hedgeRatioTS = np.array([])
    context.inLong = False
    context.inShort = False
    context.entryZ = 1.0
    context.exitZ = 0.0

    if not context.useHRlag:
        # a lag of 1 means no-lag, this is used for np.array[-1] indexing
        context.HRlag = 1


# Will be called on every trade event for the securities you specify.
def handle_data(context, data):
    _Y_value = context.portfolio.positions[context.Y].amount * context.portfolio.positions[context.Y].last_sale_price
    _X_value = context.portfolio.positions[context.X].amount * context.portfolio.positions[context.X].last_sale_price
    _leverage = (abs(_Y_value) + abs(_X_value)) / context.portfolio.portfolio_value
    record(
        X_value=_X_value,
        Y_value=_Y_value,
        leverage=_leverage
    )

    # if get_open_orders() is not empty (which translates to True), then wait until finish all the open orders
    if get_open_orders():
        return

    now = get_datetime()
    exchange_time = now.astimezone(pytz.timezone('US/Eastern'))

    # only run at this specific time (each day)
    if not (exchange_time.hour == 15 and exchange_time.minute == 30):
        return

    prices = history(35, '1d', 'price').iloc[-context.lookback::]

    Y = prices[context.Y]
    X = prices[context.X]

    try:
        hedge = hedge_ratio(Y, X, add_const=True)
    except ValueError as e:
        log.debug(e)
        return

    context.hedgeRatioTS = np.append(context.hedgeRatioTS, hedge)
    # Calculate the current day's spread and add it to the running tally
    if context.hedgeRatioTS.size < context.HRlag:
        return
    # Grab the previous day's hedgeRatio
    hedge = context.hedgeRatioTS[-context.HRlag]
    context.spread = np.append(context.spread, Y[-1] - hedge * X[-1])

    if context.spread.size > context.z_window:
        # Keep only the z-score lookback period
        spreads = context.spread[-context.z_window:]

        zscore = (spreads[-1] - spreads.mean()) / spreads.std()

        if context.inShort and zscore < 0.0:
            order_target(context.Y, 0)
            order_target(context.X, 0)
            context.inShort = False
            context.inLong = False
            record(X_pct=0, Y_pct=0)
            return

        if context.inLong and zscore > 0.0:
            order_target(context.Y, 0)
            order_target(context.X, 0)
            context.inShort = False
            context.inLong = False
            record(X_pct=0, Y_pct=0)
            return

        if zscore < -1.0 and (not context.inLong):
            # Only trade if NOT already in a trade
            y_target_shares = 1
            X_target_shares = -hedge
            context.inLong = True
            context.inShort = False

            (y_target_pct, x_target_pct) = computeHoldingsPct(y_target_shares, X_target_shares, Y[-1], X[-1])
            order_target_percent(context.Y, y_target_pct)
            order_target_percent(context.X, x_target_pct)
            record(Y_pct=y_target_pct, X_pct=x_target_pct)
            return

        if zscore > 1.0 and (not context.inShort):
            # Only trade if NOT already in a trade
            y_target_shares = -1
            X_target_shares = hedge
            context.inShort = True
            context.inLong = False

            (y_target_pct, x_target_pct) = computeHoldingsPct(y_target_shares, X_target_shares, Y[-1], X[-1])
            order_target_percent(context.Y, y_target_pct)
            order_target_percent(context.X, x_target_pct)
            record(Y_pct=y_target_pct, X_pct=x_target_pct)


def is_market_close(dt):
    ref = tradingcalendar.canonicalize_datetime(dt)
    return dt == tradingcalendar.open_and_closes.T[ref]['market_close']


def hedge_ratio(Y, X, add_const=True):
    if add_const:
        X = sm.add_constant(X)
        model = sm.OLS(Y, X).fit()
        return model.params[1]
    model = sm.OLS(Y, X).fit()
    return model.params.values


def computeHoldingsPct(yShares, xShares, yPrice, xPrice):
    yDol = yShares * yPrice
    xDol = xShares * xPrice
    notionalDol = abs(yDol) + abs(xDol)
    y_target_pct = yDol / notionalDol
    x_target_pct = xDol / notionalDol
    return (y_target_pct, x_target_pct)
