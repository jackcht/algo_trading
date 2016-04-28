"""
Pairs Trading with Kalman Filters for Quantopian

modified from 
"""

import numpy as np
import pandas as pd
from pykalman import KalmanFilter


# Put any initialization logic here.  The context object will be passed to
# the other methods in your algorithm.
def initialize(context):
    # Quantopian backtester specific variables
    ''' Slippage (using the default function)
        volume_limit: (default 0.25), which limits the proportion of volume that your order can take up per bar (in our case would be a minute)
        price_impact: (default 0.1), the slippage is calculated by multiplying the price impact constant by the square of the ratio of the order to the total volume. 
        Eg: 0.1 * (25/100) * (25/100) = 0.625%
    '''
    set_slippage(slippage.VolumeShareSlippage(volume_limit=0.025, price_impact=0.1))
    ''' commission for Interactive Brokers (which is used by Quantopian) is 0.005 pershare, 1 minimum per order 
        if no specify a commission, the backtest defaults to $0.03 per share (according to a thread) in Quantopian
    '''
    set_commission(commission.PerShare(cost=0.005, min_trade_cost=1.0))
    ''' symbol_lookup  is used for securities whose symbols have changed '''
    #set_symbol_lookup_date('2014-07-20')

    context.X = KalmanMovingAverage(symbol('HON'))
    context.Y = KalmanMovingAverage(symbol('HD'))
    context.kf = None

    # regular market time 9:30 - 16:00, 390 minutes
    # we can make it trade in customized time slot
    for minute in range(10, 390, 30):
        schedule_function(trade,
                          time_rule=time_rules.market_open(minutes=minute))


def trade(context, data):
    if context.kf is None:
        initialize_filters(context, data)
        return
    if get_open_orders():
        return

    prices = np.log(history(bar_count=1, frequency='1d', field='price'))
    context.X.update(prices)
    context.Y.update(prices)

    mu_Y = context.Y.state_means
    mu_X = context.X.state_means

    frame = pd.DataFrame([mu_Y, mu_X]).T
    # remove the initial window (1)
    context.kf.update(frame.iloc[-1])
    # get beta and alpha from the Kalman calculation
    beta, alpha = context.kf.state_mean
    # calculate the spread
    spreads = (mu_Y - (beta * mu_X + alpha)).tail(500)  #return last 500 rows
    # zscore
    zscore = (spreads[-1] - spreads.mean()) / spreads.std()

    # reference postion of y (>0 means long position)
    reference_pos = context.portfolio.positions[context.Y.asset].amount
    reference_pos_x = context.portfolio.positions[context.X.asset].amount

    record(
        beta=beta,
        alpha=alpha,
        mean_spread=spreads[-1],
        zscore=zscore
    )

    # simple implementation of stop loss
    if reference_pos_x > 0 and zscore > 2.56:
        order_target(context.Y.asset, 0)
        order_target(context.X.asset, 0)

    if reference_pos > 0 and zscore < -2.56:
        order_target(context.Y.asset, 0.0)
        order_target(context.X.asset, 0.0)

    if reference_pos:
        # Do a PNL check to make sure a reversion at least covered trading cost (by David Edwards)
        # Due to parameter drift, trades often made to be exited before the original spread has become profitable. 
        pnl = get_pnl(context, data)
        if zscore > -0.75 and reference_pos > 0 and pnl > 10:
            order_target(context.Y.asset, 0.0)
            order_target(context.X.asset, 0.0)

        elif zscore < 0.75 and reference_pos < 0 and pnl > 10:
            order_target(context.Y.asset, 0.0)
            order_target(context.X.asset, 0.0)

    else:
        # this zscore is an empirical value
        if zscore > 2.0:
            # Place an order to adjust a position to a target percent of the current portfolio value      
            # Placing a negative target percent order will result in a short position equal to the negative target percent. 
            # Portfolio value is calculated as the sum of the positions value and ending cash balance. 
            order_target_percent(context.Y.asset, -0.5)
            order_target_percent(context.X.asset, 0.5)
        if zscore < -2.0:
            order_target_percent(context.Y.asset, 0.5)
            order_target_percent(context.X.asset, -0.5)


def initialize_filters(context, data):
    # initial calculation window for mean 
    initial_bars = 10
    prices = np.log(history(initial_bars, '1d', 'price'))
    context.X.update(prices)
    context.Y.update(prices)

    # Drops the initial 0 mean value from the kalman filter
    context.X.state_means = context.X.state_means.iloc[-initial_bars:]
    context.Y.state_means = context.Y.state_means.iloc[-initial_bars:]

    context.kf = KalmanRegression(context.Y.state_means, context.X.state_means)


def get_pnl(context, data):
    x = context.X.asset
    y = context.Y.asset
    positions = context.portfolio.positions
    dx = data[x].price - positions[x].cost_basis
    dy = data[y].price - positions[y].cost_basis
    return (positions[x].amount * dx + positions[y].amount * dy)


def handle_data(context, data):
    record(market_exposure=context.account.net_leverage)


def set_trailing_stop(context, data):
    if context.portfolio.positions[context.X].amount:
        price = data[context.X].price
        context.stop_price_x = max(context.stop_price_x, context.stop_pct * price)
    if context.portfolio.positions[context.Y].amount:
        price = data[context.Y].price
        context.stop_price_y = max(context.stop_price_y, context.stop_pct * price)


class KalmanMovingAverage(object):
    """
    Estimates the moving average of a price process via Kalman Filtering, using pykalman
    """

    def __init__(self, asset, observation_covariance=1.0, initial_value=0,
                 initial_state_covariance=1.0, transition_covariance=0.05,
                 initial_window=30, maxlen=300, freq='1d'):
        self.asset = asset
        self.freq = freq
        self.initial_window = initial_window

        self.kf = KalmanFilter(transition_matrices=[1],
                               observation_matrices=[1],
                               initial_state_mean=initial_value,
                               initial_state_covariance=initial_state_covariance,
                               observation_covariance=observation_covariance,
                               transition_covariance=transition_covariance)
        self.state_means = pd.Series([self.kf.initial_state_mean], name=self.asset)
        self.state_covs = pd.Series([self.kf.initial_state_covariance], name=self.asset)

    def update(self, observations):
        for dt, observation in observations[self.asset].iterkv():
            self._update(dt, observation)

    def _update(self, dt, observation):
        mu, cov = self.kf.filter_update(self.state_means.iloc[-1],
                                        self.state_covs.iloc[-1],
                                        observation)
        self.state_means[dt] = mu.flatten()[0]
        self.state_covs[dt] = cov.flatten()[0]


class KalmanRegression(object):
    """
    Uses a Kalman Filter to estimate regression parameters in an online fashion.
    Estimated model: y ~ beta * x + alpha
    """

    def __init__(self, initial_y, initial_x, delta=1e-5):
        self._x = initial_x.name
        self._y = initial_y.name
        trans_cov = delta / (1 - delta) * np.eye(2)
        obs_mat = np.expand_dims(
            np.vstack([[initial_x], [np.ones(initial_x.shape[0])]]).T, axis=1)

        self.kf = KalmanFilter(n_dim_obs=1, n_dim_state=2,
                               initial_state_mean=np.zeros(2),
                               initial_state_covariance=np.ones((2, 2)),
                               transition_matrices=np.eye(2),
                               observation_matrices=obs_mat,
                               observation_covariance=1.0,
                               transition_covariance=trans_cov)
        state_means, state_covs = self.kf.filter(initial_y.values)
        self.means = pd.DataFrame(state_means,
                                  index=initial_y.index,
                                  columns=['beta', 'alpha'])
        self.state_cov = state_covs[-1]

    def update(self, observations):
        x = observations[self._x]
        y = observations[self._y]
        mu, self.state_cov = self.kf.filter_update(self.state_mean, self.state_cov, y,
                                                   observation_matrix=np.array([[x, 1.0]]))
        mu = pd.Series(mu, index=['beta', 'alpha'],
                       name=observations.name)
        self.means = self.means.append(mu)

    def get_spread(self, observations):
        x = observations[self._x]
        y = observations[self._y]
        return y - (self.means.beta * x + self.means.alpha)

    @property
    def state_mean(self):
        return self.means.iloc[-1]
