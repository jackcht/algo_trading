''' Learning code from the tutorial sessions of Quantopian '''


# Put any initialization logic here.  The context object will be passed to
# the other methods in your algorithm.
def initialize(context):
    
    #context.entry_bound_level1 = [-1.8, 1.8]
    #context.exit_bound = [-1.0, 1.0]
    #context.quantile_bound = [0.1, 0.9]
    #context.quantile_exit_bound = [0.4, 0.6]
    #context.rolling_minutes = 100
    #context.limit_stop_margin = 1.0
    # AAPL
    context.stock = sid(24)
    # SPY
    context.market = sid(8554)
    
    # schedule the function 'check_mean' to be run every trading day, 30 mins before the market close
    schedule_function(check_mean, date_rules.every_day(), time_rules.market_close(minutes=30))
    
    pass


# this function is run once per day before any calls to handle_data, OPTIONAL
def before_trading_start (context, data):
    # do some precalculation if needed
    # fundamentals: /help/fundamentals
    context.fundamental_df = get_fundamentals(
        query(
            # put your query in here by typing "fundamentals."
            fundamentals.income_statement.total_revenue
        )
        .filter(
            # put your filters here
            fundamentals.valuation.market_cap > 30000000000
        )
        .order_by(
            # sort your query
            fundamentals.valuation.market_cap.desc()
        )
        .limit(10)        # limiting the result for only 10 securities
    )
    
    update_universe(context.fundamental_df.columns.values)

# Will be called on every trade event for the securities you specify. 
def handle_data(context, data):
    # data[sid(X)] holds the trade event data for that security.
    # data is a dictionary keyed by the securities
    # context.portfolio holds the current portfolio state.

    # Place orders with the order(SID, amount) method.

    # TODO: implement your own logic here.
    
	#print context.message
	
    # sid(): means the security id (secuirty id never changes)
    # whereas symbol('xxx') is not robust
    #order(sid(24), 50)
    
    open_orders = get_open_orders()
    # to order the trade if the previous orders are filled already 
    if context.stock not in open_orders:
        order_target_percent(context.stock, 1)
    
    # the final cash flow may still become lower than zero due to Slippage, although we only order 100% of the portfolio available
    record(cash = context.portfolio.cash)
 
def check_mean(context, data):
    print get_datetime('US/Eastern')
    # history(bar_count, frequency, field, ffill=True)
    # bar_count: The int number of bars returned by the history call. This includes the current bar.
    # frequency: The size of the bars returned by history. Available frequencies are '1d' and '1m'.
    # field: The data field selected from history. Currently, only the OHLCV data is supported. Options are: 'open_price', 'close_price', 'price', 'high', 'low', 'volume'.
    hist = history(10, '1m', 'close_price')
    for stock in data
        print '%s - mean price: %.2f ; total revenue: %.2f' % (stock.symbol, np.mean(hist[stock]), context.fundamental_df[stock][0])    
    
def log_price(context, data):
    log.info(get_datetime())
    for stock in data: 
        log.info('Close price for %s: %.2f' % (stock.symbol, data[stock].close_price))
    log.info('\n')
    
    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
