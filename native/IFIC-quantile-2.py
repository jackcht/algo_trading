#!/usr/bin/env python
# -*- coding: utf-8 -*-
""" Pair Trading Learning code: Credit to Vera """

#import numpy as np
#import statsmodels.tsa.stattools as sts
import matplotlib.pyplot as plt
import tushare as ts
import pandas as pd
from datetime import datetime
import statsmodels.tsa.stattools as sts
#from scipy.stats.stats import pearsonr

entry_bound_level1 = [-1.8, 1.8]
exit_bound = [-1.0, 1.0]
quantile_bound = [0.1, 0.9]
quantile_exit_bound = [0.4, 0.6]
rolling_minutes = 100
limit_stop_margin = 1.0

IC = pd.read_csv('IC.csv', index_col=0, parse_dates=True)
IF = pd.read_csv('IF.csv', index_col=0, parse_dates=True)
IF_close_BP = IF['BP.Close']
IF_close_SP = IF['SP.Close']
IC_close = IC['Close']
IF_close = (IF_close_BP + IF_close_SP) / 2

df_IC = pd.DataFrame(IC_close)
df_IF = pd.DataFrame(IF_close)
df_combine = df_IC.join(df_IF).dropna()

df_combine.columns = ['IC_close', 'IF_close']

model = pd.ols(y=df_combine['IF_close'], x=df_combine['IC_close'], intercept=True)
print 'beta: ',model.beta['x']                  # beta:  0.186247962277

beta_ary = []

print len(df_combine)                           # 5642
for iirr in range(len(df_combine)):
    beta_ary.append(model.beta['x'])

AA = df_combine['IF_close'][:]
BB = df_combine['IC_close'][:].values * pd.Series(beta_ary)

spread_value =  BB*0.66 -AA.values
spread = pd.DataFrame(spread_value.values, df_combine[:].index).dropna()
spread.columns = ['spread']
spread.plot()
plt.show()
#spread.to_csv('spread_this.csv')
#assert(0)

sta = sts.adfuller(spread['spread'], 1)
print 'sts.adfuller:', sta
# sts.adfuller: (-2.5699465276636864, 0.09936914370616251, 1L, 5640L, {'5%': -2.8620525978666596, '1%': -3.4315099784996939, '10%': -2.5670428542641215}, 33859.403784217917)

mean_sp = pd.rolling_mean(spread, rolling_minutes).dropna()
#mean_sp.to_csv('mean_this.csv')

std_sp = pd.rolling_std(spread, rolling_minutes).dropna()
#std_sp.to_csv('std_this.csv')

upper_sp = pd.rolling_quantile(spread, rolling_minutes, quantile_bound[1]).dropna().values
lower_sp = pd.rolling_quantile(spread, rolling_minutes, quantile_bound[0]).dropna().values
upper_exit_sp = pd.rolling_quantile(spread, rolling_minutes, quantile_exit_bound[1]).dropna().values
lower_exit_sp = pd.rolling_quantile(spread, rolling_minutes, quantile_exit_bound[0]).dropna().values

assert(len(mean_sp) == len(std_sp))
spread = spread[rolling_minutes-1:]
print len(spread)                       # 5543
print len(mean_sp)                      # 5543
assert(len(mean_sp) == len(spread))
assert(len(upper_sp) == len(spread))

time_index = spread.index

profit = []
sum_profit = 0.0
hold_flag = False
spread_last_entrance = 0.0

last_status = 0 # 1 for above upper bound, 2 for below upper bound; above upper exit bound, 3 for below upper exit bound and above lower bound, 4 for below lower exit bound and above lower bound, 5 for below lower bound

last_spread = 0.0

cnt = 0
iiii = 0
for index, row in spread.iterrows():
    if iiii == 0:
        row_val = row['spread']
        #print 'This is start ', row_val
        if row_val > upper_sp[iiii]:
            last_status = 1
        elif row_val > upper_exit_sp[iiii]:
            last_status = 2
        elif row_val > lower_exit_sp[iiii]:
            last_status = 3
        elif row_val> lower_sp[iiii]:
            last_status = 4
        else:
            last_status = 5
        iiii += 1
        profit.append(sum_profit)
        continue
    
    append_flag = False

    row_val = row['spread']
    #print row_val

    #open the position when get out of the bound
    if row_val > upper_sp[iiii]:
        if last_status == 2 and hold_flag == False:
            #open position
            hold_flag = True
            last_spread = row_val
        last_status = 1
    elif row_val > upper_exit_sp[iiii]:
        #doing nothing
        last_status = 2
    elif row_val > lower_exit_sp[iiii]:
        if last_status == 2 and hold_flag == True:
            #close the position
            sum_profit += (last_spread - row_val - 0.5)
            cnt += 1
            hold_flag = False
        if last_spread == 4 and hold_flag == True:
            #close the position
            sum_profit += (row_val - last_spread - 0.5)
            cnt += 1
            hold_flag = False
        last_status = 3
    elif row_val> lower_sp[iiii]:
        last_status = 4
    else:
        if last_status == 4 and hold_flag == False:
            hold_flag = True
            last_spread = row_val
        last_status = 5

#print sum_profit
    profit.append(sum_profit)
    iiii += 1

profit_df = pd.DataFrame(profit, spread.index)
profit_df.to_csv('profit_this_2.csv')
profit_df.plot()
plt.show()

print cnt                       # 72
