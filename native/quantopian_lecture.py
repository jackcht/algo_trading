#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Pair trading algo based on the approach from Quantopian lecture
    copied from https://www.quantopian.com/posts/how-to-build-a-pairs-trading-strategy-on-quantopian
"""

import numpy as np


def initialize(context):
    context.stock1 = symbol('ABGB')
    context.stock2 = symbol('FSLR')

    context.threshold = 1
    context.in_high = False
    context.in_low = False


def handle_data(context, data):
    s1 = context.stock1
    s2 = context.stock2

    p60 = history(bar_count=60, frequency='1d', field='price')

    p5 = p60.iloc[-5:]

    # Get the 60 day mavg
    m60 = np.mean(p60[s1] - p60[s2])
    # Get the std of the last 60 days
    std60 = np.std(p60[s1] - p60[s2])

    # Current diff = 5 day mavg
    m5 = np.mean(p5[s1] - p5[s2])

    # Compute z-score
    if std60 > 0:
        zscore = (m5 - m60) / std60
    else:
        zscore = 0

    if zscore > context.threshold and not context.in_high:
        order_target_percent(s1, -0.5)  # short top
        order_target_percent(s2, 0.5)  # long bottom
        context.in_high = True
        context.in_low = False
    elif zscore < -context.threshold and not context.in_low:
        order_target_percent(s1, 0.5)  # long top
        order_target_percent(s2, -0.5)  # short bottom
        context.in_high = False
        context.in_low = True
    elif abs(zscore) < 1:
        order_target_percent(s1, 0)
        order_target_percent(s2, 0)
        context.in_high = False
        context.in_low = False

    record('zscore', zscore)
