import yfinance as yf
import pandas as pd

goog = yf.Ticker('goog')
data = goog.history()
data.head()