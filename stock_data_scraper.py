from time import sleep
import requests
import config
import csv
import os
import pandas as pd
from datetime import date, timedelta

stocklist = ['NVDA', 'AHT', 'AAPL', 'MSFT', 'AMZN', 
            'BNGO', 'CCIV', 'GME', 'JNJ', 'BNTX', 
            'NEGG', 'OCGN', 'UONE', 'AAPL', 'BYND', 
            'MRNA', 'PFE', 'TSLA', 'ORCL', 'PLTR', 
            'FB', 'SPY', 'BABA', 'NIO', 'PZZA', 
            'ABNB', 'VIAC', 'AMD', 'TWTR', 'NKE', 
            'SQ', 'DIS', 'TSM', 'NOK', 'SPCE', 
            'F', 'WISH', 'XPEV', 'ETSY', 'DKNG', 
            'RKT', 'BB', 'XOM', 'GE', 'ADMP', 
            'AEHR', 'CREX', 'CYRN', 'LPTH', 'WTER']

stocklist2 = ['AAPL', 'BNTX']
location = config.location
counter = 1

for stock in stocklist:
  test1 = 'https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY_EXTENDED&symbol=' + stock +'&interval=1min&slice=year1month1&apikey=' + config.api_key
  test2 = 'https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY_EXTENDED&symbol='+ stock +'&interval=1min&slice=year1month2&apikey=' + config.api_key
  CSV_URL1 = test1
  CSV_URL2 = test2
  df1 = pd.read_csv(CSV_URL1)
  df2 = pd.read_csv(CSV_URL2)
  # Creating a dataframe from the above CSV
  stockmonth_df1 = pd.DataFrame(df1, columns=['time', 'open', 'high', 'low', 'close', 'volume'])
  stockmonth_df2 = pd.DataFrame(df2, columns=['time', 'open', 'high', 'low', 'close', 'volume'])
  df1.append(df2)
  stockFileName = stock + '.csv'
  df1.to_csv(os.path.join(location,stockFileName), index=False)
  if counter % 2 == 0:
    sleep(60)
  counter += 1
