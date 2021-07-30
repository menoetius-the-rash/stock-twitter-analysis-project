from time import sleep
import config
import os
import pandas as pd

# Create the  stocklist to iterate through
stocklist = ['NVDA', 'AHT', 'AAPL', 'MSFT', 'AMZN', 
            'BNGO', 'CCIV', 'GME', 'JNJ', 'BNTX', 
            'NEGG', 'OCGN', 'UONE', 'FSLY', 'BYND', 
            'MRNA', 'PFE', 'TSLA', 'ORCL', 'PLTR', 
            'FB', 'SPY', 'BABA', 'NIO', 'PZZA', 
            'ABNB', 'VIAC', 'AMD', 'TWTR', 'NKE', 
            'SQ', 'DIS', 'TSM', 'NOK', 'SPCE', 
            'F', 'WISH', 'XPEV', 'ETSY', 'DKNG', 
            'RKT', 'BB', 'XOM', 'GE', 'ADMP', 
            'AEHR', 'CREX', 'CYRN', 'LPTH', 'WTER']

# Create a location to save the file to later
location = config.location

# create a counter for the purpose of using for sleep.
counter = 1

# For loop to iterate through the stocklist above
for stock in stocklist:
  # Create the URL for each API call specifically for 2 months behind in order to fully collect June month tweets
  stock_url1 = 'https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY_EXTENDED&symbol='+ stock +'&interval=1min&slice=year1month1&apikey=' + config.api_key
  stock_url2 = 'https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY_EXTENDED&symbol='+ stock +'&interval=1min&slice=year1month2&apikey=' + config.api_key

  # Turn stock_urls above into dataframes
  df1 = pd.read_csv(stock_url1)
  df2 = pd.read_csv(stock_url2)
  
  # Append each 2nd dataframe to the first one
  df1 = df1.append(df2)

  # Create the stock filename to save the CSV file from stocklist
  stockFileName = stock + '.csv'
  
  # Export the dataframe into the location with the filename and index as falls
  df1.to_csv(os.path.join(location,stockFileName), index=False)

  # Create a sleep function in order put loop into sleep for 60 seconds. To prevent from being timed out by the API
  if counter % 2 == 0:
    sleep(60)
  counter += 1
