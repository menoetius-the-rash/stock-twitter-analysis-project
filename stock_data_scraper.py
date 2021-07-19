import requests
import pandas as pd
from datetime import date, timedelta

'''
params = {
  'access_key': '087159d85393bf79ec34aea670edf7fe'
}

api_result = requests.get('https://api.marketstack.com/v1/tickers/', stock, '/intraday/' , params)

api_response = api_result.json()

df = pd.json_normalize()
'''
stock = 'GME'
access_key = '?access_key=087159d85393bf79ec34aea670edf7fe'


df = pd.DataFrame()

start_date = date(2021, 6, 1)
end_date = date(2021, 6, 31)
delta = timedelta(days=1)

while start_date <= end_date:
    date_set = start_date.strftime("%Y-%m-%d")
    test_result = requests.get('https://api.marketstack.com/v1/tickers/', stock, '/intraday/', date_set, access_key)
    clean_data = pd.json_normalize(test_result, record_path=['data', 'intraday'], meta=[['data', 'name'], ['data', 'symbol']])
    df.append(clean_data)
    start_date += delta

path=r'C:\Users\hvill\Destop\'