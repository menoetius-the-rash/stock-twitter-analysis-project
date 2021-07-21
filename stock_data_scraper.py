import requests
import os
import pandas as pd
from datetime import date, timedelta

'''
params = {
  'access_key': '087159d85393bf79ec34aea670edf7fe'
}
Your dedicated access key is: 77HAARG0F1BQY2GS
api_result = requests.get('https://api.marketstack.com/v1/tickers/', stock, '/intraday/' , params)

api_response = api_result.json()

df = pd.json_normalize()
'''
stock = 'GME'
access_key = '?access_key=087159d85393bf79ec34aea670edf7fe'


df = pd.DataFrame()

interval = '1min'

start_date = date(2021, 6, 1)

end_date = date(2021, 6, 30)

delta = timedelta(days=1)

while start_date <= end_date:
    date_set = start_date.strftime("%Y-%m-%d")
    url = 'http://api.marketstack.com/v1/intraday/' + access_key + '/' + stock  + date_set + '/intraday' +  + '&interval=' +  interval + 
    print(url)
    test_result = requests.get(url)
    clean_data =  pd.json_normalize(test_result, record_path=['data', 'intraday'], meta=[['data', 'name'], ['data', 'symbol']])
    df.append(clean_data)
    start_date += delta

location =r'C:\Users\Jin\Dropbox\2020 Course\Modules\Project'
df.to_csv(os.path.join(location,r'GME.csv'))


