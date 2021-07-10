
"""   WARNING: The script snscrape is installed in '/home/jin/.local/bin' which is not on PATH.
  Consider adding this directory to PATH or, if you prefer to suppress this warning, use --no-warn-script-location. """
import snscrape.modules.twitter as sntwitter
import pandas as pd

""" import snscrape.modules.twitter as sntwitter
import pandas as pd

# Creating list to append tweet data to
tweets_list1 = []

# Using TwitterSearchScraper to scrape data and append tweets to list
for i,tweet in enumerate(sntwitter.TwitterSearchScraper('from:jack').get_items()):
    if i>100:
        break
    tweets_list1.append([tweet.date, tweet.id, tweet.content, tweet.user.username])
    
# Creating a dataframe from the tweets list above 
tweets_df1 = pd.DataFrame(tweets_list1, columns=['Datetime', 'Tweet Id', 'Text', 'Username']) """


""" import os

# Using OS library to call CLI commands in Python
os.system("snscrape --jsonl --max-results 500 --since 2020-06-01 twitter-search \"its the elephant until:2020-07-31\" > text-query-tweets.json") """


# Setting variables to be used below
maxTweets = 10000

# Creating list to append tweet data to
tweets_list2 = []
stocklist = ['$NVDA', '$AHT', '$ALPP', '$MSFT', '$AMZN', 
            '$BNGO', '$CCIV', '$GME', '$JNJ', '$MJWL', 
            '$NEGG', '$OCGN', '$UONE', '$AAPL', '$BYND', 
            '$MRNA', '$PFE', '$TSLA', '$ORCL', '$PLTR', 
            '$FB', '$SPY', '$BABA', '$NIO', '$PZZA', 
            '$ABNB', '$VIAC', '$AMD', '$TWTR', '$NKE', 
            '$SQ', '$DIS', '$TSM', '$NOK', '$SPCE', 
            '$F', '$WISH', '$XPEV', '$ETSY', '$DKNG', 
            '$RKT', '$BB', '$XOM', '$GE', '$ADMP', 
            '$AEHR', '$CREX', '$CYRN', '$LPTH', '$WTER']

# Using TwitterSearchScraper to scrape data and append tweets to list
timeframe = ' since:2021-06-01 until:2021-07-01'
language = ' lang:en '
for stock in stocklist:
    for i,tweet in enumerate(sntwitter.TwitterSearchScraper(stock + language + timeframe).get_items()):
        if i>maxTweets:
            break
        tweets_list2.append([tweet.date, tweet.id, stock.lstrip('$'), tweet.user.url, tweet.user.username, tweet.user.followersCount, tweet.user.friendsCount, tweet.content, tweet.retweetCount, tweet.likeCount, tweet.replyCount, tweet.lang])

# Creating a dataframe from the tweets list above
tweets_df2 = pd.DataFrame(tweets_list2, columns=['Datetime', 'Tweet_Id', 'Stock', 'Profile', 'Username', 'Followers', 'Friends', 'Text', 'Retweet_Count', 'Like_Count', 'Reply_Count', 'Language'])

# Display first 5 entries from dataframe
tweets_df2.head()

tweets_df2.to_csv('Test1.csv', sep=',', index=False)