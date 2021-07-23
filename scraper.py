import snscrape.modules.twitter as sntwitter
import pandas as pd

# Setting variables to be used below
maxTweets = 10000

# Creating list to append tweet data to
tweets_list = []

# Create the stock list that we will iterate through
stocklist = ['$NVDA', '$AHT', '$AAPL', '$MSFT', '$AMZN', 
            '$BNGO', '$CCIV', '$GME', '$JNJ', '$BNTX', 
            '$NEGG', '$OCGN', '$UONE', '$AAPL', '$BYND', 
            '$MRNA', '$PFE', '$TSLA', '$ORCL', '$PLTR', 
            '$FB', '$SPY', '$BABA', '$NIO', '$PZZA', 
            '$ABNB', '$VIAC', '$AMD', '$TWTR', '$NKE', 
            '$SQ', '$DIS', '$TSM', '$NOK', '$SPCE', 
            '$F', '$WISH', '$XPEV', '$ETSY', '$DKNG', 
            '$RKT', '$BB', '$XOM', '$GE', '$ADMP', 
            '$AEHR', '$CREX', '$CYRN', '$LPTH', '$WTER']


# Specify the timeframe to check tweets and the language
timeframe = ' since:2021-06-01 until:2021-07-01'
language = ' lang:en '

# Use sntwitter to pull the information needed while iterating through the stocklist above
for stock in stocklist:
    # For each tweet on the specific stock, get the items
    for i,tweet in enumerate(sntwitter.TwitterSearchScraper(stock + language + timeframe).get_items()):
        # Stop if we have reached 10,0000 tweets
        if i>maxTweets:
            break
        # Append all the information below into the list as one item
        tweets_list.append([tweet.date, tweet.id, stock.lstrip('$'), tweet.user.url, tweet.user.username, tweet.user.followersCount, tweet.user.friendsCount, tweet.content, tweet.retweetCount, tweet.likeCount, tweet.replyCount, tweet.lang])

# Convert the list into a dataframe
tweets_df = pd.DataFrame(tweets_list, columns= ['Datetime', 'Tweet_Id', 'Stock', 'Profile', 'Username', 'Followers', 'Friends', 'Text', 'Retweet_Count', 'Like_Count', 'Reply_Count', 'Language'])

# Export the dataframe into a csv
tweets_df.to_csv('tweets.csv', sep=',', index=False)