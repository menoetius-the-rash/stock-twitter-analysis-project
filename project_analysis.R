library(dotenv)
library(dplyr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(tm)

# load configuration
load_dot_env("config.env")

# get location from config file
location = Sys.getenv("location")

# set location
setwd(location)

#### Sentimental Analysis ####
tweets_location = paste0(location, "/tweets")

# set location
setwd(tweets_location)

# read file to be analyse for setiment
tweets = read.csv("tweets_separated.csv", colClasses = c("character",  "character", "character", "character", 
                                                     "numeric", "numeric", "character", 
                                                     "numeric", "numeric", "numeric"))

# get a list of the stocks to go through
list_of_stocks = unique(tweets$Stock)

# create an empty data frame to store the values of stock name and number of tweets
tweets_per_stock = data.frame(stock = character(),
                             tweets = numeric())
nrow(subset(data, x=="0"))

temp_df = data.frame(list_of_stocks[1], nrow(subset(tweets, Stock==list_of_stocks[1])))
temp_df
names(temp_df) = c("stocks", "tweets")
temp_df

# count the tweets and store to the empty table
for (stock in 1:length(list_of_stocks)) {
  temp_df = data.frame(list_of_stocks[stock], nrow(subset(tweets, Stock==list_of_stocks[stock])))
  names(temp_df) = c("stocks", "tweets")
  tweets_per_stock = rbind(tweets_per_stock, temp_df)
}

# find out how many tweets are per stock
head(tweets_per_stock)
tweets_per_stock <- tweets_per_stock[order(tweets_per_stock$stock),]

list_of_stocks = tweets_per_stock$stocks

# separate each tweets for their correlated stock and save in STOCK_tweets format e.g. AAPL_tweets
for (stock in 1:length(list_of_stocks)) {
  assign(paste(list_of_stocks[stock], "_tweets", sep=""),  subset(tweets, Stock == list_of_stocks[stock]))
}

# create corpus of tweets
tweet_corpus = VCorpus(VectorSource(tweets$Text))
inspect(tweet_corpus[1:5])
paste(tweet_clean_corpus[4])
# clean tweet_corpus
# change to lowercase
tweet_clean_corpus <- tm_map(tweet_corpus, tolower)

# remove numbers
tweet_clean_corpus <- tm_map(tweet_clean_corpus, removeNumbers)

# remove whitespace
tweet_clean_corpus <- tm_map(tweet_clean_corpus, stripWhitespace)

# remove stop words
tweet_clean_corpus <- tm_map(tweet_clean_corpus, removeWords, stopwords())

# remove punctuation
tweet_clean_corpus <- tm_map(tweet_clean_corpus, removePunctuation)

tweet_clean_corpus_dtm <- DocumentTermMatrix(tweet_clean_corpus)


n <- nrow(data.frame)
raw.text.train <- data.frame[1:round(.8 * n),]
raw.text.test  <- data.frame[(round(.8 * n)+1):n,]



#### Time Series Analysis ####

#### TF-IDF ####