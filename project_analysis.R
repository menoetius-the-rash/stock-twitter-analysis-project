library(dotenv)
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(doParallel)
library(tm)
library(wordcloud)
library(SnowballC)
library(qdap)
library(data.table)
library(SentimentAnalysis)

# used for positive and negative words https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html

install.packages("qdap")

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

# count the tweets and store to the empty table
for (stock in 1:length(list_of_stocks)) {
  temp_df = data.frame(list_of_stocks[stock], nrow(subset(tweets, Stock==list_of_stocks[stock])))
  names(temp_df) = c("stocks", "tweets")
  tweets_per_stock = rbind(tweets_per_stock, temp_df)
}

# find out how many tweets are per stock
head(tweets_per_stock)

# order stock alphabetically
tweets_per_stock <- tweets_per_stock[order(tweets_per_stock$stock),]

list_of_stocks = tweets_per_stock$stocks

# separate each tweets for their correlated stock and save in STOCK_tweets format e.g. AAPL_tweets
for (stock in 1:length(list_of_stocks)) {
  assign(paste(list_of_stocks[stock], "_tweets", sep=""),  subset(tweets, Stock == list_of_stocks[stock]))
  tweet_lists = c(tweet_lists, paste(list_of_stocks[stock], "_tweets", sep=""))
}

tweet_lists

head(AAPL_tweets)

###############################################################
# leave for now
for (t in 1:length(tweet_lists)) {
  assign(paste("tweets_", tweet_lists[t], sep=""),  tweet_lists[t] %>% select(Username, Text))
  corpus_stock_tweets = paste("tweets_", tweet_lists[t], sep="")
}
######################


# tweets strip of unnecessary stuff
tweets_cleaned = tweets

###############################################################

positive_words = scan('positive-words.txt',what = 'character')
negative_words = scan('negative-words.txt',what = 'character')
# tweets strip of unnecessary stuff
tweets_cleaned = tweets
raw_tweets = tweets

# removing unnecessary text using gsub prior to creating corpus
tweets_cleaned$Text = gsub("http\\S+","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("&\\S+","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\d+","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("@#","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\s*\\$\\w*","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\s*\\#\\w*","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\s*\\@\\w*","",tweets_cleaned$Text)
tweets_cleaned$Text = iconv(tweets_cleaned$Text, "latin1", "ASCII", sub="") #emojis
tweets_cleaned$Text = gsub("^[[:space:]]*","",tweets_cleaned$Text) # Remove leading whitespaces
tweets_cleaned$Text = gsub("[[:space:]]*$","",tweets_cleaned$Text) # Remove trailing whitespaces
tweets_cleaned$Text = gsub(" +"," ", tweets_cleaned$Text) # remove extra whitespaces
tweets_cleaned$Text =  gsub("  ", " ", tweets_cleaned$Text) #Replace double space with single space

tweets_cleaned$Text[500:505]
##########################Test 1 Naive Bayes#####################################
# create corpus of tweets
tweet_corpus = Corpus(VectorSource(tweets_cleaned$Text))

length(tweet_corpus)
#[1] 327689

inspect(tweet_corpus[1:5])
paste(tweet_corpus[4])

# clean tweet_corpus
# change to lowercase
tweet_clean_corpus <- tm_map(tweet_corpus, tolower)
length(tweet_clean_corpus)
#[1] 327689

inspect(tweet_clean_corpus[1:5])

# remove numbers
tweet_clean_corpus = tm_map(tweet_clean_corpus, removeNumbers)

# remove whitespace
tweet_clean_corpus = tm_map(tweet_clean_corpus, stripWhitespace)

# remove stop words
my_stopwords = c("day", "will","week","time","today","stock","trade","call","go","look","get")
# "","","","","","",""
tweet_clean_corpus = tm_map(tweet_clean_corpus, removeWords, c(stopwords("english"), my_stopwords))

# remove punctuation
tweet_clean_corpus = tm_map(tweet_clean_corpus, removePunctuation)

# use snowballc to get root words using text stemming e.g. root of "stocks" is "stock"
tweet_clean_corpus = tm_map(tweet_clean_corpus, stemDocument)

length(tweet_clean_corpus)
#[1] 327689
inspect(tweet_clean_corpus[1:5])

# convert corpus back to a data frame
tweets_cleaned <- data.frame(text = sapply(tweet_clean_corpus, paste, collapse = " "), stringsAsFactors = FALSE)
head(tweets_cleaned)

# create a new column of sentiment score, filled with 0s for now
tweets_cleaned$sentiment_score = c(0)

tweets_to_analyse = tweets_cleaned$text
length(tweets_to_analyse)
# sentiment analysis using SentimentAnalysis package
limit = 27689
tweets_sentiment1 = analyzeSentiment(tweets_to_analyse[1:limit])
limit2 = limit + 50000
tweets_sentiment2 = analyzeSentiment(tweets_to_analyse[(limit+1):limit2])
limit3 = limit2 + 50000 
tweets_sentiment3 = analyzeSentiment(tweets_to_analyse[(limit2+1):limit3])
limit4 = limit3 + 50000 
tweets_sentiment4 = analyzeSentiment(tweets_to_analyse[(limit3+1):limit4])
limit5 = limit4 + 50000
tweets_sentiment5 = analyzeSentiment(tweets_to_analyse[(limit4+1):limit5])
limit6 = limit5 + 50000
tweets_sentiment6 = analyzeSentiment(tweets_to_analyse[(limit5+1):limit6])
limit7 = limit6 + 50000
tweets_sentiment7 = analyzeSentiment(tweets_to_analyse[(limit6+1):limit7])

totalrows = nrow(tweets_sentiment) + nrow(tweets_sentiment2) + nrow(tweets_sentiment3) + nrow(tweets_sentiment4)+
  nrow(tweets_sentiment5) + nrow(tweets_sentiment6) + nrow(tweets_sentiment7)

tweets_sentiment_score = c(tweets_sentiment1$SentimentQDAP, tweets_sentiment2$SentimentQDAP, 
                               tweets_sentiment3$SentimentQDAP, tweets_sentiment4$SentimentQDAP,
                               tweets_sentiment5$SentimentQDAP, tweets_sentiment6$SentimentQDAP,
                               tweets_sentiment7$SentimentQDAP)

tweet_sentiment_value = convertToDirection(tweets_sentiment_score)

length(tweets_sentiment_score)
length(tweet_sentiment_value)

tweets_cleaned = cbind(tweets_cleaned, tweets_sentiment_score, tweet_sentiment_value)

head(tweets_cleaned)
head(tweets_sentiment1$SentimentQDAP)
tweets_sentiment1$SentimentQDAP
ncol(tweets_sentiment_score)

head(tweets)
nrow(tweets)

tweets_with_sentiment_score = cbind(tweets, tweets_cleaned$tweets_sentiment_score, tweets_cleaned$tweet_sentiment_value)

head(tweets_with_sentiment_score)

if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.size(max = FALSE)
memory.limit(size=150000)
# Use multicore to speed up process
c = detectCores()
# avoid overload buy only using maximum - 1 cores available
cores_to_use <- makeCluster(c[1]-1)
# register cores
registerDoParallel(cores_to_use)
# Time the running of the function
start <- proc.time()

# the for loop uses multicore and uses the package stringr to remove the "+00:00" pattern on each item
result = foreach(i=1:length(head(tweets_cleaned$text)), .packages = "match") %dopar% {
  tmp_pos = 0
  tmp_neg = 0
  
  sentence = unlist(str_split(test_txt, pattern = " "))
  for (i in 1:length(sentence)) {
    if(is.na(match(sentence[i], positive_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
    if(is.na(match(sentence[i], negative_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
  }
  score = tmp_pos- tmp_neg
}

tmp_pos = 0
tmp_neg = 0
test_txt = "this is great and awesome I love it"
test_txt2 = "i hate this so much i dislike it"
sentence = unlist(str_split(test_txt, pattern = " "))
sentence[3]
class(sentence)

match(sentence, positive.words)
is.na(match(sentence[8], positive_words))
match(sentence[7], positive_words)
is.na(match(sentence[3], negative_words))

for (i in 1:length(sentence)) {
  if(is.na(match(sentence[i], positive_words)) == FALSE)
    tmp_pos = tmp_pos + match(sentence[i], positive_words)
  if(is.na(match(sentence[i], negative_words)) == FALSE)
    tmp_neg = tmp_neg + match(sentence[i], negative_words)
}
score = tmp_pos- tmp_neg

score
#stop cluster
stopCluster(cores_to_use)

# Subtract time to find out how long it took to run
time_it_took <- proc.time()-start
time_it_took


test_txt = "this is great and awesome I love it"
test = str_split(test_txt, pattern = " ")
oneword= "amazing"
nega = "great"
str_split(test_txt, pattern = " ")
class(test)

chmatch(test, positive_words)
match(oneword, positive_words)
match(nega, negative_words)
is.na(match(oneword, positive_words))

test = 10


test = test + match(nega, negative_words)
test

# create term document matrix
tweet_clean_corpus_dtm <- DocumentTermMatrix(tweet_clean_corpus)

head(tweet_clean_corpus_dtm)

# get top 10 most frequent words
term_count <- freq_terms(tweet_clean_corpus, 10)
# plot to see them
plot(term_count)

# get lowest frequency words that appear
low_freq_20 <- findFreqTerms(tweet_clean_corpus_dtm, lowfreq = 20)

# remove sparse terms that we do not need:
sparse_tweet_clean_corpus_dtm <- removeSparseTerms(tweet_clean_corpus_dtm, 0.995)
sparse_tweet_clean_corpus_dtm

# transform it back to a data frame
sparse_tweets <- as.data.frame(as.matrix(sparse_tweet_clean_corpus_dtm))

colnames(sparse_tweets) <- make.names(colnames(sparse_tweets))



#### training and test datasets ####

n <- nrow(raw_tweets)
raw_tweets_train <- raw_tweets[1:round(.8 * n),]
raw_tweets_test  <- raw_tweets[(round(.8 * n)+1):n,]


nn <- length(tweet_clean_corpus)
tweet_clean_corpus_train <- tweet_clean_corpus[1:round(.8 * nn)]
tweet_clean_corpus_test  <- tweet_clean_corpus[(round(.8 * nn)+1):nn]


nnn <- nrow(tweet_clean_corpus_dtm)
tweet_clean_corpus_dtm_train <- tweet_clean_corpus_dtm[1:round(.8 * nnn),]
tweet_clean_corpus_dtm_test  <- tweet_clean_corpus_dtm[(round(.8 * nnn)+1):nnn,]


# create a word cloud
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(tweet_clean_corpus_train, min.freq = 2000, random.order = FALSE, scale = c(3, 0.5))
warnings()


# sentiments
positive = subset(raw_tweets_train, sentiment == "Positive")

###############################################################





#### Time Series Analysis ####

#### TF-IDF ####