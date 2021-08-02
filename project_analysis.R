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

########################## Tweets strip of unnecessary stuff #####################################
tweets_cleaned = tweets

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
tweets_cleaned = data.frame(text = sapply(tweet_clean_corpus, paste, collapse = " "), stringsAsFactors = FALSE)
head(tweets_cleaned)

# use text column to as tweets to analyse
tweets_to_analyse = tweets_cleaned$text

# sentiment analysis using SentimentAnalysis package and return sentiment scores
# Break it down to smaller scale to prevent not having enough memory
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

# Checking if row is not enough
totalrows = nrow(tweets_sentiment) + nrow(tweets_sentiment2) + nrow(tweets_sentiment3) + nrow(tweets_sentiment4)+
  nrow(tweets_sentiment5) + nrow(tweets_sentiment6) + nrow(tweets_sentiment7)

# bind everything through caret for the sentiment score
tweets_sentiment_score = c(tweets_sentiment1$SentimentQDAP, tweets_sentiment2$SentimentQDAP, 
                               tweets_sentiment3$SentimentQDAP, tweets_sentiment4$SentimentQDAP,
                               tweets_sentiment5$SentimentQDAP, tweets_sentiment6$SentimentQDAP,
                               tweets_sentiment7$SentimentQDAP)

# get the sentiment value for each score
tweet_sentiment_value = convertToDirection(tweets_sentiment_score)

# bind this back to the tweets cleaned
tweets_cleaned = cbind(tweets_cleaned, tweets_sentiment_score, tweet_sentiment_value)
head(tweets_cleaned)

# save file
write.csv(tweets_cleaned, "tweets_sentiment_score.csv", row.names = FALSE)

tweets_sentiment_score = read.csv("tweets_sentiment_score.csv")

positive_words = scan('positive-words.txt',what = 'character')
negative_words = scan('negative-words.txt',what = 'character')

positive_words = c(positive_words, "buy", "rising", "rip")
negative_words = c(negative_words, "sell", "dip", "crash")

tweets_to_analyse2 = tweets_to_analyse2$text
# Use multicore to speed up process
c = detectCores()
# avoid overload buy only using maximum - 1 cores available
cores_to_use <- makeCluster(c[1]-1)
# register cores
registerDoParallel(cores_to_use)
# Time the running of the function
start <- proc.time()

# set score 
score = c()

# the for loop uses multicore and uses the package stringr to remove the "+00:00" pattern on each item
result1 = foreach(i=1:87689, .packages = c("data.table", "stringr")) %dopar% {
  tmp_pos = 0
  tmp_neg = 0
  
  sentence = unlist(str_split(tweets_to_analyse2[i], pattern = " "))
  for (i in 1:length(sentence)) {
    if(is.na(match(sentence[i], positive_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
    if(is.na(match(sentence[i], negative_words)) == FALSE)
      tmp_neg = tmp_neg + match(sentence[i], negative_words)
  }
  positive_matched_words = match(sentence, positive_words)
  negative_matched_words = match(sentence, negative_words)

  score = tmp_pos - tmp_neg
}

result2 = foreach(i=(87689+1):147690, .packages = c("data.table", "stringr")) %dopar% {
  tmp_pos = 0
  tmp_neg = 0
  
  sentence = unlist(str_split(tweets_to_analyse2[i], pattern = " "))
  for (i in 1:length(sentence)) {
    if(is.na(match(sentence[i], positive_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
    if(is.na(match(sentence[i], negative_words)) == FALSE)
      tmp_neg = tmp_neg + match(sentence[i], negative_words)
  }
  score = tmp_pos- tmp_neg
}

result3 = foreach(i=(147690+1):207691, .packages = c("data.table", "stringr")) %dopar% {
  tmp_pos = 0
  tmp_neg = 0
  
  sentence = unlist(str_split(tweets_to_analyse2[i], pattern = " "))
  for (i in 1:length(sentence)) {
    if(is.na(match(sentence[i], positive_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
    if(is.na(match(sentence[i], negative_words)) == FALSE)
      tmp_neg = tmp_neg + match(sentence[i], negative_words)
  }
  score = tmp_pos - tmp_neg
}

result4 = foreach(i=(207691+1):267692, .packages = c("data.table", "stringr")) %dopar% {
  tmp_pos = 0
  tmp_neg = 0
  
  sentence = unlist(str_split(tweets_to_analyse2[i], pattern = " "))
  for (i in 1:length(sentence)) {
    if(is.na(match(sentence[i], positive_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
    if(is.na(match(sentence[i], negative_words)) == FALSE)
      tmp_neg = tmp_neg + match(sentence[i], negative_words)
  }
  score = tmp_pos - tmp_neg
}

result5 = foreach(i=(267692+1):327689, .packages = c("data.table", "stringr")) %dopar% {
  tmp_pos = 0
  tmp_neg = 0
  
  sentence = unlist(str_split(tweets_to_analyse2[i], pattern = " "))
  for (i in 1:length(sentence)) {
    if(is.na(match(sentence[i], positive_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
    if(is.na(match(sentence[i], negative_words)) == FALSE)
      tmp_neg = tmp_neg + match(sentence[i], negative_words)
  }
  score = tmp_pos - tmp_neg
}

#stop cluster
stopCluster(cores_to_use)

# Subtract time to find out how long it took to run
time_it_took <- proc.time()-start
time_it_took

# combine the results into a total
total = unlist(c(result1, result2, result3, result4, result5))

# create a reaction to find out if value is positive or negative
reaction = c()

# put in negative/positive/neutral depending on the result
for(i in 1:length(total)){
  if(total[i] > 0){
    reaction[i] = "positive"
  } else if(total[i] < 0) {
    reaction[i] = "negative"
  } else if(total[i] == 0){
    reaction[i] = "neutral"
  }
}

# save as another sentiment score and value
tweet_sentiment_score2 = total
tweet_sentiment_value2 = reaction

# add columns to the sentiment score and value
tweets_sentiment_score$tweet_sentiment_score2 = tweet_sentiment_score2
tweets_sentiment_score$tweet_sentiment_value2 = tweet_sentiment_value2

# rename for clarity
names(tweets_sentiment_score[2]) = "tweet_sentiment_score1"
names(tweets_sentiment_score[3]) = "tweet_sentiment_value1"

# save file
write.csv(tweets_sentiment_score, "tweets_sentiment_score_updated.csv")

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

test = read.csv("tweets.csv")

nrow(test)

nrow(tweets_sentiment_score)
nrow(test)

sapply(test, class)

head(test)
###############################################################


#### Time Series Analysis ####

#### TF-IDF ####