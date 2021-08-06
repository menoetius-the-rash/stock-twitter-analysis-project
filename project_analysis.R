install.packages("dotenv")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("wordcloud")
install.packages("stringr")
install.packages("RColorBrewer")
install.packages("doParallel")
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("qdap")
install.packages("openNLP")
install.packages("data.table")
install.packages("SentimentAnalysis")
install.packages("gmodels")
install.packages("e1071")

library(dotenv)
library(ggplot2)
library(dplyr)
library(tidyr)
library(e1071)
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
library(gmodels)

# used for positive and negative words https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
#location="C:/Users/Jin/Documents/Stock Twitter Analysis Project/stock-twitter-analysis-project"
location = "/home/jin/Dropbox/2020 Course/Modules/Project/stock-twitter-analysis-project"

## load configuration
load_dot_env("config.env")

# get location from config file
location = Sys.getenv("location")

# set location
setwd(location)

#### Sentimental Analysis ####
tweets_location = paste0(location, "/tweets")
getwd()
# set location
setwd(tweets_location)

# read file to be analyse for sentiment
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

# tweets strip of unnecessary stuff
tweets_cleaned = tweets
raw_tweets = tweets
raw_tweets_cleaned = raw_tweets

# removing unnecessary text using gsub prior to creating corpus
tweets_cleaned$Text = gsub("http\\S+","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("&\\S+","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\d+","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("@#","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\s*\\$\\w*","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\s*\\#\\w*","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("\\s*\\@\\w*","",tweets_cleaned$Text)
tweets_cleaned$Text = gsub("[\r\n]","",tweets_cleaned$Text) 
tweets_cleaned$Text = gsub("[[:punct:]]","",tweets_cleaned$Text)
tweets_cleaned$Text = iconv(tweets_cleaned$Text, "latin1", "ASCII", sub="") #emojis
tweets_cleaned$Text = gsub("^[[:space:]]*","",tweets_cleaned$Text) # Remove leading whitespaces
tweets_cleaned$Text = gsub("[[:space:]]*$","",tweets_cleaned$Text) # Remove trailing whitespaces
tweets_cleaned$Text = gsub(" +"," ", tweets_cleaned$Text) # remove extra whitespaces
tweets_cleaned$Text =  gsub("  ", " ", tweets_cleaned$Text) #Replace double space with single space

raw_tweets_cleaned$Text = gsub("http\\S+","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("&\\S+","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("\\d+","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("@#","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("\\s*\\$\\w*","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("\\s*\\#\\w*","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("\\s*\\@\\w*","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("[\r\n]","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("[[:punct:]]","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = iconv(raw_tweets_cleaned$Text, "latin1", "ASCII", sub="") #emojis
raw_tweets_cleaned$Text = gsub("^[[:space:]]*","",raw_tweets_cleaned$Text) # Remove leading whitespaces
raw_tweets_cleaned$Text = gsub("[[:space:]]*$","",raw_tweets_cleaned$Text) # Remove trailing whitespaces
raw_tweets_cleaned$Text = gsub(" +"," ", raw_tweets_cleaned$Text) # remove extra whitespaces
raw_tweets_cleaned$Text =  gsub("  ", " ", raw_tweets_cleaned$Text) #Replace double space with single space

# remove duplicates for both raw_tweets_cleaned and tweets_cleaned
raw_tweets_cleaned = raw_tweets_cleaned[!duplicated(raw_tweets_cleaned$Text),]
tweets_cleaned = tweets_cleaned[!duplicated(tweets_cleaned$Text),]

length(raw_tweets_cleaned$Text)
length(tweets_cleaned$Text)
#[1] 190023

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

# remove numbers
tweet_clean_corpus = tm_map(tweet_clean_corpus, removeNumbers)

# remove whitespace
tweet_clean_corpus = tm_map(tweet_clean_corpus, stripWhitespace)

# remove stop words
my_stopwords = c("day", "will","week","time","today","stock","trade","call","go","look","get", "will")
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
limit = 40000
tweets_sentiment1 = analyzeSentiment(tweets_to_analyse[1:limit]) # 1 - 40000
limit2 = limit + 40000
tweets_sentiment2 = analyzeSentiment(tweets_to_analyse[(limit+1):limit2]) # 40001 - 80000
limit3 = limit2 + 40000 
tweets_sentiment3 = analyzeSentiment(tweets_to_analyse[(limit2+1):limit3]) # 80001 - 120000
limit4 = limit3 + 40000 
tweets_sentiment4 = analyzeSentiment(tweets_to_analyse[(limit3+1):limit4]) # 120001 - 160000
limit5 = limit4 + 30023 
tweets_sentiment5 = analyzeSentiment(tweets_to_analyse[(limit4+1):limit5]) # 160001 - 190023

# Checking if row is not enough
totalrows = nrow(tweets_sentiment1) + nrow(tweets_sentiment2) + nrow(tweets_sentiment3) + nrow(tweets_sentiment4)+ nrow(tweets_sentiment5)
totalrows

# bind everything through caret for the sentiment score
tweets_sentiment_score1 = c(tweets_sentiment1$SentimentQDAP, tweets_sentiment2$SentimentQDAP, 
                            tweets_sentiment3$SentimentQDAP, tweets_sentiment4$SentimentQDAP,      
                            tweets_sentiment5$SentimentQDAP)

# get the sentiment value for each score
tweet_sentiment_value1 = convertToDirection(tweets_sentiment_score)

# bind this back to the tweets cleaned
tweets_cleaned = cbind(tweets_cleaned, tweets_sentiment_score1, tweet_sentiment_value1)
head(tweets_cleaned)

# save file
write.csv(tweets_cleaned, "tweets_sentiment_score.csv", row.names = FALSE)


# compare with using positive/negative  word lists for sentiment from using QDAP vs list of words
tweets_sentiment_score = read.csv("tweets_sentiment_score.csv")

# scan provided positive/negative words set list
positive_words = scan('positive-words.txt',what = 'character')
negative_words = scan('negative-words.txt',what = 'character')

# add other words that can help identify positive/negative sentiments
positive_words = c(positive_words, "buy", "rising", "rip")
negative_words = c(negative_words, "sell", "dip", "crash")

tweets_to_analyse2 = raw_tweets_cleaned$Text

length(raw_tweets_cleaned$Text)
# [1] 190023

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

result1 = foreach(i=1:40000, .packages = c("data.table", "stringr")) %dopar% {
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

result2 = foreach(i=(40000+1):80000, .packages = c("data.table", "stringr")) %dopar% {
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

result3 = foreach(i=(80000+1):120000, .packages = c("data.table", "stringr")) %dopar% {
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

result4 = foreach(i=(120000+1):160000, .packages = c("data.table", "stringr")) %dopar% {
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

result5 = foreach(i=(160000+1):190023, .packages = c("data.table", "stringr")) %dopar% {
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
#   user  system elapsed 
# 47.73   10.56   64.42 

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
tweets_sentiment_score_v2$tweet_sentiment_score2 = tweet_sentiment_score2
tweets_sentiment_score_v2$tweet_sentiment_value2 = tweet_sentiment_value2

tweets_sentiment_score_v2 = as.data.frame(cbind(tweets_to_analyse2,total, reaction))
tweets_sentiment_score_v2$total = as.numeric(tweets_sentiment_score_v2$total)

# save file
write.csv(tweets_sentiment_score, "tweets_sentiment_score_updated.csv", row.names = FALSE)
write.csv(tweets_sentiment_score_v2, "tweets_sentiment_score_updated_v2.csv", row.names = FALSE)

# create term document matrix for QDAP method
tweet_clean_corpus = read.csv("tweets_sentiment_score_updated.csv")
tweets_sentiment_score_v2 = read.csv("tweets_sentiment_score_updated_v2.csv")
tweet_clean_corpus = Corpus(VectorSource(tweet_clean_corpus$text[1:30000]))
tweet_clean_corpus_dtm <- DocumentTermMatrix(tweet_clean_corpus)

head(tweet_clean_corpus_dtm)

# get top 10 most frequent words
freq_words_corp <- freq_terms(tweet_clean_corpus, 10)
# plot to see them
plot(term_count)
p<-ggplot(freq_words_corp, aes(x=FREQ, y=WORD, fill = FREQ)) + 
  xlab("Count") + ylab("Word") + labs(fill="Frequency") + 
  geom_bar(stat="identity")
p

# get lowest frequency words that appear
low_freq_20 <- findFreqTerms(tweet_clean_corpus_dtm, lowfreq = 20)

# remove sparse terms that we do not need:
sparse_tweet_clean_corpus_dtm <- removeSparseTerms(tweet_clean_corpus_dtm, 0.995)
sparse_tweet_clean_corpus_dtm

# transform it back to a data frame
sparse_tweets <- as.data.frame(as.matrix(sparse_tweet_clean_corpus_dtm))

colnames(sparse_tweets) <- make.names(colnames(sparse_tweets))

#### training and test datasets for QDAP method ####

# Data will be divided into an 30% for testing purposes and 70% for training

raw_length = nrow(tweets_sentiment_score_v2)
raw_length = 30000
raw_tweets_train = tweets_sentiment_score_v2[1:round(.7 * raw_length),]
raw_tweets_test  = tweets_sentiment_score_v2[(round(.7 * raw_length)+1):raw_length,]


tweet_clean_length = length(tweet_clean_corpus)
tweet_clean_length = 30000
tweet_clean_corpus_train = tweet_clean_corpus[1:round(.7 * tweet_clean_length)]
tweet_clean_corpus_test  = tweet_clean_corpus[(round(.7 * tweet_clean_length)+1):tweet_clean_length]


length_corpus = nrow(tweet_clean_corpus_dtm)
length_corpus = 30000
tweet_clean_corpus_dtm_train = tweet_clean_corpus_dtm[1:round(.7 * length_corpus),]
tweet_clean_corpus_dtm_test  = tweet_clean_corpus_dtm[(round(.7 * length_corpus)+1):length_corpus,]


# Create a word cloud to show the frequency of the data we have set the frequency to 2000
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(tweet_clean_corpus_train, min.freq = 2000, random.order = FALSE, scale = c(3, 0.5))


# Create a word clouds specific to negative and positive
positive_tweets = subset(raw_tweets_train, reaction == "positive")
negative_tweets = subset(raw_tweets_train, reaction == "negative")

# word cloud for positive tweets
wordcloud(positive_tweets$tweets_to_analyse2, min.freq = 2000, random.order = FALSE, scale = c(3, 0.5))

# word cloud for positive tweets
wordcloud(negative_tweets$tweets_to_analyse2, min.freq = 3000, random.order = FALSE, scale = c(3, 0.5))

# Reduce the number of features that eliminate any words that show in less than two texts. 
frequent_terms = findFreqTerms(tweet_clean_corpus_dtm_train, 2)
freq_clean_corpus_dtm_train = DocumentTermMatrix(tweet_clean_corpus_train, list(dictionary = frequent_terms))
freq_clean_corpus_dtm_test = DocumentTermMatrix(tweet_clean_corpus_test, list(dictionary = frequent_terms))


# create a function that will counting will be converted to a yes or no
conv_count_to_factor = function(i) {
  i = ifelse(i > 0, "Yes", "No")
}

remove(result1)
remove(result2)
remove(result3)
remove(result4)
remove(result5)

gc()
memory.limit(size = 60000)

#apply the function created to the columns for test/train
freq_clean_corpus_dtm_train = apply(freq_clean_corpus_dtm_train, 2, conv_count_to_factor)
freq_clean_corpus_dtm_test = apply(freq_clean_corpus_dtm_test, 2, conv_count_to_factor)
freq_clean_corpus_dtm_train
# Create model forprediction
nb_classifier <- naiveBayes(freq_clean_corpus_dtm_train, raw_tweets_train$reaction)
nb.pred <- predict(nb_classifier, freq_clean_corpus_dtm_test)

CrossTable(nb.pred, raw_tweets_test$tweets_to_analyse2
           ,
           prop.chisq = FALSE, 
           prop.t = FALSE,
           dnn = c('predicted', 'actual'))
###############################################################

#### Time Series Analysis ####

#### TF-IDF ####