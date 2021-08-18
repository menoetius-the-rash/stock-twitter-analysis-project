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
install.packages("viridis")
install.packages("TSstudio")
install.packages("xts")
install.packages("caret")

library(viridis)
library(caret)
library(xts)
library(TSstudio)
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


#location="C:/Users/Jin/Documents/Stock Twitter Analysis Project/stock-twitter-analysis-project"
#location = "/home/jin/Dropbox/2020 Course/Modules/Project/stock-twitter-analysis-project"

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
# remove https
tweets_cleaned$Text = gsub("http\\S+","",tweets_cleaned$Text)
# remove & and so on
tweets_cleaned$Text = gsub("&\\S+","",tweets_cleaned$Text)
# remove numbers
tweets_cleaned$Text = gsub("\\d+","",tweets_cleaned$Text)
# remove $ tags
tweets_cleaned$Text = gsub("\\s*\\$\\w*","",tweets_cleaned$Text)
# remove #  tags
tweets_cleaned$Text = gsub("\\s*\\#\\w*","",tweets_cleaned$Text)
# remove @ tags
tweets_cleaned$Text = gsub("\\s*\\@\\w*","",tweets_cleaned$Text)
# remove any breaks
tweets_cleaned$Text = gsub("[\r\n]","",tweets_cleaned$Text)
# punctuations removed
tweets_cleaned$Text = gsub("[[:punct:]]","",tweets_cleaned$Text)
# emojis
tweets_cleaned$Text = iconv(tweets_cleaned$Text, "latin1", "ASCII", sub="") 
# whitespaces leading removed
tweets_cleaned$Text = gsub("^[[:space:]]*","",tweets_cleaned$Text)
# whitespaces trailing removed
tweets_cleaned$Text = gsub("[[:space:]]*$","",tweets_cleaned$Text) 
# remove any extra spaces
tweets_cleaned$Text = gsub(" +"," ", tweets_cleaned$Text) 
# any double spaces "  " replaced with only one space 
tweets_cleaned$Text =  gsub("  ", " ", tweets_cleaned$Text) 

raw_tweets_cleaned$Text = gsub("http\\S+","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("&\\S+","",raw_tweets_cleaned$Text)
raw_tweets_cleaned$Text = gsub("\\d+","",raw_tweets_cleaned$Text)
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

write.csv(raw_tweets_cleaned, "raw_tweets_cleaned.csv", row.names = FALSE)

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
  # create temp variables for positive and negative
  tmp_pos = 0
  tmp_neg = 0
  
  # sentence is split and unlisted in order to prepare to match with the list of negative and positive words
  sentence = unlist(str_split(tweets_to_analyse2[i], pattern = " "))
  for (i in 1:length(sentence)) {
    # if na and matches == false then return the matched result to the temporary variable
    if(is.na(match(sentence[i], positive_words)) == FALSE)
      tmp_pos = tmp_pos + match(sentence[i], positive_words)
    # similar to the above but for the negative value
    if(is.na(match(sentence[i], negative_words)) == FALSE)
      tmp_neg = tmp_neg + match(sentence[i], negative_words)
  }
  # set to the positive matched words and for negative matched words
  positive_matched_words = match(sentence, positive_words)
  negative_matched_words = match(sentence, negative_words)
  # return a score calculate the score
  score = tmp_pos - tmp_neg
}
# do the same again 5 times
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
tweet_clean_corpus = Corpus(VectorSource(tweet_clean_corpus$text[1:50000]))
tweet_clean_corpus_dtm = DocumentTermMatrix(tweet_clean_corpus)

head(tweet_clean_corpus_dtm)

# get top 10 most frequent words
freq_words_corp = freq_terms(tweet_clean_corpus, 10)
# plot to see them
plot(freq_words_corp)
p = ggplot(freq_words_corp, aes(x=FREQ, y=WORD, fill = FREQ)) + 
  xlab("Count") + ylab("Word") + labs(fill="Frequency") + 
  geom_bar(stat="identity")
p

#### training and test datasets ####

# Data will be divided into an 20% for testing purposes and 80% for training

raw_length = 50000
raw_tweets_train = tweets_sentiment_score_v2[1:round(.8 * raw_length),]
raw_tweets_test  = tweets_sentiment_score_v2[(round(.8 * raw_length)+1):raw_length,]

tweet_clean_length = 50000
tweet_clean_corpus_train = tweet_clean_corpus[1:round(.8 * tweet_clean_length)]
tweet_clean_corpus_test  = tweet_clean_corpus[(round(.8 * tweet_clean_length)+1):tweet_clean_length]

length_corpus = 50000
tweet_clean_corpus_dtm_train = tweet_clean_corpus_dtm[1:round(.8 * length_corpus),]
tweet_clean_corpus_dtm_test  = tweet_clean_corpus_dtm[(round(.8 * length_corpus)+1):length_corpus,]

# Create a word cloud to show the frequency of the data we have set the frequency to 2000
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(tweet_clean_corpus_train, min.freq = 8000, random.order = FALSE, scale = c(3, 0.5))


# Reduce the number of features that eliminate any words that show in less than two texts. 
frequent_terms = findFreqTerms(tweet_clean_corpus_dtm_train, 2)
freq_clean_corpus_dtm_train = DocumentTermMatrix(tweet_clean_corpus_train, list(dictionary = frequent_terms))
freq_clean_corpus_dtm_test = DocumentTermMatrix(tweet_clean_corpus_test, list(dictionary = frequent_terms))


# create a function that counting will be converted to a yes or no
conv_count_to_factor = function(i) {
  i = ifelse(i > 0, "Yes", "No")
}

gc()

#apply the function created to the columns for test/train
freq_clean_corpus_dtm_train = apply(freq_clean_corpus_dtm_train, 2, conv_count_to_factor)
freq_clean_corpus_dtm_test = apply(freq_clean_corpus_dtm_test, 2, conv_count_to_factor)

# Create model for prediction
nb_classifier <- naiveBayes(freq_clean_corpus_dtm_train, raw_tweets_train$reaction)
nb_pred <- predict(nb_classifier, freq_clean_corpus_dtm_test)


capture_result = capture.output(results_table = CrossTable(nb_pred, raw_tweets_test$reaction,
                                                                prop.chisq = FALSE, 
                                                                prop.t = FALSE,
                                                                dnn = c('predicted_results', 'actual_results'))
                                     )


#write results into a csv

write.csv(capture_result, "results_table.csv", row.names = FALSE)

# [4] "|-------------------------|"                                         
# [5] "|                       N |"                                         
# [6] "|           N / Row Total |"                                         
# [7] "|           N / Col Total |"                                         
# [8] "|-------------------------|"                                         
# [9] ""                                                                    
# [10] " "                                                                   
# [11] "Total Observations in Table:  10000 "                                
# [12] ""                                                                    
# [13] " "                                                                   
# [14] "                  | actual_results "                                 
# [15] "predicted_results |  negative |   neutral |  positive | Row Total | "
# [16] "------------------|-----------|-----------|-----------|-----------|" 
# [17] "         negative |      1722 |       437 |       405 |      2564 | "
# [18] "                  |     0.672 |     0.170 |     0.158 |     0.256 | "
# [19] "                  |     0.475 |     0.122 |     0.145 |           | "
# [20] "------------------|-----------|-----------|-----------|-----------|" 
# [21] "          neutral |      1292 |      2892 |       727 |      4911 | "
# [22] "                  |     0.263 |     0.589 |     0.148 |     0.491 | "
# [23] "                  |     0.357 |     0.807 |     0.260 |           | "
# [24] "------------------|-----------|-----------|-----------|-----------|" 
# [25] "         positive |       608 |       256 |      1661 |      2525 | "
# [26] "                  |     0.241 |     0.101 |     0.658 |     0.252 | "
# [27] "                  |     0.168 |     0.071 |     0.595 |           | "
# [28] "------------------|-----------|-----------|-----------|-----------|" 
# [29] "     Column Total |      3622 |      3585 |      2793 |     10000 | "
# [30] "                  |     0.362 |     0.358 |     0.279 |           | "
# [31] "------------------|-----------|-----------|-----------|-----------|" 
#
#     Negative = (0.672+ 0.475)/2 = 0.573
#     Neutral = (0.589 + 0.807)/2 = 0.698
#     Positive = (0.658 + 0.595)/2 = 0.627
#

# Try and improve the model
nb_classifier2 <- naiveBayes(freq_clean_corpus_dtm_train, raw_tweets_train$reaction, laplace = 1)
nb_pred2 <- predict(nb_classifier2, freq_clean_corpus_dtm_test)


capture_result2 = capture.output(results_table = CrossTable(nb_pred2, raw_tweets_test$reaction,
                                                           prop.chisq = FALSE, 
                                                           prop.t = FALSE,
                                                           dnn = c('predicted_results', 'actual_results'))
)

# [11] "Total Observations in Table:  10000 "                                
# [12] ""                                                                    
# [13] " "                                                                   
# [14] "                  | actual_results "                                 
# [15] "predicted_results |  negative |   neutral |  positive | Row Total | "
# [16] "------------------|-----------|-----------|-----------|-----------|" 
# [17] "         negative |      1800 |       451 |       310 |      2561 | "
# [18] "                  |     0.703 |     0.176 |     0.121 |     0.256 | "
# [19] "                  |     0.497 |     0.126 |     0.111 |           | "
# [20] "------------------|-----------|-----------|-----------|-----------|" 
# [21] "          neutral |      1158 |      2831 |       629 |      4618 | "
# [22] "                  |     0.251 |     0.613 |     0.136 |     0.462 | "
# [23] "                  |     0.320 |     0.790 |     0.225 |           | "
# [24] "------------------|-----------|-----------|-----------|-----------|" 
# [25] "         positive |       664 |       303 |      1854 |      2821 | "
# [26] "                  |     0.235 |     0.107 |     0.657 |     0.282 | "
# [27] "                  |     0.183 |     0.085 |     0.664 |           | "
# [28] "------------------|-----------|-----------|-----------|-----------|" 
# [29] "     Column Total |      3622 |      3585 |      2793 |     10000 | "
# [30] "                  |     0.362 |     0.358 |     0.279 |           | "
# [31] "------------------|-----------|-----------|-----------|-----------|" 
# [32] ""                                                                    
# [33] " "  
# Negative = (0.703 + 0.497) / 2 = 0.6
# Neutral = (0.613 + 0.790) / 2 = 0.7015
# Positive = (0.657 + 0.664)/2 = 0.6605

#write results into a csv

write.csv(capture_result2, "results_table2.csv", row.names = FALSE)

# Evaluate using a confusion matrix:
conf_matrix <- table(pred= nb_pred2, raw_tweets_test$reaction)

confusionMatrix(conf_matrix)
# 
# Confusion Matrix and Statistics
# 
# 
# pred       negative neutral positive
# negative     1800     451      310
# neutral      1158    2831      629
# positive      664     303     1854
# 
# Overall Statistics
# 
# Accuracy : 0.6485          
# 95% CI : (0.6391, 0.6579)
# No Information Rate : 0.3622          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4697          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
#                      Class: negative Class: neutral Class: positive
# Sensitivity                   0.4970         0.7897          0.6638
# Specificity                   0.8807         0.7214          0.8658
# Pos Pred Value                0.7029         0.6130          0.6572
# Neg Pred Value                0.7551         0.8599          0.8692
# Prevalence                    0.3622         0.3585          0.2793
# Detection Rate                0.1800         0.2831          0.1854
# Detection Prevalence          0.2561         0.4618          0.2821
# Balanced Accuracy             0.6888         0.7556          0.7648

###############################################################

#### Time Series Analysis ####

# Combine the pricing of the stock after 5mins, 10mins, 15mins, 30mins

# Load stock files
stock_location = paste0(location,"/stocks")
setwd(stock_location)
stocks = read.csv("stocks_updated_trimmed.csv", colClasses = c("character", "character",  "numeric", "numeric", "numeric", 
                                                                 "numeric", "integer", "character"))
# load tweet files
tweet_location = paste0(location,"/tweets")
setwd(tweet_location)
tweets = read.csv("raw_tweets_cleaned.csv", colClasses = c("character" , "character", "character",  "character", "numeric", "numeric", "character", "numeric", "numeric" , "numeric"))

# read results with sentiment
tweets_with_sentiment = read.csv("tweets_sentiment_score_updated_v2.csv")

# add sentiment reaction to the correlated stock
tweets$sentiment = tweets_with_sentiment$reaction

# Remove unneeded columns
tweets = subset(tweets, select=-c(7))
stocks = subset(stocks, select=-c(3,4,5))
nrow(tweets)

# rename stock column in tweets
colnames(tweets)[3] = "stock"
colnames(tweets)

head(tweets)
head(stocks)
# fill in price of stocks using close for the stocks and the corresponding time
new_dataset_test = tweets %>% left_join(stocks, by=c("date","time","stock"))

# check if there are any NAs 
any(is.na(new_dataset_test$close))

# remove any NAs
new_dataset_test2 = na.omit(new_dataset_test)
new_dataset_test2 = subset(new_dataset_test2, select = -c(12))
head(new_dataset_test2)

# create 5 mins date and time called date2 and time2

# combine both original date and time
dateTime = paste(new_dataset_test2$date, new_dataset_test2$time)
# convert time back to date time with timezone Irish time
dateTime = as.POSIXct(dateTime,tz="Europe/Dublin")

# create the dateTime2 for adding 5 mins to the dateTime and add 5 mins to it
dateTime2 = lubridate::minutes(5) + dateTime

# add the datetime2 to a new column called time2
new_dataset_test2$time2 = dateTime2

# use tidyr separate to separate both date and time and name them as date2/time2
new_dataset_test2 = tidyr::separate(new_dataset_test2, time2, c("date2", "time2"), sep = " ")

# strip seconds off the time2
new_dataset_test2$time2 = substr(new_dataset_test2$time2, 1, 5)

# remove volume column
stocks = subset(stocks, select = -c(4))

# rename columns for join
colnames(stocks)[1] = "date2"
colnames(stocks)[2] = "time2"
colnames(stocks)[3] = "close2"

# close2 column will be the stock price after 5min
new_dataset_test3 = new_dataset_test2 %>% left_join(stocks, by=c("date2","time2","stock"))

# remove NAs in new_dataset_test3
new_dataset_test3 = na.omit(new_dataset_test3)

head(new_dataset_test3)
nrow(new_dataset_test3)

# similar method as the above but this time 10 mins after 5mins above so 15min
dateTime = paste(new_dataset_test3$date, new_dataset_test3$time)
# convert time back to date time with timezone Irish time
dateTime3 = as.POSIXct(dateTime,tz="Europe/Dublin")
dateTime3 = lubridate::minutes(15) + dateTime3

# add the datetime2 to a new column called time2
new_dataset_test3$time3 = dateTime3

# use tidyr separate to separate both date and time and name them as date2/time2
new_dataset_test3 = tidyr::separate(new_dataset_test3, time3, c("date3", "time3"), sep = " ")

# strip seconds off the time2
new_dataset_test3$time3 = substr(new_dataset_test3$time3, 1, 5)

# rename columns for join
colnames(stocks)[1] = "date3"
colnames(stocks)[2] = "time3"
colnames(stocks)[3] = "close3"

# close3 will be price of stock after 15 mins of original tweet time
new_dataset_test4 = new_dataset_test3 %>% left_join(stocks, by=c("date3","time3","stock"))


# remove NAs in new_dataset_test4
new_dataset_test4 = na.omit(new_dataset_test4)

head(new_dataset_test4)
nrow(new_dataset_test4)

# do the same cycle again but for 30 mins after original tweet
dateTime = paste(new_dataset_test4$date, new_dataset_test4$time)
dateTime4 = as.POSIXct(dateTime,tz="Europe/Dublin")
dateTime4 = lubridate::minutes(30) + dateTime4
new_dataset_test4$time4 = dateTime4
new_dataset_test4 = tidyr::separate(new_dataset_test4, time4, c("date4", "time4"), sep = " ")
new_dataset_test4$time4 = substr(new_dataset_test4$time4, 1, 5)
colnames(stocks)[1] = "date4"
colnames(stocks)[2] = "time4"
colnames(stocks)[3] = "close4"
new_dataset_test4 = new_dataset_test4 %>% left_join(stocks, by=c("date4","time4","stock"))
new_dataset_test4 = na.omit(new_dataset_test4)
head(new_dataset_test4)
nrow(new_dataset_test4)

new_dataset_test5 = new_dataset_test4

# do the same but after 60mins/1hr
dateTime = paste(new_dataset_test5$date, new_dataset_test5$time)
dateTime5 = as.POSIXct(dateTime,tz="Europe/Dublin")
dateTime5 = lubridate::hours(1) + dateTime5
new_dataset_test5$time5 = dateTime5
new_dataset_test5 = tidyr::separate(new_dataset_test5, time5, c("date5", "time5"), sep = " ")
new_dataset_test5$time5 = substr(new_dataset_test5$time5, 1, 5)
colnames(stocks)[1] = "date5"
colnames(stocks)[2] = "time5"
colnames(stocks)[3] = "close5"
new_dataset_test5 = new_dataset_test5 %>% left_join(stocks, by=c("date5","time5","stock"))
new_dataset_test5 = na.omit(new_dataset_test5)
head(new_dataset_test5)
nrow(new_dataset_test5)


# save file for backup
write.csv(new_dataset_test5, "time_series_sentiment.csv", row.names = FALSE)

ts = read.csv("time_series_sentiment.csv")

nrow(ts)

colnames(ts)

#rename columns we need for clarity
colnames(ts)[11] = "priceCurrent"
colnames(ts)[14] = "price5min"
colnames(ts)[17] = "price15min"
colnames(ts)[20] = "price30min"
colnames(ts)[23] = "price60min"

# remove unnecessary columns
ts_clean = subset(ts, select = -c(12,13,15,16,18,19,21,22))
colnames(ts_clean)
head(ts_clean)

# create new columns that will describe percentage change from the changes
percent_change = function(x,y) {
  change = (y-x)/x
  return(round(change*100, digits = 3))
}

# Calculate the percentage change from the price of the tweet with the intervals
ts_clean$changePer5 = percent_change(ts_clean$priceCurrent,ts_clean$price5min)
ts_clean$changePer15 = percent_change(ts_clean$priceCurrent,ts_clean$price15min)
ts_clean$changePer30 = percent_change(ts_clean$priceCurrent,ts_clean$price30min)
ts_clean$changePer60 = percent_change(ts_clean$priceCurrent,ts_clean$price60min)

head(ts_clean)
colnames(ts_clean)

# create separate tables that order the change in decreasing order for  the change in percentage per time 
ts_clean_order5 = ts_clean[order(ts_clean$price5min, decreasing = TRUE),]
ts_clean_order15 = ts_clean[order(ts_clean$price15min, decreasing = TRUE),]
ts_clean_order30 = ts_clean[order(ts_clean$price30min, decreasing = TRUE),]
ts_clean_order60 = ts_clean[order(ts_clean$price60min, decreasing = TRUE),]
head(ts_clean_order5)
head(ts_clean_order15)
head(ts_clean_order30)
head(ts_clean_order60)

# check the frequency of the stocks mentioned
table(ts_clean$stock)

# use for example stock AMZN amazon.

amazonCurr5m = subset(ts_clean, stock=="AMZN", select = c(11, 12))

# do some work to turn date and time as actual date and actual time and not character
ts_clean_test = ts_clean 
ts_clean_test$date = as.Date(ts_clean$date)
ts_clean_test$time = as.POSIXct(ts_clean_test$time,format="%H:%M")
colnames(ts_clean_test)
# Positive and negative tweets separated
ts_clean_test_positive = subset(ts_clean_test, sentiment=="positive")
ts_clean_test_negative = subset(ts_clean_test, sentiment=="negative")

# plot chart to see if there is a visible correlation for positive tweets for 60 mins
ggplot(ts_clean_test_positive, aes(x=changePer60, y=price60min)) +
  geom_point(color="turquoise", size = 2) +
  labs(title = "Change in 1hr positive",
       x = "Percentage change in 1hr",
       y = "Stock Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color="black")

# plot chart to see if there is a visible correlation for negative tweets for 60mins
ggplot(ts_clean_test_negative, aes(x=changePer60, y=price60min)) +
  geom_point(color="red", size = 2) +
  labs(title = "Change in 1hr negative",
       x = "Percentage change in 1hr",
       y = "Stock Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color="black")

# get a table of frequency of sentiment
f_sentiment = as.data.frame(table(ts_clean_test$sentiment))

# graphing for Frequency of sentiment
ggplot(f_sentiment, mapping =  aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") + 
  labs(fill = "Sentiment",x = "Sentiment", y="Sentiment Breakdown", title = "Frequency of Sentiment")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=Freq), vjust=-0.3)

#Wilcoxon sign ranked sum test specific stock
amzn = subset(ts_clean_test, stock=="AMZN")

wilcox.test(amzn$priceCurrent, amzn$price5min, paired = TRUE, alternative = "two.sided")
# Wilcoxon signed rank test with continuity correction
# 
# data:  amzn$priceCurrent and amzn$price5min
# V = 1656338, p-value = 0.0003095
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(amzn$priceCurrent, amzn$price60min, paired = TRUE, alternative = "two.sided")
# Wilcoxon signed rank test with continuity correction
# 
# data:  amzn$priceCurrent and amzn$price60min
# V = 1779065, p-value = 2.807e-12
# alternative hypothesis: true location shift is not equal to 0


# Create a pie chart of the sentiment break down for amazon stock
amzn_sentiment = as.data.frame(table(amzn$sentiment))
amzn_sentiment$percent = round(100*amzn_sentiment$Freq/sum(amzn_sentiment$Freq), digits = 1)
amzn_sentiment$label = paste(amzn_sentiment$Var1," (", amzn_sentiment$percent,"%)", sep = "")
pie(amzn_sentiment$Freq, labels = amzn_sentiment$label, col = viridis(3))


wilcox.test(ts_clean_test$priceCurrent, ts_clean_test$price5min, paired = TRUE, alternative = "two.sided")
# Wilcoxon signed rank test with continuity correction
# 
# data:  ts_clean_test$priceCurrent and ts_clean_test$price5min
# V = 2392718269, p-value = 2.445e-10
# alternative hypothesis: true location shift is not equal to 0


# Focusing on just the positive and negative tweets
ts_positive_tweets = subset(ts_clean_test, sentiment=="positive")
ts_negative_tweets = subset(ts_clean_test, sentiment=="negative")
nrow(ts_positive_tweets)
nrow(ts_negative_tweets)

wilcox.test(ts_positive_tweets$priceCurrent, ts_positive_tweets$price5min, paired = TRUE, alternative = "two.sided")
# # 	Wilcoxon signed rank test with continuity correction for 5 minute positive
# 
# data:  ts_positive_tweets$priceCurrent and ts_positive_tweets$price5min
# V = 158701517, p-value = 0.0001946
# alternative hypothesis: true location shift is not equal to 0

wilcox.test(ts_negative_tweets$priceCurrent, ts_negative_tweets$price5min, paired = TRUE, alternative = "two.sided")
# Wilcoxon signed rank test with continuity correction for 5 minute negative
# 
# data:  ts_negative_tweets$priceCurrent and ts_negative_tweets$price5min
# V = 311765605, p-value = 0.02884
# alternative hypothesis: true location shift is not equal to 0

cor(ts_clean_test_positive$price5min,ts_clean_test_positive$changePer5)

cor(ts_clean_test_positive$changePer60, ts_clean_test_positive$price60min, method="spearman")

# Some time series for specific stocks
test_ts_amzn60m = xts(amzn$price60min, amzn$time)

# Plot time series for 60m
ts_plot(test_ts_amzn60m)


