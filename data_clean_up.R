library(dotenv)
library(stringr)
library(foreach)
library(doParallel)
library(tidyverse)
library(lubridate)
install.packages("lubridate")
install.packages('Rcpp')

load_dot_env("config.env")
location = Sys.getenv("location")
location
setwd(location)
getwd()

stock_test = read.csv("AAPL.csv")
tweets = read.csv("tweets.csv")

# editing timezone to match with the tweets
typeof(stock_test$time)
typeof(tweets$Datetime)

pb.txt <- "2009-06-03 08:00+00:00"
pb.date <- as.POSIXct(pb.txt, tz="Atlantic/Reykjavik")
pb.date
format(pb.date, tz="Europe/Dublin",usetz=TRUE)
test = format(pb.date, tz="America/New_York",usetz=TRUE)

# Europe/Dublin
# America/New_York
# Atlantic/Reykjavik

head(stock_test$time)
head(tweets$Datetime)

test_tweets = tweets$Datetime
test_run = head(test_tweets)
pb.txt <- "2009-06-03 08:00+00:00"
pattern = "\\+00:00"
pb.txt.removed = str_remove_all(test_run[1], pattern)
test_run[1]
pb.txt.removed
converted_times = {}

for (x in test_run) {
  correct_time = str_remove(x, pattern)
  converted_times =  rbind(converted_times,correct_time)
}
converted_times

test_run[1]
for (i in 1:length(test_run)){
  x = str_remove(test_run[i], pattern)
  test_run[i] = x
}
test_run
typeof(test_run)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

test_run2 = tweets
length(test_run2)
test_run2$Datetime = foreach(i=1:length(test_run2$Datetime), .packages = "stringr") %dopar% {
                        x = str_remove(test_run2$Datetime[i], pattern)
                        test_run2$Datetime[i] = x
                      }
head(test_run2$Datetime)
sapply(test_run2, class)
test_run2$Datetime = unlist(test_run2$Datetime)
write.csv(test_run2, "tweets_time_cleaned.csv", row.names = FALSE)
head(test_run2)

#stop cluster
stopCluster(cl)


####### WORKING convert to Irish time
####### tweets

test.txt = lubridate::ymd_hms(test_run3$Datetime[1])
test.txt.GMTp1 = lubridate::with_tz(test.txt, "Europe/Dublin")
test.txt.GMTp1

test_run2 = read.csv("tweets_time_cleaned.csv")
cleaned = subset(test_run2, select =  -c(2,3,4,12))
head(cleaned)
write.csv(cleaned, "tweets_time_cleaned_trimmed.csv", row.names = FALSE)


test_run3 = read.csv("tweets_time_cleaned_trimmed.csv")

# time_in_UTC = lubridate::ymd_hms(test_run3$Datetime[i])
# converted_to_IST = lubridate::with_tz(time_in_UTC, "Europe/Dublin")
# numeric_date = as.numeric(as.character(converted_to_IST))
# numeric_date_converted = as.POSIXct(numeric_date, origin = "1950-01-01") 
# test_run3$Datetime[i] = numeric_date_converted
test_run4 = test_run3
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

start <- proc.time()
test_run4$Datetime = foreach(i=1:length(test_run4$Datetime), .packages = "lubridate") %dopar% {
  #loop contents here
  time_in_UTC = lubridate::ymd_hms(test_run4$Datetime[i])
  converted_to_IST = lubridate::with_tz(time_in_UTC, "Europe/Dublin")
  test_run4$Datetime[i] = converted_to_IST
}
time_it_took <- proc.time()-start
# user  system elapsed 
# 139.98   20.64  282.19 
time_it_took
#stop cluster
stopCluster(cl)

# convert list of dates into a vector by using do.call to keep date and time name instead of displaying as numeric
test_run4$Datetime = do.call("c", test_run4$Datetime)
# write to file
write.csv(test_run4, "tweets_time_cleaned_trimmed_time_converted.csv", row.names = FALSE)


###### stocks Irish time
stock_test = read.csv("AAPL.csv")

# convert time

test_run5 = stock_test

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

start <- proc.time()
#convert
test_run5$time = foreach(i=1:length(test_run5$time), .packages = "lubridate") %dopar% {
  #loop contents here
  tz_assigned = as.POSIXct(test_run5$time[i], tz="America/New_York")
  tz_converted = lubridate::with_tz(tz_assigned, "Europe/Dublin")
}

time_it_took <- proc.time()-start
time_it_took
# user  system elapsed 
# 9.99    1.39   14.11 

#stop cluster
stopCluster(cl)

# convert list of dates into a vector by using do.call to keep date and time name instead of displaying as numeric
test_run5$time = do.call("c", test_run5$time)
write.csv(test_run5, "AAPL_time_converted.csv", row.names = FALSE)

typeof(test_run5$time)
head(test_run5)
head(stock_test)

OlsonNames()

