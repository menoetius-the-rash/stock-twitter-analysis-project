library(dotenv)
library(stringr)
library(foreach)
library(doParallel)
library(tidyverse)
library(lubridate)
library(data.table)
library(digest)

load_dot_env("config.env")
location = Sys.getenv("location")
location
setwd(location)
getwd()

stock_test = read.csv("AAPL.csv")
tweets = read.csv("tweets.csv")


#setup parallel backend to use many processors
# remove the pattern "+00:00" from the time
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

test_run2 = tweets
length(test_run2)
pattern = "\\+00:00"

test_run2$Datetime = foreach(i=1:length(test_run2$Datetime), .packages = "stringr") %dopar% {
                        x = str_remove(test_run2$Datetime[i], pattern)
                        test_run2$Datetime[i] = x
                      }

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
cleaned = subset(test1, select =  -c(2,3,4,12,13))
head(cleaned)
write.csv(cleaned, "tweets_time_cleaned_trimmed.csv", row.names = FALSE)
write.csv(cleaned, "FSLY_cleaned_trimmed.csv", row.names = FALSE)
cleaned$Datetime = do.call("c", cleaned$Datetime)

test_run3 = read.csv("tweets_time_cleaned_trimmed.csv")


test_run4 = test_run3
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

start <- proc.time()
test1$Datetime = foreach(i=1:length(test1$Datetime), .packages = "lubridate") %dopar% {
  #loop contents here
  time_in_UTC = lubridate::ymd_hms(test1$Datetime[i])
  converted_to_IST = lubridate::with_tz(time_in_UTC, "Europe/Dublin")
  test1$Datetime[i] = converted_to_IST
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
test1$time = foreach(i=1:length(test1$time), .packages = "lubridate") %dopar% {
  #loop contents here
  tz_assigned = as.POSIXct(test1$time[i], tz="America/New_York")
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

# apply this to all stock csvs
stock_folder = paste0(location,"/stocks")
setwd(stock_folder)
getwd()

list_of_files = list.files(pattern="*.csv")
myfiles = lapply(list_of_files, read.delim)
length(list_of_files)

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
start <- proc.time()
for (stock in 1:length(list_of_files)) {
  stocks = read.csv(list_of_files[stock])
  stocks$time = foreach(i=1:length(stocks$time), .packages = "lubridate") %dopar% {
    #loop contents here
    tz_assigned = as.POSIXct(stocks$time[i], tz="America/New_York")
    tz_converted = lubridate::with_tz(tz_assigned, "Europe/Dublin")
  }
  stock_file_update = str_split(list_of_files[stock], pattern = "\\.")
  stocks$time = do.call("c", stocks$time)
  stock_file_update = unlist(stock_file_update)
  stock_rewritten = as.character(paste0(stock_file_update[1], "_updated.", stock_file_update[2]))
  stocks$stock = stock_file_update[1]
  write.csv(stocks, stock_rewritten, row.names = FALSE)
}
time_it_took <- proc.time()-start
time_it_took
stopCluster(cl)

###### pseudonymisation of usernames
setwd(location)
tweets = read.csv("FSLY_tweets.csv")

test1 = tweets

pseudonymize_names = sapply(test1$Username, digest, algo="crc32", serialize=FALSE)

test1$Username = pseudonymize_names

write.csv(test1, "tweets_cleaned_pseudonymized.csv", row.names = FALSE)

head(test1)
