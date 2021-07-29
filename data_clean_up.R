library(dotenv)
library(stringr)
library(foreach)
library(doParallel)
library(tidyverse)
library(lubridate)
library(data.table)
library(tibbletime)
library(digest)
library(dplyr)
library(readr)

load_dot_env("config.env")
location = Sys.getenv("location")
location
setwd(location)
getwd()

tweets = read.csv("tweets.csv")

#setup parallel backend to use many processors
# remove the pattern "+00:00" from the time
pattern = "\\+00:00"

# Use multicore to speed up process
c = detectCores()
# avoid overload buy only using maximum - 1 cores available
cores_to_use <- makeCluster(c[1]-1)
# register cores
registerDoParallel(cores_to_use)
# Time the running of the function
start <- proc.time()
# the for loop uses multicore and uses the package stringr to remove the "+00:00" pattern on each item
tweets$Datetime = foreach(i=1:length(tweets$Datetime), .packages = "stringr") %dopar% {
                        x = str_remove(tweets$Datetime[i], pattern)
                        tweets$Datetime[i] = x
                      }
#stop cluster
stopCluster(cores_to_use)

# Subtract time to find out how long it took to run
time_it_took <- proc.time()-start
time_it_took
# user  system elapsed 
# 166.31   24.44  241.03 

# unlist the result in order to be able to save in CSV
tweets$Datetime = unlist(tweets$Datetime)

# save file to CSV
write.csv(tweets, "tweets_time_cleaned.csv", row.names = FALSE)


#### Clean & Convert time to Irish time ####

#### Clean &Convert time on tweets to Irish time ####

# Read file
tweets = read.csv("tweets_time_cleaned.csv")

# Remove unneeded columns
cleaned = subset(tweets, select =  -c(2,4,12))

# Write file to back up
write.csv(cleaned, "tweets_time_cleaned_trimmed.csv", row.names = FALSE)

#cleaned$Datetime = do.call("c", cleaned$Datetime)

tweets = read.csv("tweets_time_cleaned_trimmed.csv")

# Use multicore to speed up process
c = detectCores()
# avoid overload buy only using maximum - 1 cores available
cores_to_use <- makeCluster(c[1]-1)
# register cores
registerDoParallel(cores_to_use)
# Time the running of the function
start <- proc.time()

# The for loop uses parallel to convert the time from UTC to Local Irish Time
tweets$Datetime = foreach(i=1:length(tweets$Datetime), .packages = "lubridate") %dopar% {
  time_in_UTC = lubridate::ymd_hms(tweets$Datetime[i])
  converted_to_IST = lubridate::with_tz(time_in_UTC, "Europe/Dublin")
  tweets$Datetime[i] = converted_to_IST
}
#stop cluster
stopCluster(cores_to_use)
# Subtract time to find out how long it took to run
time_it_took <- proc.time()-start
time_it_took
# user  system elapsed 
# 184.00   23.39  345.57 

# convert list of dates into a vector by using do.call to keep date and time name instead of displaying as numeric
tweets$Datetime = do.call("c", tweets$Datetime)

# write to file for back up
write.csv(tweets, "tweets_time_cleaned_trimmed_time_converted.csv", row.names = FALSE)


#### Convert Stocks to Irish time

# get to the folder where the stock *.csv are stored
stock_folder = paste0(location,"/stocks")
setwd(stock_folder)

# Create a list of the files in this folder that have .csv
list_of_files = list.files(pattern="*.csv")

# Use multicore to speed up process
c = detectCores()
# avoid overload buy only using maximum - 1 cores available
cores_to_use <- makeCluster(c[1]-1)
# register cores
registerDoParallel(cores_to_use)
# Time the running of the function
start <- proc.time()

# this for loop goes through each stock.csv in the list_of_files
for (stock in 1:length(list_of_files)) {
  
  # Assign the stock just read as stocks
  stocks = read.csv(list_of_files[stock])
  
  # Using a for loop with the package lubridate. Convert the time from America/New York to local Irish Time
  stocks$time = foreach(i=1:length(stocks$time), .packages = "lubridate") %dopar% {
    tz_assigned = as.POSIXct(stocks$time[i], tz="America/New_York")
    tz_converted = lubridate::with_tz(tz_assigned, "Europe/Dublin")
  }
  
  # String manipulation with the stock name in order to save the stock into a new file name
  stock_file_update = str_split(list_of_files[stock], pattern = "\\.")
  
  # the do.call function concatenates using c. It allows us to unlist the converted time
  # in order for it to be written in csv
  stocks$time = do.call("c", stocks$time)
  stock_file_update = unlist(stock_file_update)
  
  # Create the file name for the stock from the split string
  stock_rewritten = as.character(paste0(stock_file_update[1], "_updated.", stock_file_update[2]))
  
  # add a column of the stock ticker name e.g. "MSFT" is the value listed if the stock being processed is Microsoft
  stocks$stock = stock_file_update[1]
  
  # write file for back up
  write.csv(stocks, stock_rewritten, row.names = FALSE)
}

#stop cluster
stopCluster(cores_to_use)
# Subtract time to find out how long it took to run
time_it_took <- proc.time()-start
time_it_took

#### Pseudonymisation of twitter usernames ####
# set location
setwd(location)

# read file
tweets = read.csv("tweets_time_cleaned_trimmed_time_converted.csv")

# This converts the names using algorithm crc32
pseudonymize_names = sapply(tweets$Username, digest, algo="crc32", serialize=FALSE)

# change names of username to the converted ones
tweets$Username = pseudonymize_names

# save the file
write.csv(tweets, "tweets_cleaned_pseudonymized.csv", row.names = FALSE)

#### Combine all updated stock CSVs as one big CSV called stocks.csv ####

# get to the folder where the stock *.csv are stored
stock_folder = paste0(location,"/stocks")
setwd(stock_folder)


#   time        open        high         low       close      volume       stock 
# "character"   "numeric"   "numeric"   "numeric"   "numeric"   "integer" "character" 
# Create a list of the files in this folder that have "_updated.csv"
stocks_combined = data.frame(time = character(),
                    open = numeric(),
                    high = numeric(),
                    low = numeric(),
                    close = numeric(),
                    volume = integer(),
                    stock = character())

files_to_combine = list.files(pattern="*_updated.csv")
stocks = read.csv("F_updated.csv", colClasses = c("character", "numeric", "numeric", "numeric", 
                                                  "numeric", "integer", "character"))
stocks$stock = as.character(stocks$stock)
stocks$stock = c('F')
write.csv(stocks, "F_updated.csv", row.names = FALSE)



class(stocks$time[1])

stocks$time = ymd_hms(stocks$time,tz="Europe/Dublin")
stocks_tibbletime = as_tbl_time(stocks, index = time)

class(stocks$time)
head(stocks$time)
class(stocks_tibbletime)
head(stocks_tibbletime)
NROW(stocks_tibbletime)
stocks %>% filter(time ~ "2021-07")

test = filter_time(stocks_tibbletime, ~'2015-06')

test = stocks_tibbletime %>% as_tibble %>% mutate(index=seq(n())) %>% 
  arrange(time)
head(test)

head(stocks_tibbletime)
stocks_tibbletime %>%
  filter_time(~"2021-06")
write.csv(stocks_tibbletime, "_TEST.csv", row.names = FALSE)


for (i in 1:length(files_to_combine)) {
  stock_name = 
  stocks = read.csv(files_to_combine[i], colClasses = c("character", "numeric", "numeric", "numeric", 
                                                        "numeric", "integer", "character"))
  print(paste0(i, " FILE"))
  print(paste0(sapply(stocks, class)))
}

ydm_hm()



for (stock in 1:length(files_to_combine)) {
  # Assign the stock just read as stocks
  stocks = read.csv(files_to_combine[stock], colClasses = c("character", "numeric", "numeric", "numeric", 
                                                              "numeric", "integer", "character"))
  stocks_combined = merge.data.frame(stocks_combined, stocks_combined, all = TRUE)
  
  stocks_updated <- data.frame(lapply(stocks, is.character), stringsAsFactors=FALSE)
  
  # String manipulation with the stock name in order to save the stock into a new file name
  stock_file_update = str_split(list_of_files[stock], pattern = "\\.")
  
  # the do.call function concatenates using c. It allows us to unlist the converted time
  # in order for it to be written in csv
  stocks$time = do.call("c", stocks$time)
  stock_file_update = unlist(stock_file_update)
  
  # Create the file name for the stock from the split string
  stock_rewritten = as.character(paste0(stock_file_update[1], "_updated.", stock_file_update[2]))
}

#stocks= data.frame(lapply(stocks, as.character), stringsAsFactors=FALSE)

col_to_update <- sapply(stocks, is.character)
stocks[col_to_update] <- lapply(stocks[col_to_update], as.character)

sapply(stocks, class)

stocks <- list.files(path=stock_folder, pattern="*_updated.csv", full.names = TRUE) %>% 
  lapply(read_csv, colClasses = c("character", "numeric", "numeric", "numeric", 
                                  "numeric", "integer", "character") ) %>% 
  bind_rows 

for (i in 1:length(files_to_combine)) {
  stocks = read.csv(files_to_combine[i], colClasses = c("character", "numeric", "numeric", "numeric", 
                                                            "numeric", "integer", "character"))
  stocks_combined = rbind(stocks_combined, stocks)
}

write.csv(stocks_combined, "stocks.csv", row.names = FALSE)

tbl <- stocks %>% 
  transmute(time = as.POSIXct(strptime(date,"%Y/%m/%d %H:%M:%S")))
