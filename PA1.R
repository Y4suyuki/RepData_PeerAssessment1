# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
raw_activity_data <- read.csv(unz('activity.zip', 'activity.csv'))

##Convert date to Date class
raw_activity_data$date <- as.Date(raw_activity_data$date)

##Remove rows steps is NA
activity_data <- raw_activity_data[!is.na(raw_activity_data$steps),]

##make data table for fast data manipulation
library(data.table)
DT <- data.table(activity_data)
## What is mean total number of steps taken per day?
##Total number of steps taken per day
library(lattice)
total_steps_per_day <- DT[, sum(steps), by=date]
histogram(total_steps_per_day$V1, title='Histogram of total steps per day', xlab='steps') 

data.frame('mean'=mean(total_steps_per_day$V1),'median'=median(total_steps_per_day$V1))


## What is the average daily activity pattern?

dap <- DT[, mean(steps), by=interval]
xyplot(dap$V1 ~ dap$interval, type='l', xlab='interval', ylab='steps')
### max 
dap[dap$V1==max(dap$V1),]

## Imputing missing values
##Note that there are a number of days/intervals where there are missing values (codes as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data

##1. Calculate and report the total number of missing values in the dataset.
table(is.na(raw_activity_data$steps))

##2. Device a strategy for filling in all missing values in the dataset.
##Let's fill NA with latest non NA value. With na.locf from zoo library, it's pretty easy

##3. Create a new dataset that is equal to the original dataset but with the missing data filled in
library(zoo)
na_filled <- na.locf(raw_activity_data)
na_filled$steps <- as.numeric(na_filled$steps)
##4. Make a histogram of the total number of steps taken each day and Calculate and report **mean** and **median** total number of steps taken per day.

na_filled_DT <- data.table(na_filled)
na_filled_tspd <- na_filled_DT[, sum(steps), by=date]
histogram(na_filled_tspd$V1, title='histogram of total steps per day with missing values filled with latest non NA values')


## Are there differences in activity patterns between weekdays and weekends?
library(reshape)
na_filled$date <- as.Date(na_filled$date)
na_filled$weekday_end <- !weekdays(na_filled$date) %in% c("Sunday", "Saturday")
mdata <- melt(na_filled, id=c('date', 'weekday_end', 'interval'))
res <- cast(mdata, interval ~ weekday_end ~ variable, function(x) mean(x, na.rm=TRUE))
res_df <- data.frame(res)
int_df <- data.frame(unique(mdata$interval))
res_df <- cbind(int_df, res_df)
we_df <- res_df[, c(1,2)]
wd_df <- res_df[, c(1,3)]
colnames(we_df) <- c('interval', 'step')
colnames(wd_df) <- c('interval', 'step')
we_df$f <- 'weekend'
wd_df$f <- 'weekday'
result <- rbind(we_df, wd_df)
result$interval <- as.numeric(as.character(result$interval))
xyplot(step ~ interval | f, data=result, type='l', layout=c(1,2))
na_filled_DT$weekday_end <- !weekdays(na_filled_DT$date) %in% c("Sunday", "Saturday")
