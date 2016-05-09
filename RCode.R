################################################################################
# Loading and preprocessing the data Show any code that is needed to:
# 1. Load the data (i.e. read.csv())
# 2. Process/transform the data (if necessary) into a format suitable for your
# analysis
################################################################################

# Set some variables concerned about the enviroment
library(dplyr)


workingdirectory <- "~/R/RepData_PeerAssessment1"
zipfile.name <- "activity.zip"
csvfile.name <- "activity.csv"
files.path <- workingdirectory
zipfile.path <- paste(files.path, zipfile.name, sep = "/")
csvfile.path <- paste(files.path, csvfile.name, sep = "/")

setwd(workingdirectory)

# Check if zipfile is where expected
if (!file.exists(zipfile.path)) {
    stop(
        paste(
            "Message: The script couldn't continous because the data file was not found at '",
            zipfile.path, "'", ". Solution: place the ", zipfile.name,
            " (the one that contains the data source for this script) at ",
            files.path, " directory and try again", sep = ""
        )
    )
}

# Unzip if csv file doesn't exists
if (!file.exists(csvfile.path)) {
    unzip(zipfile.path)
}

# Read the data
activity.rawdata <-
    read.csv(
        file = csvfile.path, header = TRUE, sep = ",", stringsAsFactors = FALSE
    )
activity.data <- activity.rawdata

# Converts steps columns into number values
if (class(activity.data$steps) != "integer") {
    activity.data$steps <- as.numeric(activity.data$steps)
}

# Converts the date column into date values
if (class(activity.data$date) != "Date") {
    activity.data$date <-
        as.Date(x = as.character(activity.data$date), format = "%Y-%m-%d")
}

# Converts the interval column into number values
if (class(activity.data$steps) != "integer") {
    activity.data$steps <- as.numeric(activity.data$steps)
}

################################################################################
# Some informations about the dataset
# * structure (variables and observations, columns and data type of each)
# * Number of NAs at each column
################################################################################

# Data structure info
str(activity.data)

# Percental of NA at each column
sapply(
    activity.data, FUN = function(x) {
        mean(is.na(x))
    }
)

################################################################################
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the
# dataset.
# 1. Make a histogram of the total number of steps taken each day
# 2. Calculate and report the mean and median total number of steps taken per
# day
################################################################################
library(plyr)

# Create a new dataset with no NA
activity.naignored <- activity.data[!is.na(activity.data$steps),]

# Summarises the total, mean and media daily number os steps
totalbyday.naignored <- ddply(
    activity.naignored, 
    .(date),
    summarise,
    total = sum(steps)
)

# Plots the histogram
hist(totalbyday.naignored$total,
     main = "Frequency of days by its number of steps", 
     xlab = "Number of the steps in one day",
     ylab = "Frequency of days"
)

# Adds a vertical blue line that marks the daily average number of steps
abline(v = mean(totalbyday.naignored$total), lwd = 3, col = "blue")

# mean daily number of steps
mean(totalbyday.naignored$total)

# median daily number of steps
median(totalbyday.naignored$total)

################################################################################
# What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
################################################################################

# Calculate the mean number of steps of each 5-minute interval across all days
meanbyinterval.naignored <- ddply(
    activity.naignored, 
    .(interval), 
    summarise, 
    mean = mean(steps)
)

#Plot the graph with the average number of steps for each 5-minutes interval
plot(
    x = meanbyinterval.naignored$interval, 
    y = meanbyinterval.naignored$mean, 
    type = "l", 
    xlab = "Interval", 
    ylab = "Number of steps", 
    main = "Average number of steps Vs day interval"
)

# Gets the 5-minutes interval which has the maximum average number of steps
# across all days
meanbyinterval.naignored[
    meanbyinterval.naignored$mean == max(meanbyinterval.naignored$mean),]

################################################################################
# Imputing missing values
# Note that there are a number of days/intervals where there are missing values
# (coded as NA). The presence of missing days may introduce bias into some
# calculations or summaries of the data.
# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
# 2. Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could use the
# mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
# 4. Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?
################################################################################

# Calculate the number of rows with missing values
sum(!complete.cases(activity.data))

# Replace NA values by interval mean
activity.nafilled <- activity.data
for (i in 1:nrow(activity.nafilled)) {
        #Get the step value
        step.value <- activity.nafilled[i,"steps"]
        if (is.na(step.value)) {
            #Get the step interval
            step.interval <- activity.nafilled[i, "interval"]
            
            #Repacle step value from na by the mean step value at the correspond
            #interval
            activity.nafilled[i,"steps"] <- 
                meanbyinterval.naignored[
                    meanbyinterval.naignored$interval == step.interval, "mean"]
        }
}
rm(step.value, step.interval)

# Calculate the total by day from activity.nafilled
totalbyday.nafilled <- ddply(
    activity.nafilled, 
    .(date),
    summarise,
    total = sum(steps)
)

# Plots the histogram
hist(totalbyday.nafilled$total,
     main = "Frequency of days by its number of steps", 
     xlab = "Number of the steps in one day",
     ylab = "Frequency of days"
)

# Adds a vertical blue line that marks the daily average number of steps
abline(v = mean(totalbyday.nafilled$total), lwd = 3, col = "blue")

# mean daily number of steps
mean(totalbyday.nafilled$total)

# median daily number of steps
median(totalbyday.nafilled$total)

# Plots a boxplot graph so it enables a visually comparison between ignored and 
# filled NAs values
totalbyday.nafilled <- cbind(totalbyday.nafilled, datasource = "na.filled")
totalbyday.naignored <- cbind(totalbyday.naignored, datasource = "na.ignored")
totalbyday.union <- rbind(totalbyday.nafilled, totalbyday.naignored) %>% 
    data.frame()
boxplot(
    total ~ datasource, 
    totalbyday.union, 
    main = "Total daily steps from Filled Vs Ignored NAs")

cbind(
    na.filled = summary(totalbyday.nafilled$total), 
    na.ignored = summary(totalbyday.naignored$total)
)

################################################################################
# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the 
# dataset with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels – “weekday” and
# “weekend” indicating whether a given date is a weekday or weekend day.
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). The plot should look 
# something like the following, which was creating using simulated data:
# Your plot will look different from the one above because you will be using 
# the activity monitor data. Note that the above plot was made using the 
# lattice system but you can make the same version of the plot using any 
# plotting system you choose.
################################################################################

# Add the weekday (sun-sat) and weekdaytype (weekday or weekend) columns
activity.weekday <- cbind(
    activity.nafilled, 
    weekday = sapply(activity.nafilled$date, weekdays),
    weekdaytype = sapply(activity.weekday$weekday, function(x){
        if(as.numeric(x) %in% c(2,3,5,6,7) ){
            "weekday"
        }else{
            "weekend"
        }
    }))

# Calculate the mean number os steps by each 5-minutes interval
total.byintervalandweekday <- ddply(
    activity.weekday, 
    .(interval, weekdaytype),
    summarise,
    mean = mean(steps)
)

# Plot hte values by
library(ggplot2)
ggplot(
    data = total.byintervalandweekday, 
    aes(x = interval, y = mean, color = factor(weekdaytype)),
    xlab = "Interval",
    ylab = "Mean number of steps"
) + geom_line()  + scale_color_hue(name = "Legenda") + ggtitle(label = "Mean number of steps by interval - Weekend Vs Weekday")

