## ------------------------------------------------------------------------
# Conditionally download and unzip the data file required
if(!file.exists("activity.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip", "curl")
}
if(!file.exists("activity.csv")) {
    unzip("activity.zip")
}
##Load the data (i.e. read.csv())
rawactivity<-read.csv("activity.csv")

# once we read in the raw data, we won't need it again
unlink("activity.csv")

## ------------------------------------------------------------------------
##Process/transform the data (if necessary) into a format suitable for your analysis
# convert 'date' from charater type to date type
activity<-rawactivity
activity$date<-as.Date(activity$date)

## ------------------------------------------------------------------------
str(activity)

## ------------------------------------------------------------------------
# For this part of the assignment, you can ignore the missing values in the dataset.
activity<-na.omit(activity)
# Use aggragate to sum over the number of steps for each date, omitting incomplete observations
stepsbydate <- aggregate(steps ~ date, activity, sum)

## ------------------------------------------------------------------------
str(stepsbydate)

## ------------------------------------------------------------------------
##Make a histogram of the total number of steps taken each day
hist(stepsbydate$steps, main="Histogram of total number\nof steps per day", 
     xlab="Total number of steps in a day", ylab="Number of days")

## ------------------------------------------------------------------------
##Calculate and report the mean and median total number of steps taken per day
mean(stepsbydate$steps)
median(stepsbydate$steps)

## ------------------------------------------------------------------------
# redraw the histogram
hist(stepsbydate$steps, main="Histogram of total number\nof steps per day", 
     xlab="Total number of steps in a day", ylab="Number of days")

# place vertical lines for mean and median on histogram
mean_val=mean(stepsbydate$steps)
median_val=median(stepsbydate$steps)
abline(v=mean_val, col = "green")
abline(v=median_val, col = "red")

# create legend
legend('topright', lty=1,col = c("green", "red"),
legend = c(paste('Mean: ', round(mean_val,digits=2)), paste('Median: ', median_val))
)

## ------------------------------------------------------------------------
# free memory of objects no longer required
rm(stepsbydate, mean_val, median_val)

## ------------------------------------------------------------------------
stepsbyinterval <- aggregate(steps ~ interval, activity, mean)

## ------------------------------------------------------------------------
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(stepsbyinterval$interval, stepsbyinterval$steps, type = "l", main="Average number of steps", 
     xlab="Interval", ylab="Number of steps")

## ------------------------------------------------------------------------
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
stepsbyinterval[which.max(stepsbyinterval$steps),]

## ------------------------------------------------------------------------
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(stepsbyinterval$interval, stepsbyinterval$steps, type = "l", main="Average number of steps", 
     xlab="Interval", ylab="Number of steps")
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval=stepsbyinterval[which.max(stepsbyinterval$steps),]
# place vertical lines for mean and median on histogram
abline(h=max_interval$steps, col = "purple")
abline(v=max_interval$interval, col = "blue")

# create legend to say what the line represents
legend('topright', lty=1, col = c("purple","blue"),
legend = c(paste('Maximum steps: ', round(max_interval$steps,digits=2)),paste('Interval: ', round(max_interval$interval,digits=2)))
)

## ------------------------------------------------------------------------
# free memory of objects no longer required
rm(stepsbyinterval)
rm(activity)
rm(max_interval)

## ------------------------------------------------------------------------
sum(!complete.cases(rawactivity))

## ------------------------------------------------------------------------
# make a copy of the original dataset, and also convert the date
activity<-rawactivity
activity$date<-as.Date(activity$date)

# fill in the missing values by averaging those around it 
for (i in 1:nrow(activity)){
    if(is.na(activity$steps[i])){
        #if this is the first interval, use the value of the first that is not na
        j<-i+1
        while (j<=nrow(activity) && is.na(activity$steps[j])) {j<-j+1}
        # we're assuming we have at least one that is not NA 
        # otherwise we're going to use the average of two NAs
        if (i == 1) {
            # actually, we could take the one at the average from the other end of the day
            interpolation=activity$steps[j]
        } else if (j > nrow(activity)) {
            # actually, we could take the one at the average from the other end of the day
            interpolation=activity$steps[i-1]
        } else {
            interpolation=(activity$steps[j]+activity$steps[i-1])/2
        }
        # if there are multiple na's we should set them all equal
        # might as well fill them all in while we're here
        activity$steps[i:j-1]=interpolation
        print(paste("NAs in rows",i,":",j-1," (",(j-i)*5/60,"hours long ) have been set to",interpolation))
    }
}
# clean up
rm (i,j,interpolation)

## ------------------------------------------------------------------------
##Process/transform the data (if necessary) into a format suitable for your analysis
###Use aggragate to sum over the number of steps for each date, note that there will be no omitted observations
stepsbydate <- aggregate(steps ~ date, activity, sum)

## ------------------------------------------------------------------------
##Make a histogram of the total number of steps taken each day
hist(stepsbydate$steps, main="Histogram of total (imputed) number\nof steps per day", 
     xlab="Total number of steps in a day", ylab="Number of days")

## ------------------------------------------------------------------------
##Calculate and report the mean and median total number of steps taken per day
mean(stepsbydate$steps)
median(stepsbydate$steps)

## ------------------------------------------------------------------------
# redraw the histogram
hist(stepsbydate$steps, main="Histogram of total number\nof steps per day", 
     xlab="Total number of steps in a day", ylab="Number of days")

# place vertical lines for mean and median on histogram
mean_val=mean(stepsbydate$steps)
median_val=median(stepsbydate$steps)
abline(v=mean_val, col = "green")
abline(v=median_val, col = "red")

# create legend
legend('topright', lty=1,col = c("green", "red"),
legend = c(paste('Mean: ', round(mean_val,digits=2)), paste('Median: ', median_val))
)
# free memory of objects no longer required
rm(stepsbydate, mean_val, median_val)

## ------------------------------------------------------------------------
# the date column is actually string data and we need to convert it to date to use weekdays()
# then we need to convert the "weekday"/"weekend" strings back into factors
activity$day<-as.factor(ifelse(weekdays(activity$date) == "Sunday" | weekdays(activity$date) == "Saturday","weekend", "weekday"))

## ------------------------------------------------------------------------
str(activity)

## ------------------------------------------------------------------------
# aggregate steps as interval to get average number of steps in an interval across day types
stepsbyintervalandday <- aggregate(steps ~ interval+day, activity, mean)

# ggplot and qplot are easy to use
# conditional install and load the package
if (! "ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2")
library(ggplot2)

# create two plots in a single column - using the day type as a facet
qplot(interval, steps, data=stepsbyintervalandday, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ day, ncol=1)

# cleanup
rm(activity)
rm(stepsbyintervalandday)

## ------------------------------------------------------------------------
##Process/transform the data (if necessary) into a format suitable for your analysis
# convert 'date' from charater type to date type
activity<-na.omit(rawactivity)
activity$date<-as.Date(activity$date)
# the date column is actually string data and we need to convert it to date to use weekdays()
# then we need to convert the "weekday"/"weekend" strings back into factors
activity$day<-as.factor(ifelse(weekdays(activity$date) == "Sunday" | weekdays(activity$date) == "Saturday","weekend", "weekday"))
stepsbyintervalandday <- aggregate(steps ~ interval+day, activity, mean)

# conditional install and load the package
if (! "ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2")
library(ggplot2)

# create two plots in a single column - using the day type as a facet
qplot(interval, steps, data=stepsbyintervalandday, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ day, ncol=1)
rm(activity)
rm(stepsbyintervalandday)

## ------------------------------------------------------------------------
# final cleanup
rm(rawactivity)

