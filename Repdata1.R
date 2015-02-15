
echo = TRUE # make all code available to read

getwd()

unzip("repdata-data-activity.zip")

list.files()

act <- read.csv("activity.csv")

head(act)
#    steps      date  interval
# 1    NA 2012-10-01        0
# 2    NA 2012-10-01        5
# 3    NA 2012-10-01       10
# 4    NA 2012-10-01       15
# 5    NA 2012-10-01       20
# 6    NA 2012-10-01       25

class(act$steps); class(act$date); class(act$interval)
# "integer"
# "factor"
# "integer"

View(act)
?aggregate
# What is mean total number of steps taken per day?
steps_each_day <- aggregate(steps ~ date, act, FUN = sum, na.rm=TRUE)

direct <- paste (getwd(), "/plot111.png", sep = "", collapse = NULL)
png(filename = direct, width = 500, height = 500, units = "px")

# Make a histogram of the total number of steps taken each day
hist(steps_each_day$steps, breaks = 30, main = "Total Steps Each Day", col = "blue", xlab = "Steps")

dev.off()

mean(steps_each_day$steps); median(steps_each_day$steps)
# 10766.19
# 10765

# What is the average daily activity pattern? Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps_each_interval <- aggregate(steps ~ interval, act, FUN = mean)

direct <- paste (getwd(), "/plot112.png", sep = "", collapse = NULL)
png(filename = direct, width = 500, height = 500, units = "px")

plot(steps_each_interval$interval, steps_each_interval$steps, type = "l", xlab = "5-minute Intervals", ylab = "Average Number of Steps", main = "Time Series Plot", col = "blue")

dev.off()

# Find the 5-minute intervall contains the maximum number of steps.
steps_each_interval[which.max(steps_each_interval$steps), ]
#      interval    steps
# 104      835   206.1698

# Calculate the total number of missing values in the dataset
noNAact <- na.omit(act)
nrow(act) - nrow(noNAact)

# Or
sum(is.na(act$steps)) 
# 2304

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated.U could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Use the mean for the 5-minute interval to fill each NA value in the steps column.
new_act <- act
for (i in 1:nrow(new_act)) {
    if (is.na(new_act$steps[i])) {
        new_act$steps[i] <- steps_each_interval[which( new_act$interval[i] == steps_each_interval$interval ), ]$steps
    }
}
# Check how many NA values left.
sum(is.na(new_act))

# Make a histogram of the total number of steps taken each day
steps_each_day2 <- aggregate(steps ~ date, new_act, FUN = sum)

direct <- paste (getwd(), "/plot113.png", sep = "", collapse = NULL)
png(filename = direct, width = 500, height = 500, units = "px")

hist(steps_each_day2$steps, breaks = 30, main = "Total Steps Each Day", xlab = "Day", col = "green")

dev.off()

mean(steps_each_day2$steps); median(steps_each_day2$steps)
#  10766.19
#  10766.19 

# Are there differences in activity patterns between weekdays and weekends? Use the dataset with the filled-in missing values for this part.
class(new_act$date)

new_act$date <- as.Date(new_act$date, "%Y-%m-%d")

day <- weekdays(new_act$date)

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
daypattern <- character()

for (i in 1:nrow(new_act)) {
    if (day[i] == "Saturday" | day[i] == "Sunday")  {
        daypattern[i] <- "Weekend"
    } else {
        daypattern[i] <- "Weekday"
    }
}

new_act$daypattern <- daypattern
new_act$daypattern <- factor(new_act$daypattern)

stepsEachDay <- aggregate(steps ~ interval + daypattern, new_act, FUN = mean)

names(stepsEachDay) <- c("interval", "daypattern", "steps")

direct <- paste (getwd(), "/plot114.png", sep = "", collapse = NULL)
png(filename = direct, width = 500, height = 500, units = "px")

library(lattice)
xyplot(steps ~ interval | daypattern, stepsEachDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of Steps")

dev.off()

#

knitr::knit2html("PA1_template.Rmd")
