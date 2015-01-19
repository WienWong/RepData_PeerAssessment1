
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

class(act$steps)
# "integer"
class(act$date)
# "factor"
class(act$interval)
# "integer"

# act$month <- as.numeric(format(act$date, "%m"))
# 
# noNAact <- na.omit(act)
# 
# rownames(noNAact) <- 1:nrow(noNAact) # To set rownames in the data frame 
# 
# head(noNAact)
# 
# class(noNAact$steps)
# 
# class(noNAact$date)
# 
# class(noNAact$interval)
# 
# class(noNAact$month)

# Make a histogram of the total number of steps taken each day
steps_each_day <- aggregate(steps ~ date, act, FUN = sum)

hist(steps_each_day$steps, breaks = 20, main = "Total Steps Each Day", col = "blue", xlab = "Steps")

mean(steps_each_day$steps)

median(steps_each_day$steps)


steps_each_interval <- aggregate(steps ~ interval, act, mean)

plot(steps_each_interval$interval, steps_each_interval$steps, type = "l", xlab = "5-minute Interval", ylab = "Average Number of Steps", main = "Time Series Plot", col = "blue")

steps_each_interval[which.max(steps_each_interval$steps), ]
#      interval    steps
# 104      835   206.1698

noNAact <- na.omit(act)

nrow(act) - nrow(noNAact)
# 2304

knitr::knit2html("PA1_template.Rmd")
