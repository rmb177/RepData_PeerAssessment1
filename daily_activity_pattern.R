
library("lubridate")

setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format="%Y-%m-%d"))


# Generate a line graph that shows the average daily activity over each 
# five-minute interval.
generateDailyActivityData <- function()
{
    data <- read.csv("activity.csv", 
                     header=TRUE, 
                     colClasses=c("numeric", "myDate", "numeric"), 
                     comment.char="")
    
    stepsByInterval <- aggregate(data$steps, by=data[c("interval")], FUN=mean, na.rm=TRUE)
    plot(stepsByInterval[, 1], stepsByInterval[, 2], type="l")
    
    maxIndex = which.max(stepsByInterval[, 2])
    print(paste("The interval with the maximum average number of steps taken is interval",
                 stepsByInterval[maxIndex, 1],
                 "with the average number of steps =",
                 stepsByInterval[maxIndex, 2]))
    
    weekend = c("Saturday", "Sunday")
    data[, "time.of.week"] <- sapply(data[, 2], function(x) if (is.element(weekdays(x), weekend)) as.factor('weekend') else as.factor('weekday'))
    weekdaysData <- data[data$time.of.week == "weekday", ]
    weekendsData <- data[data$time.of.week == "weekend", ]
    
    par(mfrow = c(2, 1))
    par(mar = c(3, 3, 3, 3))
    
    stepsByIntervalWeekends <- aggregate(weekendsData$steps, by=weekendsData[c("interval")], FUN=mean, na.rm=TRUE)
    plot(stepsByIntervalWeekends[, 1], 
         stepsByIntervalWeekends[, 2], 
         type="l",
         main="Mean Step Count by Interval (Weekends)",
         xlab="",
         ylab="",
         col="#498dff",
         cex.main=0.75,
         cex.axis=0.75,
         ylim=c(0, 250))
    
    stepsByIntervalWeekdays <- aggregate(weekdaysData$steps, by=weekdaysData[c("interval")], FUN=mean, na.rm=TRUE)
    plot(stepsByIntervalWeekdays[, 1], 
         stepsByIntervalWeekdays[, 2],
         type="l",
         main="Mean Step Count by Interval (Weekdays)",
         xlab="",
         ylab="",
         col="#498dff",
         cex.main=0.75,
         cex.axis=0.75,
         ylim=c(0, 250))
    
    mtext("Interval", side=1, padj=-2, outer=TRUE, cex=0.75)
    mtext("Mean Step Count", side=2, padj=2, outer=TRUE, cex=0.75)
}

