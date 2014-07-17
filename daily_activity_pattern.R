
library("lubridate")

setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format="%Y-%m-%d"))


# Generate a line graph that shows the average daily activity over each 
# five-minute interval.
generate_daily_activity_data <- function()
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
}
