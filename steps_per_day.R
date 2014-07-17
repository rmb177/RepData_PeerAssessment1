
library("lubridate")

setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format="%Y-%m-%d"))

# Generate a histogram showing the frequency of ranges of steps per day.
# Also print out the mean and median number of steps taken per day
generate_steps_per_day_data <- function()
{
    data <- read.csv("activity.csv", 
                     header=TRUE, 
                     colClasses=c("numeric", "myDate", "character"), 
                     comment.char="")
    
    stepsByDay <- aggregate(data$steps, by=data[c("date")], FUN=sum, na.rm=TRUE)
    
    # Remove days with no observations
    stepsByDay <- stepsByDay[stepsByDay[,2] > 0, ]
    
    hist(stepsByDay[, 2], 
         breaks=20,
         main="Frequency of Daily Total Step Counts ",
         xlab="Total Steps",
         col="#daf0dd")
    
    print(paste("Mean number of steps taken per day = ", mean(stepsByDay[, 2])))
    print(paste("Median number of steps taken per day = ", median(stepsByDay[, 2])))
}
    