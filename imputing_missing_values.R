
library("lubridate")

setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format="%Y-%m-%d"))


# Generate a line graph that shows the average daily activity over each 
# five-minute interval.
generateImputedStepsPerDay <- function()
{
    data <- read.csv("activity.csv", 
                     header=TRUE, 
                     colClasses=c("numeric", "myDate", "numeric"), 
                     comment.char="")
    
    stepsByInterval <- aggregate(data$steps, by=data[c("interval")], FUN=mean, na.rm=TRUE)
    data[, 1] <- apply(data, 1, lookupAverageStepsForInterval, stepsByInterval)
    
    stepsByDay <- aggregate(data$steps, by=data[c("date")], FUN=sum, na.rm=TRUE)
    
    # Remove days with no observations
    #stepsByDay <- stepsByDay[stepsByDay[,2] > 0, ]
    
    hist(stepsByDay[, 2], 
         breaks=20,
         main="Frequency of Daily Total Step Counts ",
         xlab="Total Steps",
         col="#daf0dd")
    
    print(paste("Mean number of steps taken per day = ", mean(stepsByDay[, 2])))
    print(paste("Median number of steps taken per day = ", median(stepsByDay[, 2])))
}

lookupAverageStepsForInterval <- function(rowData, lookupTable)
{
    if (is.na(rowData[1]))
    {
        return(lookupTable[lookupTable$interval == as.numeric(rowData[3]), 2])
    }
    else
    {
        return(as.numeric(rowData[1]))
    }
}
