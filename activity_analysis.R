# Script to perform the data anaylsis on the 
# activity data


# Set up date processing
library("lubridate")
setClass('myDate')
setAs("character", "myDate", function(from) as.Date(from, format="%Y-%m-%d"))

# Unzip and read in data
unzip("activity.zip")
activityData <- read.csv("activity.csv", 
                         header=TRUE, 
                         colClasses=c("numeric", "myDate", "numeric"), 
                         comment.char="")

# Sum up the total number of steps for each individual day
stepsByDay <- aggregate(activityData$steps, by=activityData[c("date")], FUN=sum, na.rm=TRUE)

# Sum up the total number of steps for each time interval
stepsByInterval <- aggregate(activityData$steps, by=activityData[c("interval")], FUN=mean, na.rm=TRUE)


################ Part 1: Generate information on daily total step counts

# Remove days without any observations
stepsByDayValidObservations <- stepsByDay[stepsByDay[,2] > 0, ]

hist(stepsByDayValidObservations[, 2], 
     breaks=20,
     main="Frequency of Daily Total Step Counts ",
     xlab="Total Steps",
     col="#daf0dd")

print(paste("Mean number of steps taken per day = ", round(mean(stepsByDayValidObservations[, 2]), 2)))
print(paste("Median number of steps taken per day = ", round(median(stepsByDayValidObservations[, 2], 2))))


################ Part 2: Generate information on interval mean step counts

plot(stepsByInterval[, 1], 
     stepsByInterval[, 2], 
     type="l",
     main="Mean Step Count by Interval",
     xlab="Interval",
     ylab="Mean Step Count",
     col="#498dff")

maxIndex = which.max(stepsByInterval[, 2])
print(paste("The interval with the maximum average number of steps taken is interval",
            stepsByInterval[maxIndex, 1],
            "with the average number of steps =",
            round(stepsByInterval[maxIndex, 2], 2)))


################ Part 3: Impute missing values
print(paste("The total number of NA values in the original data set =", sum(!is.na(activityData[, 1]))))

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

imputed_data <- data.frame(apply(activityData, 1, lookupAverageStepsForInterval, stepsByInterval), 
                           activityData[, 2], 
                           activityData[, 3])
colnames(imputed_data) <- colnames(activityData)

imputedStepsByDay <- aggregate(imputed_data$steps, by=imputed_data[c("date")], FUN=sum, na.rm=TRUE)

hist(imputedStepsByDay[, 2], 
     breaks=20,
     main="Frequency of Daily Total Step Counts ",
     xlab="Total Steps",
     col="#daf0dd")

print(paste("Mean number of steps taken per day = ", round(mean(imputedStepsByDay[, 2]), 2)))
print(paste("Median number of steps taken per day = ", round(median(imputedStepsByDay[, 2]), 2)))
