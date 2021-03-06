---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Read in the activity data file (note that "activity.csv" is already located in the working directory) and convert dates to R's date format.

```r
#read in data
activityData <- read.csv("activity.csv")
#process dates
activityData$date <- as.Date(activityData$date)
```


## What is mean total number of steps taken per day?

Calculate total number of steps per day.

```r
sumData <- tapply(activityData$steps, activityData$date, sum)
```

Produce histogram from data.

```r
hist(sumData, xlab="Number of Steps per day", main="Histogram of Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate mean and median.

```r
#calculate mean
meanSteps <- mean(sumData, na.rm = TRUE)
#calculate median
medianSteps <- median(sumData, na.rm = TRUE)
```

The mean number of steps taken per day was 1.0766189\times 10^{4}, and the median number of steps taken per day was 10765.
Note here that because some days in the data have had no step values recorded (all NAs), these have been ignored when calculating the average.

## What is the average daily activity pattern?

Calculate average number of steps per 5-minute interval (ignoring NAs), change intervals from chr to numeric and then plot.

```r
timeAvgData <- tapply(activityData$steps, activityData$interval, mean, na.rm=TRUE, simplify=FALSE)
names(timeAvgData) <- as.numeric(names(timeAvgData))
plot(x=names(timeAvgData), y=timeAvgData, type="l", xlab="5-minute Intervals", ylab="Average Steps", main="Average Steps per 5-minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Then calculate location of maximum.

```r
maxAvgLoc <- names(which.max(timeAvgData))
maxAvg <- max(unlist(timeAvgData))
```

The 5-minute interval with the maximum number of steps is 835, with 206.1698113 steps.

## Imputing missing values

First determine number of NAs.

```r
bad <- is.na(activityData$steps)
countNA <- sum(bad)
```

There are 2304 missing values in the dataset.

Missing values in the dataset will be filled using the mean for that particular 5 minute interval.

Copy dataset, calculate mean and replace appropriately.

```r
imputedData <- activityData
imputedData$steps[is.na(imputedData$steps)] <- ave(imputedData$steps, imputedData$interval, FUN=function(x)mean(x, na.rm=TRUE))[is.na(imputedData$steps)]
```

Then calculate new total number of steps each day, and create histogram.

```r
#calculate total number of steps each day (for imputed data)
sumDataNew <- tapply(imputedData$steps, imputedData$date, sum)
#create histogram
hist(sumDataNew, xlab="Number of Steps per day", main="Histogram of Daily Steps (imputed data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

And calculate new mean and median.

```r
#calculate mean of imputed
meanStepsNew <- mean(sumDataNew, na.rm = TRUE)
#calculate median of imputed
medianStepsNew <- median(sumDataNew, na.rm = TRUE)
```

In the imputed data, the mean number of steps taken per day was 1.0766189\times 10^{4} (compared to 1.0766189\times 10^{4}), and the median number of steps taken per day was 1.0766189\times 10^{4} (compared to 10765).

As shown in the figures above, filling in the missing values using the mean for that particular 5 minute interval will increase the median number of steps taken each day, but will not affect the mean number of steps taken each day. This is because when calculating the initial mean (from the non-imputed data), days that had no step measurements at all were dropped from the number of days when calculating the average (as otherwise it would skew the first average lower than it should be)
In addition, imputing the missing data will also increase the estimates of the total daily number of steps, as values that were previously NA were replaced with values that were greater than or equal to 0.

## Are there differences in activity patterns between weekdays and weekends?

Note: the following code requires the "lattice" plotting system!


```r
#load lattice, make sure that it's installed!
library(lattice)
```

Create a factor variable for weekday/weekend, and add it to the imputed data calculated previously.

```r
#find day names
dayname <- weekdays(imputedData$date)
#convert to weekend/weekday
dayname[grepl("S(aturday|unday)", dayname)] = "weekend"
dayname[grepl("Monday|Tuesday|Wednesday|Thursday|Friday", dayname)] = "weekday"
#create factor variable
dayname <- factor(dayname)
#add factor to data
imputedData$dayname <- dayname
```

Then, calculate average number of steps taken per 5 minute interval, on weekdays compared to weekends - and create an appropriate panel plot.

```r
timeAvgDataWeekday <- aggregate(list(Steps = imputedData$steps), list(Interval = imputedData$interval, Day = imputedData$dayname), FUN = mean)
xyplot(Steps ~ Interval | Day, data = timeAvgDataWeekday, type="l", layout = c(1, 2), main="Activity Pattern Comparison between Weekdays and Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


