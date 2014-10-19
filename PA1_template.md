---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Reproducible Research: Peer Assessment 1

**Adding R Libraries**

```r
library(ggplot2)
library(knitr)
```

## Loading and preprocessing the data
1.Load the data (i.e. read.csv())    

*I read the data using read.csv after unzipping from file.*

2.Process/transform the data (if necessary) into a format suitable for your analysis

*While reading the data using read.csv, I used specific classes to read the column data. I have shown the summary of the data here.*

```r
activitydata <- read.csv( unzip("activity.zip"),sep=",",na.strings = "NA",colClasses =c("numeric","Date","numeric"))
summary(activitydata)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```
## What is mean total number of steps taken per day?
For this part of the assignment, the missing values in the dataset are ignored. 

*Missing values were ignored and not made zero. For e.g. 10-01-12 had NA for all intervals.*

1.Make a histogram of the total number of steps taken each day 

*Calculated total number of steps taken each day by using tapply and then plotted histogram.*

2.Calculate and report the mean and median total number of steps taken per day

*Mean and Median were computed and reported.*

```r
activitydata_noNA<-activitydata[which(!is.na(activitydata$steps)),]
steps_perday<-tapply(activitydata_noNA$steps, activitydata_noNA$date, sum)
hist(steps_perday, breaks = 15, xlab="Number of Steps", main="Histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
#Mean total number of steps taken per day:
stepsperday_mean <-mean(steps_perday, na.rm = TRUE)
stepsperday_mean
```

```
## [1] 10766
```

```r
#Median total number of steps taken per day:
stepsperday_median<- median(steps_perday,na.rm=TRUE)
stepsperday_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1.Time series plot (i.e. type = “l” ) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

*Plot of average daily activity pattern shown here.*

```r
steps_avgdaily <- tapply(activitydata_noNA$steps,activitydata_noNA$interval,mean)
plot(y = steps_avgdaily, x = names(steps_avgdaily), type = "l", xlab = "5-Minute-Interval", 
    main = "Plot of average daily activity pattern", ylab = "Average number of steps (averaged across all days)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

*Identified the 5-minute interval on average across all the days in the dataset that contains the maximum number of steps.*

```r
steps_avgdaily[steps_avgdaily==max(steps_avgdaily)]
```

```
##   835 
## 206.2
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA ). The presence of missing days may introduce bias into some calculations or summaries of the data. 

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA s)

*Calculate number of NAs in "steps" variable.*


```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5- minute interval, etc.Create a new dataset that is equal to the original dataset but with the missing data filled in.

*For this project, I have used mean for that 5-minute interval to fill in for the missing values in the dataset.*


```r
activitydata_fill <- activitydata
activitydata_fill[which(is.na(activitydata_fill$steps)),1]<-
        steps_avgdaily[as.character(activitydata_fill[which(is.na(activitydata_fill$steps)),3])]
```

Make a histogram of the total number of steps taken each day

*Calculated total number of steps taken each day of the filled dataset and showed the histogram.*


```r
dailysteps_fill <- tapply(activitydata_fill$steps,activitydata_fill$date,function(x) sum(x,na.rm=TRUE))
hist(dailysteps_fill, breaks = 15, xlab="Number of Steps", main="Histogram of the total number of steps taken each day (filled dataset)")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Calculate and report the mean and median total number of steps taken per day. 

*Calculated the median and the mean of the filled in dataset.*


```r
mean(dailysteps_fill)
```

```
## [1] 10766
```

```r
median(dailysteps_fill)
```

```
## [1] 10766
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

*The median minimally changed due to imputing of missing data.*


```r
mean(dailysteps_fill)-mean(steps_perday)
```

```
## [1] 0
```

```r
median(dailysteps_fill)-median(steps_perday)
```

```
## [1] 1.189
```

## Are there differences in activity patterns between weekdays and weekends?

**The dataset with the filled-in missing values is used.**

1.A new factor variable is created in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. 

2.A panel plot containing a time series plot (i.e. type = “l” ) of the 5-minute interval (x-axis) and the average number of steps taken is constructed, averaged across all weekday days or weekend days (y-axis).

*From the two graphs, we can clearly see that the distribution throughout the day is quite different for weekday vs. weekend. One simple conclusion from the plot is that, the individual seems to wake up at least one hour later at the weekends.*


```r
daytype <- function(date) {
        if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
                "Weekend"
        } else {
                "Weekday"
        }
}
activitydata_fill$daytype <- as.factor(sapply(activitydata_fill$date, daytype))
activitydata_fill$day <- sapply(activitydata_fill$date, FUN = daytype)

averages <- aggregate(steps ~ interval + day, data = activitydata_fill, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
