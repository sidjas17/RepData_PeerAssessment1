**Reproducible Research Assignment 1 **

===================================================

**Loading and processing the data.**


Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis

```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```

**What is mean total number of steps taken per day ?**

Make a histogram of the total number of steps taken each day.

Calculate and report the mean and median total number of steps taken per day

```r
steps1 <- tapply(data$steps,data$date,sum,na.rm=TRUE)
library(ggplot2)
qplot(steps1,xlab = "Number of steps taken each day.",ylab = "Total Frequency",binwidth = 500)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean and median of total number of steps taken each day.


```r
mean(steps1)
```

```
## [1] 9354.23
```

```r
median(steps1)
```

```
## [1] 10395
```

**What is the average activiy pattern ?**

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average <- tapply(data$steps,data$interval,mean,na.rm = TRUE)

plot(names(average),average ,xlab = "5-min interval",type = "l",ylab = "Average number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
maxavg <- max(average)
maxinterval <- as.numeric(names(average)[which(average==max(average))])
```

**Imputing missing values.**

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
totalna <- sum(is.na(data$steps))

imputedata <- data
imputedata$steps[which(is.na(data$steps))]<- as.vector(average[as.character(data[which(is.na(data$steps)),3])])
library(ggplot2)
stepseachday<- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#Calculating mean and median.
mean(stepseachday)
```

```
## [1] 10766.19
```

```r
median(stepseachday)
```

```
## [1] 10766.19
```

**Are there differences in activity patterns between weekdays and weekends ?**


```r
imputedata$dayType <- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6),"weekends","weekdays")
aggregatedata <- aggregate(steps~interval+ dayType,data=imputedata,mean)
ggplot(aggregatedata,aes(interval,steps)) + geom_line()+ facet_grid(dayType ~ .)  + xlab("5-minute interval") +ylab("Average Number Of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



