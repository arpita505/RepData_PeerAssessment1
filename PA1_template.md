---
title: "Reproducible_Research_ProgAssgn1"
author: "Arpita Singh"
date: "12/27/2018"
output:  
  html_document:
    keep_md: true

---



##Reading in the dataset and processing the data



```r
activity <- read.csv("activity.csv")
```



##Histogram of the total number of steps taken each day:



```r
totalSteps <- tapply(activity$steps, activity$date, FUN=sum , na.rm = TRUE)
qplot(totalSteps, binwidth = 500, xlab = "total number of steps taken each day",ylab = "count")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


##Mean and median number of steps taken each day


```r
mean(totalSteps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(totalSteps, na.rm = TRUE)
```

```
## [1] 10395
```

##Time series plot of the average number of steps taken


```r
average <- aggregate(x = list(steps = activity$steps), by = list(interval = activity$interval), 
    FUN = mean, na.rm = TRUE)

ggplot(data=average, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


## The 5-minute interval that, on average, contains the maximum number of steps


```r
average[which.max(average$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```



## Imputing missing data


#### Calculate and report the total number of missing values in the dataset 


```r
missing <- is.na(activity$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

#### Devise a strategy for filling in all of the missing values in the dataset


```r
imputed <- activity
imputed$steps <- impute(activity$steps, fun=mean)
```



## Histogram of the total number of steps taken each day after missing values are imputed

#### Make a histogram of the total number of steps taken each day 



```r
totalStepsImputed <- tapply(imputed$steps, imputed$date, FUN=sum , na.rm = TRUE)
qplot(totalStepsImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

#### Calculate and report the mean and median total number of steps taken per day.


```r
mean(totalStepsImputed )
```

```
## [1] 10766.19
```

```r
median(totalStepsImputed)
```

```
## [1] 10766.19
```


## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
imputed$dateType <-  ifelse(as.POSIXlt(imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```


#### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
averageImputed <- aggregate(steps ~ interval + dateType, data=imputed, mean)
ggplot(averageImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



