---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: TRUE
---




```r
library(dplyr)
library(knitr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```


## Loading and preprocessing the data

```r
data <- read.csv("./activity.csv")
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")
```



## What is mean total number of steps taken per day?

```r
# mean <- tapply(data$steps, data$date , mean, na.rm = TRUE)
totalStepsDf <- data %>%
  group_by(date) %>%
  summarise(totalSteps = sum(steps, na.rm = TRUE))
  

mean_ <- mean(totalStepsDf$totalSteps)
median_ <- median(totalStepsDf$totalSteps)
```

### The Total number of steps taken per day

```r
kable(totalStepsDf, digits = 2)
```



date          totalSteps
-----------  -----------
2012-10-01             0
2012-10-02           126
2012-10-03         11352
2012-10-04         12116
2012-10-05         13294
2012-10-06         15420
2012-10-07         11015
2012-10-08             0
2012-10-09         12811
2012-10-10          9900
2012-10-11         10304
2012-10-12         17382
2012-10-13         12426
2012-10-14         15098
2012-10-15         10139
2012-10-16         15084
2012-10-17         13452
2012-10-18         10056
2012-10-19         11829
2012-10-20         10395
2012-10-21          8821
2012-10-22         13460
2012-10-23          8918
2012-10-24          8355
2012-10-25          2492
2012-10-26          6778
2012-10-27         10119
2012-10-28         11458
2012-10-29          5018
2012-10-30          9819
2012-10-31         15414
2012-11-01             0
2012-11-02         10600
2012-11-03         10571
2012-11-04             0
2012-11-05         10439
2012-11-06          8334
2012-11-07         12883
2012-11-08          3219
2012-11-09             0
2012-11-10             0
2012-11-11         12608
2012-11-12         10765
2012-11-13          7336
2012-11-14             0
2012-11-15            41
2012-11-16          5441
2012-11-17         14339
2012-11-18         15110
2012-11-19          8841
2012-11-20          4472
2012-11-21         12787
2012-11-22         20427
2012-11-23         21194
2012-11-24         14478
2012-11-25         11834
2012-11-26         11162
2012-11-27         13646
2012-11-28         10183
2012-11-29          7047
2012-11-30             0


### Histogram of the total number of steps taken each day

```r
hist_fig1 <- with(totalStepsDf, hist(totalSteps, xlab = "Total Steps"
                                     ,main = "Histogram of Total Steps", col= "skyblue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


### Mean of the total number of steps taken per day is 9354.2295082.
### Median of the total number of steps taken per day is 10395.


## What is the average daily activity pattern?

```r
avgDailyActivity <- data %>%
  group_by(interval) %>%
  summarise(avgSteps = mean(steps, na.rm = T))
```




```r
with(avgDailyActivity
     , plot(interval, avgSteps, "l"
            , xlab = "Interval", ylab = "Average Steps"
            , main = "Time Series plot for Average Daily Activity"
            , col = "royalblue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
maxVal <- max(avgDailyActivity$avgSteps)
maxInterval <- avgDailyActivity[which(avgDailyActivity$avgSteps == maxVal),]$interval
```

### On an avergae, 835 interval contains the maximum number of steps.


## Imputing missing values

### total number of missing values in the dataset 

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

### filling in all of the missing values in the dataset by taking mean for every interval

```r
means <- tapply( data$steps, data$interval, mean , na.rm=TRUE)

data_NA <- data[is.na(data$steps),]
data_not_NA <- data[!is.na(data$steps),]

data_NA$steps <- as.factor(data_NA$interval)
levels(data_NA$steps) <- means
levels(data_NA$steps) <- round(as.numeric(levels(data_NA$steps)))
data_NA$steps <- as.integer(as.vector(data_NA$steps))

new_data <- rbind(data_NA, data_not_NA)
```



### impact of imputing missing data on the estimates of the total daily number of steps


```r
new_totalStepsDf <- new_data %>%
  group_by(date) %>%
  summarise(totalSteps = sum(steps, na.rm = TRUE))
  

mean_2 <- mean(new_totalStepsDf$totalSteps)
median_2 <- median(new_totalStepsDf$totalSteps)

par(mfrow = c(1,2))
plot(hist_fig1, col = "skyblue", main = "Histogram 1", xlab = "Total Steps")
hist_fig2 <- with(new_totalStepsDf, 
                  hist(totalSteps, xlab = "Total Steps"
                       ,main = "Histogram 2", col= "blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



#### Summary of Total Steps in first part of assignment 


```r
summary(totalStepsDf$totalSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
#### Summary of Total Steps with imputed NA values

```r
summary(new_totalStepsDf$totalSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10762   10766   12811   21194
```
As can be seen the the Mean and Median value have incresed after imputing NA values, The minimum value has also changed although the maximum value is unchanged.



## Are there differences in activity patterns between weekdays and weekends?



```r
weekend <- c("Saturday", "Sunday")
new_data$day <- as.factor(ifelse(weekdays(new_data$date) %in% weekend, "Weekend", "Weekday"))

avgDailyActivity_byday <- aggregate(steps ~ interval + day, 
                                    data = new_data, FUN = mean)
head(avgDailyActivity_byday)
```

```
##   interval     day      steps
## 1        0 Weekday 2.28888889
## 2        5 Weekday 0.40000000
## 3       10 Weekday 0.15555556
## 4       15 Weekday 0.17777778
## 5       20 Weekday 0.08888889
## 6       25 Weekday 1.57777778
```

```r
plot <- ggplot(data = avgDailyActivity_byday, aes(interval, steps))
plot + geom_line(color = "violet") + facet_grid( day ~.) +
  xlab("Interval") + ylab("Number of Steps") + 
  ggtitle("Average Steps by Interval") +
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



