---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

``` r
activity <- read.csv("https://raw.githubusercontent.com/Caddiecxdy/RepData_PeerAssessment1/master/activity.csv")
```
## What is mean total number of steps taken per day?

``` r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
daily_steps <- activity %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE))
hist(daily_steps$total_steps)
```

![](PA1_template1_files/figure-html/total-number-of-steps-taken-per-day-1.png)<!-- -->

``` r
mean_daily_steps<-mean(daily_steps$total_steps)
print(mean_daily_steps)
```

```
## [1] 9354.23
```

``` r
median_daily_steps<-median(daily_steps$total_steps)
print(median_daily_steps)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

``` r
interval_average <- activity %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE)) %>%
  arrange(interval)
plot(interval_average$interval, interval_average$avg_steps, type = "l")
```

![](PA1_template1_files/figure-html/average_daily_activity_pattern-1.png)<!-- -->

``` r
max_point <- interval_average[which.max(interval_average$avg_steps), ]
print(max_point$interval)
```

```
## [1] 835
```

## Imputing missing values

``` r
no_NA <- sum(is.na(activity))
print(no_NA)
```

```
## [1] 2304
```

``` r
activity_filled_mean <- activity %>%
  group_by(date) %>%
  mutate(
    steps_filled = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
hist(activity_filled_mean$steps_filled)
```

![](PA1_template1_files/figure-html/Imputing_missing_values-1.png)<!-- -->

``` r
mean_daily_filled_steps<-mean(activity_filled_mean$steps_filled,na.rm = TRUE)
print(mean_daily_filled_steps)
```

```
## [1] 37.3826
```

``` r
median_daily_filled_steps<-median(activity_filled_mean$steps_filled,na.rm = TRUE)
print(median_daily_filled_steps)
```

```
## [1] 0
```
## Are there differences in activity patterns between weekdays and weekends?

``` r
activity_weekends <- activity_filled_mean %>%
  mutate(
    date = as.Date(date),
    day_type = ifelse(
      weekdays(date) %in% c("Saturday", "Sunday"),
      "weekend",
      "weekday"
    ),
    day_type = factor(day_type, levels = c("weekday", "weekend"))
  )
interval_avg <- aggregate(steps ~ interval + day_type, 
                         data = activity_weekends, 
                         FUN = mean)
weekday_data <- subset(interval_avg, day_type == "weekday")
weekend_data <- subset(interval_avg, day_type == "weekend")
weekday_data <- weekday_data[order(weekday_data$interval), ]
weekend_data <- weekend_data[order(weekend_data$interval), ]
plot(weekday_data$interval, weekday_data$steps,
     type = "l")
```

![](PA1_template1_files/figure-html/weekdays_and_weekends-1.png)<!-- -->

``` r
plot(weekend_data$interval, weekend_data$steps,
     type = "l")
```

![](PA1_template1_files/figure-html/weekdays_and_weekends-2.png)<!-- -->
