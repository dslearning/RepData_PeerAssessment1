# Reproducible Research: Peer Assessment 1

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

## Loading and preprocessing the data

```r
d <- read.table(file="activity.csv",head=TRUE, sep=",", stringsAsFactors =  FALSE)
d$date <- as.Date(d$date, format = "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
g<- ggplot(d, aes(x=date, y = steps)) + 
  geom_histogram(stat = "identity", color="blue", fill="blue" ) +
  labs(x="Date") +
  labs(y="Total number of steps")

g
```

![](PA1_template_files/figure-html/plot_hist-1.png) 

####Mean and median of the total number of steps per day

```r
mean(d$steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
median(d$steps, na.rm = TRUE)
```

```
## [1] 0
```

## What is the average daily activity pattern?

```r
d_avg <- d %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

x_axis_formatter <- function(l) {
  l <- sprintf("%d:%02d", as.integer( l / 100), l %% 100)
}

g <- ggplot(d_avg, aes(x=interval, y=avg_steps)) +
  geom_line(col="blue") +
  scale_x_continuous(labels=x_axis_formatter) +
  labs(x="Time of measurement, HH:MM") +
  labs(y="Average number of steps")
g
```

![](PA1_template_files/figure-html/act_pattern-1.png) 

According to this chart the interval at 8:35 contains maximum number of steps.

## Imputing missing values
Number of missing values:

```r
mv<-as.integer(count(d[is.na(d$steps),]))
mv
```

```
## [1] 2304
```

```r
# Filling NAs value with the mean for the same interval
d2<-d
for(i in unique(d2$interval)){
  d2[is.na(d2$steps) & d2$interval==i,]$steps = mean(d[d$interval==i,]$steps, na.rm = TRUE)
}

g<- ggplot(d2, aes(x=date, y = steps)) + 
  geom_histogram(stat = "identity", color="blue", fill="blue" ) +
  labs(x="Date") +
  labs(y="Total number of steps")
g
```

![](PA1_template_files/figure-html/miss_val-1.png) 

#### Mean of total steps per day

```r
mean(d2$steps, na.rm = TRUE)
```

```
## [1] 37.3826
```

#### Median of total steps per day

```r
median(d2$steps, na.rm = TRUE)
```

```
## [1] 0
```
_**Since for replacement of NAs values we used mean values for the same interval, there are no significant changes in diagram. Mean value of total steps per day is the same. Median value equals to zero. This behaviour is a result of our filling strategy: we tried to keep original mean values.**_

## Are there differences in activity patterns between weekdays and weekends?

```r
dow <- d %>%
  mutate(typeOfDay = ifelse(weekdays(d$date) %in% c("Saturday","Sunday"), "weekend","weekday")) %>%
  group_by(interval, typeOfDay) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

dow$typeOfDay <- as.factor(dow$typeOfDay)

g <- ggplot(dow, aes(x=interval, y=avg_steps)) +
  geom_line(col="blue") +
  #facet_grid(. ~ typeOfDay,  ) +
  facet_wrap(~ typeOfDay, nrow = 2) +
  scale_x_continuous(labels=x_axis_formatter) +
  labs(x="Time of measurement, HH:MM") +
  labs(y="Average number of steps")

g
```

![](PA1_template_files/figure-html/type_of_day-1.png) 
