---
title: "Reproducible Research: Peer Assessment 1"
author: "Randall Bohn"
output: 
  html_document:
    theme: journal
    highlight: kate
    keep_md: true
---


## Loading and preprocessing the data

```r
if (!file.exists("activity.csv")) {
  unzip("./activity.zip")
}
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)

#load libraries here
library(ggplot2)

theme_set(theme_bw())
```

## What is mean total number of steps taken per day?

```r
# calculate the total number of steps per day
steps_per_day <- aggregate(steps ~ date, data=activity, sum)

# make a histogram of the total number of steps taken each day
ggplot(steps_per_day, aes(x=steps)) +
  geom_histogram(fill="slategray", binwidth=2000) +
  xlab("Number of Steps Taken") +
  ylab("days") +
  ggtitle("Distribution of Steps per Day")
```

![plot of chunk peer1a](figure/peer1a-1.png) 

What makes this a histogram and not a bar chart?
A histogram shows the distribution of a variable. 
In this case the variable is 'steps' or the number of steps taken in a day.
The chart shows that most days that number is between 10,000 and 15,000, 
but it can vary between zero and over 20,000.

[http://www.forbes.com/sites/naomirobbins/2012/01/04/a-histogram-is-not-a-bar-chart/]


```r
# calculate and report the mean and median of the total number of steps taken per day
s <-data.frame(
  mean = mean(steps_per_day$steps), 
  median = quantile(steps_per_day$steps, probs = 0.5))
row.names(s) <- "steps per day"
s
```

```
##                   mean median
## steps per day 10766.19  10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
