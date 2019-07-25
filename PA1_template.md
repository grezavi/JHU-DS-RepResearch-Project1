---
title: "PA1_template"
author: "Gabe Rezavi"
date: "7/25/2019"
output: html_document
keep_md: true 
---

This is my possibly fourth attempt for our RMarkdown Project 1 document. Let's load the data and libraries and check it out.


```r
library(ggplot2)
library(dplyr)
library(knitr)
activity <- read.csv(file="activity.csv", header=TRUE, sep=",")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity,3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```
What is mean total number of steps taken per day? Visualization (histogram)? 


```r
Tot.Steps <- aggregate(steps ~ date, activity, FUN=sum)
head(Tot.Steps,3)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
```

```r
hist(Tot.Steps$steps, col="blue", xlab = "Number of Steps", ylab = "Steps", main = "Total Steps taken by Day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-13-1.png" width="672" />
Mean and Median of the total number of steps taken per day?


```r
Mean.Steps <- mean(Tot.Steps$steps)
Mean.Steps
```

```
## [1] 10766.19
```

```r
Median.Steps <- median(Tot.Steps$steps)
Median.Steps
```

```
## [1] 10765
```

What is the average daily activity pattern?


```r
Intervals <- aggregate(steps ~ interval, activity, FUN=sum)
head(Intervals,3)
```

```
##   interval steps
## 1        0    91
## 2        5    18
## 3       10     7
```

```r
plot(Intervals$interval, Intervals$steps, type = "l", lwd = 2, xlab = "Interval", ylab = "Total Steps", main = "The Average Daily Activity Pattern") 
```

<img src="PA1_template_files/figure-html/unnamed-chunk-15-1.png" width="672" />



```r
max.Steps <- Intervals[which.max(Intervals$steps),1]
max.Intervals <- max(Intervals)
max.Steps
```

```
## [1] 835
```

```r
max.Intervals
```

```
## [1] 10927
```
The maximum numbers of steps 10927 were taken on the 835 interval, as illustrated here using the inline code.


Now, I will try to impute missing values starting with first calculating the total number of missing values (NAs) in the dataset.


```r
table(is.na(activity))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```
Let's impute the missing values with an appropriate strategy and recreate the data set as max.Steps2.


```r
Mean.Intervals <- aggregate(steps ~ interval, activity, FUN=mean)
New.Intervals <- merge(x=activity, y=Mean.Intervals, by="interval")

New.Intervals$steps <- ifelse(is.na(New.Intervals$steps.x), New.Intervals$steps.y, New.Intervals$steps.x)
head(New.Intervals)
```

```
##   interval steps.x       date  steps.y    steps
## 1        0      NA 2012-10-01 1.716981 1.716981
## 2        0       0 2012-11-23 1.716981 0.000000
## 3        0       0 2012-10-28 1.716981 0.000000
## 4        0       0 2012-11-06 1.716981 0.000000
## 5        0       0 2012-11-24 1.716981 0.000000
## 6        0       0 2012-11-15 1.716981 0.000000
```

```r
New.Intervals <- select(New.Intervals, steps, date, interval)
head(New.Intervals)
```

```
##      steps       date interval
## 1 1.716981 2012-10-01        0
## 2 0.000000 2012-11-23        0
## 3 0.000000 2012-10-28        0
## 4 0.000000 2012-11-06        0
## 5 0.000000 2012-11-24        0
## 6 0.000000 2012-11-15        0
```

```r
Tot.Steps2 <- aggregate(steps ~ date, New.Intervals, FUN=sum)
head(Tot.Steps2,3)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
```

Before and after visualization via simple histograms.


```r
par(mfrow=c(1,2))

hist(Tot.Steps2$steps, col="yellow", xlab = "Steps", ylab = "Frequency", ylim = c(0,35), main = "Total Number Of Steps Taken Each day (w/ Imputation)", cex.main = 0.7)

hist(Tot.Steps$steps, col="red", xlab = "Steps", ylab = "Frequency", ylim = c(0,35), main = "Total Number Of Steps Taken Each day (w/out Imputation)", cex.main = 0.7)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-19-1.png" width="672" />
Now, let's calculate how much the mean and the media values have changed as a result of these imputations.


```r
Mean.Steps2 <- mean(Tot.Steps2$steps)
Median.Steps2 <- median(Tot.Steps2$steps)
Diff.in.Mean <- Mean.Steps - Mean.Steps2
Diff.in.Median <- Median.Steps - Median.Steps2
Diff.in.Mean
```

```
## [1] 0
```

```r
Diff.in.Median
```

```
## [1] -1.188679
```

The difference in mean is 0 while the difference in the median is -1.1886792.

Now, let's check out the difference in activities between the weekdays and the weekend.


```r
library(chron)
table(is.weekend(New.Intervals$date))
```

```
## 
## FALSE  TRUE 
## 12960  4608
```

```r
New.Intervals$dayofweek <- ifelse(is.weekend(New.Intervals$date), "weekend", "weekday")
head(New.Intervals)
```

```
##      steps       date interval dayofweek
## 1 1.716981 2012-10-01        0   weekday
## 2 0.000000 2012-11-23        0   weekday
## 3 0.000000 2012-10-28        0   weekend
## 4 0.000000 2012-11-06        0   weekday
## 5 0.000000 2012-11-24        0   weekend
## 6 0.000000 2012-11-15        0   weekday
```

```r
Mean.Itervals2 <- aggregate(steps ~ interval + dayofweek, New.Intervals, FUN=mean)
head(Mean.Itervals2)
```

```
##   interval dayofweek      steps
## 1        0   weekday 2.25115304
## 2        5   weekday 0.44528302
## 3       10   weekday 0.17316562
## 4       15   weekday 0.19790356
## 5       20   weekday 0.09895178
## 6       25   weekday 1.59035639
```

```r
ggplot(Mean.Itervals2, aes(x=interval, y=steps)) + geom_line(color="blue", size=1) + facet_wrap(~dayofweek, nrow=2) + labs(x="\nInterval", y="\nNumber of steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" width="672" />


