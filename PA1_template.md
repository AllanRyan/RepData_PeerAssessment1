---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
#Load libraries
library(knitr)
library(timeDate)
library(xlsx)
library(XML)
library(data.table)
```


```r
#Load data
setwd("c:/HW/ReproResearch/data")
stepdata <- read.table("activity.csv", sep=",",header=TRUE)
```


```r
# Transform data
StepTrans <- data.frame(steps=as.numeric(stepdata$steps),date=as.Date(stepdata$date), interval=stepdata$interval)
```


## What is mean total number of steps taken per day?

### Strip the missing values from the data

```r
#Subset the data to include just complete cases
good <- complete.cases(StepTrans)
compcases <- StepTrans[good, ]
```

###Generate histogram of the total number of steps per day

```r
steps.day <- aggregate(steps ~ date, data=compcases, FUN=sum)
hist(steps.day$steps)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

###Calculate the mean and median steps per day

```r
mean(steps.day$steps)
```

```
## [1] 10766.19
```

```r
median(steps.day$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

###Generate a time series plot of the average number of steps per interval

```r
steps.interval <- aggregate(steps ~ interval, data= compcases, FUN=mean)
plot(steps ~  interval, steps.interval, type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

###Identify the interval with the maximum # of steps, subset and print the observation

```r
maxInt <- max(steps.interval$steps)
StepMaxInt <- subset(steps.interval,steps.interval$steps==maxInt)
StepMaxInt 
```

```
##     interval    steps
## 104      835 206.1698
```



## Imputing missing values

###Indentify and calculate the number of missing values

```r
### missing values
totalObs <- length(StepTrans$steps) 
## Number of observations missing in dataset
colSums(is.na(StepTrans))
```

```
##    steps     date interval 
##     2304        0        0
```

###Calculate and print the mean number of steps per interval (to be used to impute missing values)

```r
AvgStepsInt <- mean(compcases$steps)
head(AvgStepsInt)
```

```
## [1] 37.3826
```
###Create a dataset with mean # of steps per interval imputed to observations with NA

```r
ImputedSteps <- data.frame(steps=ifelse(is.na(StepTrans$steps), AvgStepsInt, StepTrans$steps),date=StepTrans$date, interval=StepTrans$interval)
```

###Generate histogram of total steps per day

```r
Imp.steps.day <- aggregate(steps ~ date, data=ImputedSteps, FUN=sum)
hist(Imp.steps.day$steps)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

###Calculate and display mean and median steps per day

```r
mean(Imp.steps.day$steps)
```

```
## [1] 10766.19
```

```r
median(Imp.steps.day$steps)
```

```
## [1] 10766.19
```
The result of using average number of steps per interval to fill in the missing values has done little to alter the overall average but has caused the median and the mean to converge even more.The histograms differ in that the middle bars is taller which is understandable given that mean values were used to impute the missing data.

## Are there differences in activity patterns between weekdays and weekends?
###Generate a dataset with weekends and weekdays identfied as factors within a new variable

```r
Imp.steps.interval <- aggregate(steps ~ interval + date, data=ImputedSteps, FUN=sum)
v <- c("Monday","Tuesday", "Wednesday", "Thursday", "Friday")
Imp.steps.interval$Day <- ifelse(weekdays(Imp.steps.interval$date) %in% v,  "Weekday", "Weekend")
Imp.steps.interval$Day <- as.factor(Imp.steps.interval$Day)

##Create a subset with only the weekend data
WE.steps.interval <- subset(Imp.steps.interval, Imp.steps.interval$Day =="Weekend")
WE.Aggregate <- aggregate(steps ~  interval, data= WE.steps.interval, FUN=mean)

##Create a subset with only the weekday data
WD.steps.interval <- subset(Imp.steps.interval, Imp.steps.interval$Day =="Weekday")
WD.Aggregate <- aggregate(steps ~  interval, data= WD.steps.interval, FUN=mean)
```

###Create panel plots of time series of weekend and weekday patterns by interval

```r
opt  = par(mfrow= c(2,1))
plot(steps ~  interval, WE.Aggregate, type = "l", main="Weekends")
plot(steps ~  interval, WD.Aggregate, type = "l", main="Weekdays")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

```r
par(opt)
```
These plots clearly illustrate the different patterns of activity. During the week individuals get up earlier, have a burst of activity then there are peaks at lunch, late afternoon and early evening. On weekends people get up later but show more sustained activity through the day. 
