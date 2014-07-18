==========================================  
title: "ReproducibleResearch-Assignment1"  
output: html_document  
==========================================  

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at five-minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in five-minute intervals each day.

## Data
The data for this assignment was downloaded from the following web site: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip.

The variables included in this dataset are:

*steps: Number of steps taking in a five-minute interval (missing values are coded as NA)

*date: The date on which the measurement was taken in YYYY-MM-DD format

*interval: Identifier for the five-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Steps
The following steps were taken in order to analyze the dataset and carry out the assignment. Assignment questions are listed and answered. R code, figures and text demonstrate the flow of the analysis.

1. Ensure the correct working directory, containing downloaded zip file, is being used (/Users/nicolegoebel/GitHub/DataScienceCoursera/ReproducibeResearch):

```r
setwd("/Users/nicolegoebel/GitHub/DataScienceCoursera/ReproducibeResearch/Assignment1/")  
```
2. Unzip the pre-dowloaded "repdata-data-activity.zip" file.

```r
if (!file.exists("activity.csv")){
    unzip("repdata-data-activity.zip")
} else {message("file is already unzipped")}
```

```
## file is already unzipped
```
3. Load data.

```r
dat <- read.csv("activity.csv")
```
4. What is the mean total number of steps taken per day?  

To calculate the mean total number of steps taken per day, a vector of steps was isolated from the data frame, the NA values were removed from this vector, and then the mean and median of the number of steps were calculated, as shown in the following code:

```r
stepsVec<-dat$steps
bad<-is.na(stepsVec)
stepsNoNA<-stepsVec[!bad]
meanSteps<-mean(stepsNoNA)
medianSteps<-median(stepsNoNA, na.rm=TRUE)
```
The mean number of steps taken was: 37.3826  

The median number of steps taken was: 0

A histogram of the total number of steps taken each day is shown below:

```r
hist(stepsNoNA, ylab='frequency of steps', main="Histogram of the Total Number of Steps Taken Each Day",xlab="Bins For Number of Steps")
```

![plot of chunk figure total number of steps](figure/figure total number of steps.png) 

5. What is the average daily activity pattern?  

The following code was used to create the figure below figure. The figure shows the average daily activity pattern in a time-series plot of the five-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
datesVec<-dat$date
intervalVec<-dat$interval
datesNoNA<-datesVec[!bad]
intervalNoNA<-intervalVec[!bad]
datNoNA<-data.frame(intervalNoNA, stepsNoNA, datesNoNA)
pattern<-aggregate(.~intervalNoNA, FUN=mean, data=datNoNA)
tmp<-pattern$stepsNoNA==max(pattern$stepsNoNA)
row<-pattern[tmp,]
maxInt<-row$intervalNoNA
maxSteps<-row$stepsNoNA
```


```r
with(pattern, plot(intervalNoNA, stepsNoNA, type="l", xlab="Interval", ylab="Number of Steps", main="Average Steps Averaged Across All Days (Oct-Nov 2012)"))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

On average across all the days in the dataset, the 835th five-minute interval contains the maximum number of steps (206.1698).

6. Input misssing values.  

The number of missing values were calculated using the following code:


```r
naindex <- which(is.na(dat)==TRUE)
numNAs<-length(naindex)
```
The total number of missing values in the dataset are 2304.

Missing NAs were filled in by using the total mean for the five-minute intervals as calculated above for the dataset (ignoring NAs). This data manipulation is shown in the code below:

```r
newDat<-data.frame(dat$interval, dat$steps, dat$date)
newDat[naindex,2] <- meanSteps
newMean<-mean(newDat$dat.steps)
newMedian<-median(newDat$dat.steps, na.rm=TRUE)
totalSteps<-sum(stepsNoNA)
totalNewSteps<-sum(newDat$dat.steps)
diffSteps<-totalNewSteps-totalSteps
```

A histogram of total number of steps taken each day is shown below.

```r
hist(newDat$dat.steps, ylab='Frequency of steps', main="Histogram of the Total Number of Steps Taken Each Day Using New Data Set",xlab="Bins For Number of Steps")
```

![plot of chunk histogram with new data set](figure/histogram with new data set.png) 

Using the new data set with substituted NA values, the mean (37.3826) and median (0) are different than calculated when ignoring NA values.

The total number of steps with the new data set (6.5674 &times; 10<sup>5</sup>) is 8.613 &times; 10<sup>4</sup> steps greater than that calculated when ignoring NA values. Using an average value for all average step intervals is likely to overestimate the average steps.

7. Are there differences in activity patterns between weekdays and weekends?

Using the data set with the filled-in missing values, a new factor variable was created within the data set. This factor variable specifies two levels: "weekday" and "weekend", indicating whether a given date is a weekday or a weekend day. This factor was used to plot a time series of the five-minute interval (x-axis) and the average number of steps taken, averaged across either weekday days or weekend days (y-axis). The figure below indicates that the peak number of steps occurs around the same interval, but declines after this peak on weekday days. The number of steps over the intervals on weekend days shows greater activity.


```r
newDat$dat.date<-as.POSIXct(as.character(newDat$dat.date),format="%Y-%m-%d")
newDat$weekday<-weekdays(newDat$dat.date)
newDat["weekdayORend"]<-newDat$weekday
newDat$weekdayORend <- as.character(newDat$weekdayORend)
newDat$weekdayORend[newDat$weekdayORend=="Monday"] <- "weekday"
newDat$weekdayORend[newDat$weekdayORend=="Tuesday"] <- "weekday"
newDat$weekdayORend[newDat$weekdayORend=="Wednesday"] <- "weekday"
newDat$weekdayORend[newDat$weekdayORend=="Thursday"] <- "weekday"
newDat$weekdayORend[newDat$weekdayORend=="Friday"] <- "weekday"
newDat$weekdayORend[newDat$weekdayORend=="Saturday"] <- "weekend"
newDat$weekdayORend[newDat$weekdayORend=="Sunday"] <- "weekend"

newDatWeekend<-newDat[newDat$weekdayORend == "weekend",]
newDatWeekday<-newDat[newDat$weekdayORend == "weekday",]
  
we<-data.frame(newDatWeekend$dat.interval, newDatWeekend$dat.steps)
wd<-data.frame(newDatWeekday$dat.interval, newDatWeekday$dat.steps)

wePattern<-aggregate(.~newDatWeekend.dat.interval, FUN=mean, data=we)
wdPattern<-aggregate(.~newDatWeekday.dat.interval, FUN=mean, data=wd)
```


```r
par(mfrow = c(2, 1))
plot(wePattern$newDatWeekend.dat.interval, wePattern$newDatWeekend.dat.steps, type="l", xlab="Interval", ylab="Number of Steps", main="Weekend")
plot(wdPattern$newDatWeekday.dat.interval, wdPattern$newDatWeekday.dat.steps, type="l", xlab="Interval", ylab="Number of Steps", main="Weekday")
```

![plot of chunk figure](figure/figure.png) 
