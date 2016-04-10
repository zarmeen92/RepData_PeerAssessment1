---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
```{r,echo=TRUE}
data <- read.csv("activity/activity.csv")
dim(data)
head(data)
```
## What is mean total number of steps taken per day?
```{r,echo=TRUE}

steps_data <- aggregate(steps ~ date, data=data, sum, na.rm = TRUE)
hist(steps_data$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```

## What is the average daily activity pattern?
```{r,echo=TRUE}
steps_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_interval$interval,steps_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
max_interval <- steps_interval[which.max(steps_interval$steps),1]
print(paste("The maximum number of steps in a five minute interval was: ", max_interval))
```

## Imputing missing values
```{r,echo=TRUE}
imputed_data <- data
imputed_data$steps[is.na(imputed_data$steps)] <- median(data$steps, na.rm=TRUE)

head(imputed_data)
steps_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

#computing mean
rawsteps_mean <- mean(data$steps, na.rm=TRUE)
rawsteps_median <- median(data$steps, na.rm=TRUE)
print(paste("The mean steps per day is: ", rawsteps_mean))
print(paste("The median steps per day is: ", rawsteps_median))

bsteps_mean <- mean(imputed_data$steps)
bsteps_median <- median(imputed_data$steps)
print(paste("The mean is: ", bsteps_mean))
print(paste("The median is: ", bsteps_median))
```

## Are there differences in activity patterns between weekdays and weekends?
```{r,echo=TRUE}

imputed_data$date <- as.Date(imputed_data$date)
imputed_data$dayname <- weekdays(imputed_data$date)
imputed_data$weekend <- as.factor(ifelse(imputed_data$dayname == "Saturday" |                                                                  imputed_data$dayname == "Sunday", "weekend", "weekday"))
library(lattice)
plotdata <- aggregate(steps ~ interval + weekend,imputed_data, mean)
xyplot(steps ~ interval | factor(weekend), data=plotdata, aspect=1/3, type="l")
```