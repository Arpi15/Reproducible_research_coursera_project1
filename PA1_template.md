---
title: "Reproducible Research - Course Project 1"
author: "Arpita Mukherjee"
date: "11/07/2019"
output: html_document
---

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken


## Loading and Preprocessing the data


So, to begin with we will first load the dataset :


```{r, echo=TRUE}
activity <- read.csv("~/Desktop/coursera/activity.csv", stringsAsFactors = FALSE, header = TRUE)
```

Now that our dataset is in the variable "activity", we will do some basic processing with the data:

```{r, echo=TRUE}
head(activity)  
tail(activity)
str(activity)
dim(activity)
```



## What is mean total number of steps taken per day?

For this part we will first calculate the total number of steps taken per day using "tapply" function:

```{r, echo=TRUE}
totalsteps_perday <- tapply(activity$steps, activity$date, sum)
```


Then, we will plot a histogram for total number of steps taken per day

```{r, echo=TRUE}
hist(totalsteps_perday, xlab = "Total Steps", main = "Histogram of Total number of Steps taken per day") 
```

Now, we will calculate the mean and the median for total number of steps taken per day after removing the missing values from the dataset:

```{r, echo=TRUE}
mean1 <- mean(totalsteps_perday, na.rm = TRUE)
mean1
median1 <- median(totalsteps_perday, na.rm = TRUE)
median1
```






##  What is the average daily activity pattern?


For this, we need to find the average daily activity.We will do this using aggregate function:

```{r, echo=TRUE}
avg_daily_activity <- aggregate(steps ~ interval, mean, data = activity, na.rm = TRUE) 
```

Now, we will make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) :

```{r, echo=TRUE}
plot(avg_daily_activity$interval, avg_daily_activity$steps, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average activity across all days")
```


Now, for finding the 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps, we will use which.max function to first find the row id and then fetching the 5-minute interval using the row id:

```{r, echo=TRUE}
max_row_id <- which.max(avg_daily_activity$steps)   
max_row_id
avg_daily_activity[max_row_id, ]
```





##Imputing missing values

For this part, as we can see that the original dataset contains a lot of missing (NA) values, so first we need to find the total number of missing values in the dataset and then devise a strategy to fill in those missing values which we will see later:

```{r, echo=TRUE}
missing_values <- !complete.cases(activity)       
sum(missing_values)
```

So, we can see that the original dataset contains 2304 missing values.
Now, to fill those missing values we will calculate the mean for 5-minute interval and fill the missing values:

```{r, echo=TRUE}
for(i in 1:nrow(activity))
{if(is.na(activity$steps[i]))         
{ interval_val <- activity$interval[i]
  row_id <- which(avg_daily_activity$interval == interval_val)
  steps_val <- avg_daily_activity$steps[row_id]
  activity$steps[i] <- steps_val
}                       
}
```

So, what we have done here is we have checked NA values for each row in steps and replaced it with mean of steps which we have already calculated during average daily activity.Now our new dataset has all the missing values filled in.

```{r, echo=TRUE}
head(activity)
```


Now, we will make a histogram of the total number of steps taken each day and Calculate the mean and median total number of steps taken per day.For calculating the total number of steps per day we will use the same method using tapply.

```{r, echo=TRUE}
totalsteps_perday_imputed <- tapply(activity$steps, activity$date, sum)
hist(totalsteps_perday_imputed, xlab = "Total Steps", main = "Histogram of Total number of Steps taken per day (Imputed)")
```

We can see that in the new histogram frequency values have changed from 25 to 35.
Now we will calculate the new mean and median.

```{r, echo=TRUE}
mean <- mean(totalsteps_perday_imputed)
mean
median <- median(totalsteps_perday_imputed) 
median
```

For the new dataset, median values have changed from 10765 to 10766.19




##Are there differences in activity patterns between weekdays and weekends?

For this part of the question we have to first find whether a particular date is weekday or a weekend.For this, we will take the help of weekdays function.We will use the new dataset with filled in values for this part.

```{r, echo=TRUE}
day <- weekdays(as.Date(activity$date)) 
daylevel <- vector()               
for(i in 1:nrow(activity))
        {
        if (day[i] == "Saturday")
       {daylevel[i] <- "Weekend"}
        
 else if (day[i] == "Sunday")
        {daylevel[i] <- "Weekend"}
else 
{daylevel[i] <- "Weekday"}
}

activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)
head(activity$daylevel)
```

Here, first we have converted "date"" variable to "day" using weekdays function.Then we have assigned vector to "daylevel" variable.After that we are checking for each day if it's a Saturday or Sunday.If it's Saturday or Sunday we are assigning it to "Weekend" variable otherwise it's a "Weekday".Then we are assigning the "daylevel" variable to a factor variable.


Now to finish off we will make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).For this, we will use the lattice package.

First we will add the daylevel column to the new dataset using aggregate function:


```{r, echo=TRUE}
rm(mean)
steps_by_day <- aggregate(steps ~ interval + daylevel, data = activity, mean)  

```

```{r, echo=TRUE}

library(lattice)           
xyplot(steps ~ interval|daylevel, steps_by_day, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of Steps")
```
