# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
###Libraries
These are the libraries we are going to use:

```r
library(dplyr)
library(lubridate)
library(ggplot2)
```

### Loading the data
We begin by loading the activity data CSV file into a variable called data.

```r
data <- read.csv("activity.csv", na.strings="NA")
```
### Transforming the data
Let's look to what we've loaded

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
Date and interval types:

```r
class(data$date[0])
```

```
## [1] "factor"
```

```r
class(data$interval[0])
```

```
## [1] "integer"
```
It is useful to merge these columns into a single, date-class column. Let us call this new column DateFull:


```r
data <- mutate(data, dateFull = ymd_hm(paste(date, sprintf("%04d", interval))))
head(data)
```

```
##   steps       date interval            dateFull
## 1    NA 2012-10-01        0 2012-10-01 00:00:00
## 2    NA 2012-10-01        5 2012-10-01 00:05:00
## 3    NA 2012-10-01       10 2012-10-01 00:10:00
## 4    NA 2012-10-01       15 2012-10-01 00:15:00
## 5    NA 2012-10-01       20 2012-10-01 00:20:00
## 6    NA 2012-10-01       25 2012-10-01 00:25:00
```

```r
class(data$dateFull[0])
```

```
## [1] "POSIXct" "POSIXt"
```

Now, we are ready to to operate with date functions.


## What is mean total number of steps taken per day?
First, we group the data by date:

```r
byday <- group_by(data, date)
```
Because date has a resolution of day, we can be sure that byday holds a correct day-by grouping.

### Steps per day
The total steps per day is calculated by summarizing byday with sum:

```r
steps_per_day<-summarise(byday, steps=sum(steps) )
steps_per_day
```

```
## Source: local data frame [61 x 2]
## 
##          date steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## ..        ...   ...
```

### A histogran of steps per day

```r
hist(steps_per_day$steps, main="Historgram of steps per day", xlab="steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

### Mean and median of the steps taken per day
A call to summary gives:

```r
summary(steps_per_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

## What is the average daily activity pattern?
First, we  group data by interval, then summarize with mean:

```r
byinterval <- group_by(data, interval)
steps_by_interval <- summarise(byinterval, steps=mean(steps, na.rm=TRUE))
```
### Average steps vs interval (across all days)

```r
plot(steps_by_interval, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
.

### Which interval holds the maximum ?
To know which 5-minute inteval contains the maximum number of steps, we use which.max over our summarized data to get the index:

```r
max_ind <- which.max(steps_by_interval$steps)
steps_by_interval[max_ind,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```
We can see then, that the 8:35AM time its the one when more average steps occurr, with a value of 206.



## Imputing missing values

### How many missing values we have

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
mean(is.na(data$steps))
```

```
## [1] 0.1311475
```
2304 missing values, which is the 13% of the data set.

### Filling NAs
Let us try to fill NAs with the hourly mean. First, we create a new dateHourly column, constructed by removing the minutes and seconds from the dateFull col.

```r
tmpdata <- mutate(data, dateHourly=ymd_h(as.character(dateFull, "%Y%m%d %H") ))
```
Now, lets create a data frame with the steps averaged by the hour.

```r
byhour <- group_by(tmpdata, dateHourly)
step_hour_means <- summarise(byhour, steps_mean = mean(steps) )
```
We merge the two dataframes, and add a new column called steps_filled. This column will hold the original step value if not NA, or will hold the hour averege instead

```r
tmpdata <- merge(tmpdata, step_hour_means)
tmpdata <- mutate(tmpdata, steps_filled=ifelse(is.na(steps), steps_mean, steps))
```
#### Did it work ?
Summary for steps_filled

```r
summary(tmpdata$steps_filled)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
Summary for steps

```r
summary(data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```
This gives us the same number of missing NAs. 

#### Why dit it fail?
By examining the data we can see that an hour-based mean is not useful becasue missing values are present in  whole-days blocks, meaning whole days are missing. Now, becasue we don't have any data for that day, there doesn't seem to exist a good mean-injection value we can use to fill NAs.

### New data set without NAs
A better approach will be then to simple eliminate rows with missing values:


```r
fdata <- data[complete.cases(data),]
summary(fdata$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   12.00  806.00
```
Which gives us the same summary as before, but without NAs. 

Our new working dataset will be fdata.

### Analyzing the new set

#### Let us do a histogram.
First, we  group data by interval, then summarize with mean.

```r
byinterval <- group_by(fdata, interval)
steps_by_interval <- summarise(byinterval, steps=mean(steps, na.rm=TRUE))
plot(steps_by_interval, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png) 
#### Mean and median:
From our new NA-removed data

```r
summary(fdata$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    0.00   37.38   12.00  806.00
```
From our original data

```r
summary(data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

#### In conclusion
We got the same histogram, mean and mediam as when we used the full data set.

By simply removing missing data, we are obtaining the same results.




## Are there differences in activity patterns between weekdays and weekends?

### New factor, day_type
First, we create a new column called wday, which holds the week day's name, and then, a factor called day_type with either  'weekend' or 'weekday' values.

```r
fdata <- mutate(fdata, wday=weekdays(dateFull, abbreviate=TRUE))
fdata <- mutate(fdata, day_type=ifelse(wday=='Sat' | wday=='Sun', 'weekend', 'weekday'))
```
### Weekend vs weekday plot
To get an interval steps, splitted by day_type, we group by interval and day_type, then, summarize with mean:

```r
grouped <- group_by(fdata, interval, day_type)
grouped_mean <- summarise(grouped, steps=mean(steps))
```

Using qplot we can have two plots of  steps vs interval one for each day_type:

```r
qplot(data=grouped_mean, x=interval, y=steps, facets=day_type~. , geom="line", color=day_type)
```

![](PA1_template_files/figure-html/unnamed-chunk-25-1.png) 

### Analysis
This plot shows two differences in activity patterns:

1. Weekends' activities start later.
2. Weekends' activity are more evenly distributed trhu the day, whereas weekdays' mornings are clearly peaking.


#### end.

