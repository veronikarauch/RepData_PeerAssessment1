# Reproducible Research: Peer Assessment 1

###1. Loading the data and packages used for analysing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lattice)
setwd("~/Downloads")
unzip("repdata-data-activity.zip")
df <- read.csv("activity.csv")
```

###2. Mean total number of steps taken per day
- Total number of steps taken per day

```r
df.na<-na.omit(df)
df.day<-df.na %>%
    group_by(date)%>%
    summarise(tot.steps=sum(steps))
df.day
```

```
## Source: local data frame [53 x 2]
## 
##          date tot.steps
## 1  2012-10-02       126
## 2  2012-10-03     11352
## 3  2012-10-04     12116
## 4  2012-10-05     13294
## 5  2012-10-06     15420
## 6  2012-10-07     11015
## 7  2012-10-09     12811
## 8  2012-10-10      9900
## 9  2012-10-11     10304
## 10 2012-10-12     17382
## ..        ...       ...
```

- Histogramm of total numberof steps taken per day

```r
b <- ggplot(df.day, aes(df.day$tot.steps))
p<-b+geom_histogram(binwidth = 1000)+labs(title="Histogram of Steps Taken per Day", 
                           x = "Number of Steps per Day", y = "Frequency")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

- Mean and Median of Total number of steps per day



```r
mean(df.day$tot.steps)
```

```
## [1] 10766.19
```


```r
median(df.day$tot.steps)
```

```
## [1] 10765
```

### Average daily activity pattern

- Time series plot of average daily activity

```r
df$interval<-as.factor(df$interval)
df.average<-df.na %>%
    group_by(interval)%>%
    summarise(average.steps=mean(steps))
df.average
```

```
## Source: local data frame [288 x 2]
## 
##    interval average.steps
## 1         0     1.7169811
## 2         5     0.3396226
## 3        10     0.1320755
## 4        15     0.1509434
## 5        20     0.0754717
## 6        25     2.0943396
## 7        30     0.5283019
## 8        35     0.8679245
## 9        40     0.0000000
## 10       45     1.4716981
## ..      ...           ...
```

```r
with(df.average, plot(interval,average.steps, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

- which interval contains the maximum steps

```r
filter(df.average, average.steps==max(average.steps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval average.steps
## 1      835      206.1698
```
Therefore the interval with the maximum steps is interval 835 with an average of 206.1968 steps

###Inputting missing values

- Number of missing values 

```r
miss <- is.na(df)
sum(miss)
```

```
## [1] 2304
```

- Replace missing values by the average for the interval (step 2 and 3)

```r
df.new <- df
for(i in 1:length(df.new$steps)){
     if (is.na(df.new$steps[i])) {
    df.new$steps[i]<-df.average[df.new$interval[i],2]
    }  
    }
df.new$steps<-as.numeric(df.new$steps)
head(df.new)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
- Histogramm of totalnumber of steps per day with new data set

```r
df.new.day<-df.new %>%
    group_by(date)%>%
    summarise(tot.steps=sum(steps))
df.new.day
```

```
## Source: local data frame [61 x 2]
## 
##          date tot.steps
## 1  2012-10-01  10766.19
## 2  2012-10-02    126.00
## 3  2012-10-03  11352.00
## 4  2012-10-04  12116.00
## 5  2012-10-05  13294.00
## 6  2012-10-06  15420.00
## 7  2012-10-07  11015.00
## 8  2012-10-08  10766.19
## 9  2012-10-09  12811.00
## 10 2012-10-10   9900.00
## ..        ...       ...
```

```r
b <- ggplot(df.new.day, aes(tot.steps))
p<-b+geom_histogram(binwidth = 1000)+labs(title="Histogram of Steps Taken per Day with New Data Set", 
                           x = "Number of Steps per Day", y = "Frequency")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

- Mean and Median of Total number of steps per day in New Data Set


```r
mean(df.new.day$tot.steps)
```

```
## [1] 10766.19
```


```r
median(df.new.day$tot.steps)
```

```
## [1] 10766.19
```

Mean and Median in the dataset which has missing values filled in by the interval average are equal and are also equal to the mean of the original data set. However the median of the original data set differs by a value of 1.19 less.

###Difference in activity Patterns, Weekends and Weekdays

- Adding a new factor variable column

```r
df.new$date<-as.Date(df.new$date)
df.new$day.type<-weekdays(df.new$date)
df.new$day.type[df.new$day.type=="Monday"]<-"Weekday"
df.new$day.type[df.new$day.type=="Tuesday"]<-"Weekday"
df.new$day.type[df.new$day.type=="Wednesday"]<-"Weekday"
df.new$day.type[df.new$day.type=="Thursday"]<-"Weekday"
df.new$day.type[df.new$day.type=="Friday"]<-"Weekday"
df.new$day.type[df.new$day.type=="Saturday"]<-"Weekdend"
df.new$day.type[df.new$day.type=="Sunday"]<-"Weekdend"
df.new$day.type <- as.factor(df.new$day.type)
```

- timeseries plot with intervals averaged by weekdays and weekend days

```r
df.average.type<-df.new %>%
    group_by(day.type, interval)%>%
    summarise(average.steps=mean(steps))
df.average.type
```

```
## Source: local data frame [576 x 3]
## Groups: day.type
## 
##    day.type interval average.steps
## 1   Weekday        0    2.25115304
## 2   Weekday        5    0.44528302
## 3   Weekday       10    0.17316562
## 4   Weekday       15    0.19790356
## 5   Weekday       20    0.09895178
## 6   Weekday       25    1.59035639
## 7   Weekday       30    0.69266247
## 8   Weekday       35    1.13794549
## 9   Weekday       40    0.00000000
## 10  Weekday       45    1.79622642
## ..      ...      ...           ...
```

```r
xyplot(average.steps ~ interval | day.type, df.average.type, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
