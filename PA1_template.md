---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




```r
    library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
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

```r
    library(ggplot2)
    library(missForest)
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## Loading required package: foreach
```

```
## Loading required package: itertools
```

```
## Loading required package: iterators
```

```r
    library(chron)
```

```
## NOTE: The default cutoff when expanding a 2-digit year
## to a 4-digit year will change from 30 to 69 by Aug 2020
## (as for Date and POSIXct in base R.)
```

```
## 
## Attaching package: 'chron'
```

```
## The following object is masked from 'package:foreach':
## 
##     times
```

```
## The following objects are masked from 'package:lubridate':
## 
##     days, hours, minutes, seconds, years
```

```r
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile="data.zip")
    unzip("data.zip")
    data<-read.csv("activity.csv")
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




## What is mean total number of steps taken per day?

```r
    #changing date from char  to date class then subsetting 
    data[,2]<-ymd(data[,2])
    plot1<- data %>% group_by(data[,2])%>% summarise(sum=sum(steps,na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
    plot1<-as.data.frame(plot1)
    hist(plot1$sum,
     main="TOTAL STEPS PER DAY",
     xlab="Date",
     col="red")
```

![](PA1_template_files/figure-html/stepperday_1-1.png)<!-- -->
**SUMMARY OF DATA - MEAN AND MEDIAN**

```r
    #mean and median 
    summary(plot1$sum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```




## What is the average daily activity pattern?

```r
    tseries<-data%>% group_by(interval)%>%summarise(avg=mean(steps,na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
    tseries<-as.data.frame(tseries)
    ggplot(tseries,aes(interval,avg))+geom_line(col="red")
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->


```r
   tseries[which.max(tseries$avg),]
```

```
##     interval      avg
## 104      835 206.1698
```




## Imputing missing values

```r
   sum(is.na(data))
```

```
## [1] 2304
```

```r
   imputed<-data%>% group_by(interval)%>%summarise(mi=mean(steps,na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
   imputed<-as.data.frame(imputed)
   imputed[,2]<-round(imputed[,2])
   bata<-data
   for(i in 1:nrow(bata))
   {
        if(is.na(bata[i,1]))
        bata[i,1]<-imputed[which(imputed[,1]==bata[i,3]),2]
    }
    head(bata)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```
**Ploting the histogram of the imputed data**


```r
    bata[,2]<-ymd(bata[,2])
    pimpu<-bata%>%group_by(bata[,2])%>%summarise(sums=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
    pimpu<-as.data.frame(pimpu)
    hist(pimpu[,2],breaks=5,col="red",xlab="intervals",main="IMPUTED DATA")
```

![](PA1_template_files/figure-html/plotimputed-1.png)<!-- -->
**Imputung the data deacrese biasness in the data**

```r
   summary(pimpu[,2])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10762   10766   12811   21194
```
**we see that mean and median both increases after imputing missing values**

## Are there differences in activity patterns between weekdays and weekends?



```r
    chk<-weekdays(bata$date)
    chk<-as.data.frame(chk)
    names(chk)<-"week"
    chk[which(chk[,1]=="Saturday"|chk[,1]=="Sunday"),1]="weekend"
    chk[which(chk[,1]!="weekend"),1]="weekdays"
```

**Now we will draw the plot** 

```r
    bata<-cbind(bata,chk)
    wplot<-bata%>% group_by(interval,week) %>% summarise(means=mean(steps,na.rm=TRUE))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
    wplot$week <- factor(wplot$week)
    ggplot(wplot,aes(interval,means))+geom_line(col="red")+facet_grid(week~.)
```

![](PA1_template_files/figure-html/weekdraw-1.png)<!-- -->




