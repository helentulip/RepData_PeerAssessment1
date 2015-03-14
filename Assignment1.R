rawdata<-read.csv("activity.csv",sep=",",header=TRUE)
day_data<-aggregate(steps~date,data=rawdata,FUN=sum)
hist(day_data$steps,xlab="steps",main="Total Number of Steps Per Day")
mean_step<-mean(day_data$steps)
median_step<-median(day_data$steps)
print(mean_step)
print(median_step)
day_data$step_interval<-day_data$steps/288
plot(day_data$step_interval,type="l", xlab="5-minute interval",ylab="average number of steps")
max<-day_data$date[which.max(day_data$step_interval)]
max
rem_NA<-na.omit(rawdata)
n_NA<-nrow(rawdata)-nrow(rem_NA)
n_NA
interval_data<-aggregate(steps~interval,data=rawdata,FUN=mean)
x<-merge(rawdata,interval_data,by="interval")
for(i in 1:17568){x$steps.x[is.na(x$steps.x)]<-x$steps.y}
day_data2<-aggregate(steps.x~date,data=x,FUN= sum)
head(day_data2)
hist(day_data2$steps.x,xlab="",main="Total Number of Steps Each Day")
mean_step2<-mean(day_data2$steps)
median_step2<-median(day_data2$steps)
print(mean_step2)
print(median_step2)
x$date<-weekdays(as.Date(x$date))
for(i in 1:17568){x$wk<-ifelse(x$date %in% c("Saturday","Sunday"),"weekend","weekday")}
stepsbyday<-aggregate(steps.x ~ interval+wk,data=x,FUN=mean)
library(lattice)
xyplot(steps.x ~ interval|wk,stepsbyday,type="l",layout=c(1,2),xlab="Interval",ylab="Number of steps")
library(knitr)
knit2html("Assignment1.Rmd")