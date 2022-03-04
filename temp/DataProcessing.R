library(ggplot2)
library(dplyr)
#download data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "activity.zip")
#read data
dir.create("data/")
unzip("activity.zip",exdir="data/")
datos<-read.csv("data/activity.csv",h=T)
#data frame with steps as integers (lots of missing), dates as factors and interval as int

#What is mean total number of steps taken per day?
#For this part of the assignment, you can ignore the missing values in the dataset.
#Calculate the total number of steps taken per day
totalstepsday<-aggregate(steps~date,datos,sum,na.rm=T)

#If you do not understand the difference between a histogram and a barplot, research the difference between them. 
#Make a histogram of the total number of steps taken each day
hist(totalstepsday$steps,main="Total steps per day",xlab="Total steps per day")
#Calculate and report the mean and median of the total number of steps taken per day
mean(totalstepsday$steps)
median(totalstepsday$steps)
#these can also be obtained with summary(totalstepsday$steps)

##What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") 
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)
agg<-aggregate(steps~interval,datos,mean,na.rm=T)
plot(agg$interval,agg$steps,type="l",xlab="Interval",ylab="Steps")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
agg[which.max(agg$steps),]$interval

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values 
#(coded as NA\color{red}{\verb|NA|}NA). 
#The presence of missing days may introduce bias into some calculations or summaries 
#of the data.
#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)

sum(is.na(datos))

#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.
#i'll use the mean for that interval rounded to the nearest integer
#which we have from agg above
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
datos2=datos
for (i in 1:nrow(datos2)) {
  if(is.na(datos2$steps[i])){
    #print(datos2$interval[i])
    datos2$steps[i]=round(agg[agg$interval==datos2$interval[i],]$steps)
    }
}

#Make a histogram of the total number of steps taken each day and 
#Calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily 
#number of steps?
aggdatos2<-aggregate(steps~date,datos2,sum)
hist(aggdatos2$steps,xlab="Total steps",main="Histogram of total steps per day")
mean(aggdatos2$steps)
median(aggdatos2$steps)

#Are there differences in activity patterns between weekdays and weekends?
#  For this part the weekdays() function may be of some help here. 
#Use the dataset with the filled-in missing values for this part.

#weekdays(as.Date(datos2$date,"%Y-%m-%d"))

#Create a new factor variable in the dataset with two levels – 
#“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

wdays<-weekdays(as.Date(datos2$date,"%Y-%m-%d"))
daytype=factor(wdays,levels=c("weekday","weekend"))
weekend=c("Saturday","Sunday")
weekday=c("Friday","Monday","Tuesday","Wednesday","Thursday","Friday")
for (i in 1:length(wdays)){
  if(wdays[i] %in% weekend){daytype[i]=as.factor("weekend")}
  if(wdays[i] %in% weekday){daytype[i]=as.factor("weekday")}
}

datos2$daytype=daytype

###
#Make a panel plot containing a time series plot 
#(i.e. type = "l") of the 5-minute interval (x-axis) and the 
#average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should look like 
#using simulated data.
stepsday<-aggregate(steps~daytype+interval,datos2,mean)
ggplot(stepsday,aes(interval,steps))+geom_line(colour="blue")+facet_grid(rows=vars(daytype))
p<-ggplot(stepsday,aes(interval,steps))+geom_line(colour="blue")
pp<-p+facet_wrap(~daytype,nrow=2)+theme_bw()+theme(panel.grid=element_blank(),panel.background = element_blank())+theme(strip.background = element_rect(colour="black",fill="salmon"))
pp+ylab("Number of steps")
ggsave("StepsWeekday.png",plot=last_plot())


#ggplot(stepsday,aes(interval,steps))+geom_line(colour="blue")+facet_wrap(~daytype,nrow=2,repeat.tick.labels = c('top','left'))+theme_bw()+theme(panel.grid=element_blank(),panel.background = element_blank())+theme(strip.background = element_rect(colour="black",fill="salmon"))

  
  