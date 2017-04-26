library(ggplot2)
library(dplyr)

# tickMarks returns n tick marks used for time axis
tickMarks <- function (intervals, n) {
    intervals[seq(1, length(intervals), length(intervals)/n )]
}

#1
unzip("activity.zip", exdir = "data")
data0 <- read.csv("data/activity.csv", colClasses = c("numeric", "Date", "numeric"))
data0 <- mutate(data0, 
    interval = gsub('^(.{2})(.*)$', #split to 2 groups with 2 numbers each
                    '\\1:\\2', 
                    sprintf("%04d", interval ) )) #add leading zeros
data0$interval <- as.factor(data0$interval)

#2
daysTotalSteps <- data0 %>% 
    group_by(date) %>% 
    summarize(totalSteps = sum(steps))
qplot(totalSteps, data = daysTotalSteps, binwidth=2000)
#3
mean(daysTotalSteps$totalSteps, na.rm = TRUE)
median(daysTotalSteps$totalSteps, na.rm = TRUE)
#4
periodStepsAvg <- data0 %>% 
    group_by(interval) %>% 
    summarise(stepsAvg = mean(steps, na.rm = TRUE))
ggplot(periodStepsAvg, aes(x=interval, y=stepsAvg, group=1)) +
    geom_line() +
    scale_x_discrete(breaks = tickMarks(periodStepsAvg$interval, 12))
#5
periodStepsAvg$stepsAvg[which.max(periodStepsAvg$stepsAvg)]
#6
sum(is.na(data0$steps))
data1 <- mutate(data0, 
    steps = round(ifelse(#replace NAs with steps' average for this inverval
        is.na(steps),
        periodStepsAvg$stepsAvg[which(periodStepsAvg$interval %in% interval)], 
        steps
        )))
#7
#8
data1 <- mutate(data1, 
    daytype = as.factor(ifelse(
        grepl("Sun|Sat", weekdays(date)), 
        "weekend", 
        "weekday")))
weekends <- data1 %>% 
    group_by(daytype, interval) %>% 
    summarize(avg_steps = mean(steps))
ggplot(weekends, aes(x=interval, y=avg_steps, group=daytype))+
    facet_grid(daytype~.)+
    geom_line()+
    scale_x_discrete(breaks = tickMarks (weekends$interval, 24))
