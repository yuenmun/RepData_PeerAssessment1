activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
suppressMessages(library(dplyr))
totalstep <- 
    activity %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarise(total = sum(steps))
suppressMessages(library(ggplot2))
ggplot(totalstep, aes(x=total)) + 
    geom_histogram(aes(fill = ..count..), binwidth=1000) 


daily <- activity %>%
    group_by(interval) %>%
    summarise(avg_steps = mean(steps, na.rm = T))

ggplot(daily, aes(x = interval, y = avg_steps, group = 3)) + 
    geom_line() + scale_x_discrete(breaks = seq(0, 2500, 500))

activity_noNA <- cbind(activity, daily)
activity_noNA$steps[is.na(activity_noNA$steps)] <- activity_noNA$avg_steps[(is.na(activity_noNA$steps))]

summarydata <- matrix(c(mean(totalstep$total), 
                        mean(activity_alldata$total), 
                        median(totalstep$total), 
                        median(activity_alldata$total)), 
                        nrow=2, 
                        ncol=2)