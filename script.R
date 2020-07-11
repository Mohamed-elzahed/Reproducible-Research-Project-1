# What is mean total number of steps taken per day?
data <- read.csv('repdata_data_activity/activity.csv')
data$date <- as.Date(data$date)
library(reshape2)
melted_data <- melt(data, id=c('date', 'interval'))
steps_per_day <- dcast(melted_data, date~variable, sum, na.rm=T)
hist(steps_per_day$steps, 
     xlab = 'Steps', ylab='The number of days', 
     main='The total number of steps taken each day ')
dev.copy(png, 'TheTotalNumberOfSteps.png')
dev.off()
mean(steps_per_day$steps)
median(steps_per_day$steps)

# What is the average daily activity pattern?
avg_steps_across_days <- dcast(melted_data, interval~variable,
                               mean, na.rm=T)
with(avg_steps_across_days,
     plot(interval, steps, type = 'l',
          xlab='Time Interval', ylab='Average Steps',
     main ="Average steps taken over all days vs time interval"))
dev.copy(png, 'Average steps taken over all days vs time interval.png')
dev.off()
maxsteps_interval <- 
        avg_steps_across_days$interval[
              which.max(avg_steps_across_days$steps)]

# Imputing missing values
clean <- data[!is.na(data$steps),]
melted_clean <- melt(clean[,-2],id=c('day','interval'))
avgTables <- dcast(melted_clean, day+interval~variable, mean)
nas <- data[is.na(data$steps),]
cleaned_data <- merge(nas, avgTables, by=c('day', 'interval'))
cleaned_data <- cleaned_data[,c(5,4,2,1)]
names(cleaned_data) <- c('steps', 'date', 'interval', 'day')
complete_data <- rbind(clean, cleaned_data)
melted_complete <- melt(complete_data, id=c('date', 'interval', 'day'))
melted_complete$value <- as.numeric(melted_complete$value)
casted_complete <- dcast(melted_complete, date~variable, sum)
hist(casted_complete$steps, col = 'grey', ann=F)
#hist(steps_per_day$steps, col = 'grey', add=T)
title(xlab='Steps', ylab='Frequency', 
      main = "Total Steps per Day with NAs Fixed")
# legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
dev.copy(png, 'Total Steps per Day with NAs Fixed.png')
dev.off()
complete_data$dayType <- ifelse(complete_data$day %in% c('Sat', 'Sun'),
                                'Weekend', 'Weekday')
library(lattice)
melted_complete <- melt(complete_data, id=c('date', 'interval', 'day','dayType'))
casted_complete <- dcast(melted_complete,dayType+interval~variable, mean)

xyplot(steps~interval|dayType, data=casted_complete, type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
dev.copy(png,'Average Steps per Interval Based on Type of Day.png')
dev.off()




















