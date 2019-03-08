unzip(zipfile = "activity.zip")
data = read.csv("activity.csv", header = TRUE)

library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
q<-qplot(total.steps, binwidth = 1000, 
         ylab = "Count",xlab = "Total steps per day", main ="Number of Steps")

print(q)

mean(total.steps, na.rm = TRUE)
median(total.steps, na.rm = TRUE)

library(ggplot2)
avgAct <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), 
                      FUN = mean, na.rm = TRUE)
g <-ggplot(data = avgAct, aes(x = interval, y = steps)) + geom_line() + xlab("interval") + 
    ylab("Avg number of steps")
print(g)

avgAct[which.max(avgAct$steps), ]
natot <- sum(is.na(data$steps))
print (natot)
data.filled = data

fillval <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) 
        filled <- c(steps) else filled <- (avgAct[avgAct$interval == interval, "steps"])
        return(filled)
}
data.filled <- data
data.filled$steps <- mapply(fillval, data.filled$steps, data.filled$interval)

sum(is.na(data.filled$steps))

plottotal = sapply(split(data.filled$steps, data.filled$date), 
                                    sum)
hist(plottotal, col = "green", ylim = c(0, 50), main = "Number of steps (NAs removed)",xlab = "Total steps/Day")

print(mean(plottotal))
print(median(plottotal))


checkWeek.end <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("Weekday") else if (day %in% c("Saturday", "Sunday")) 
            return("Weekend") 
}
data.filled$date <- as.Date(data.filled$date)
data.filled$day <- sapply(data.filled$date, FUN = checkWeek.end)

avgAct <- aggregate(steps ~ interval + day, data = data.filled, mean)
g2<-ggplot(avgAct, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("Interval") + ylab("Steps") + ggtitle ("Weekday vs Weekend Activity Patterns")
print(g2)
