library(data.table)
library(ggplot2)
library(dplyr)
activity_data <- fread("activity.csv")

total_steps <- activity_data[, lapply(.SD, sum, na.rm = FALSE), 
                             .SDcols = c("steps"), by = .(date)]
total_steps %>% ggplot(aes(steps)) +
    geom_histogram(fill = "seagreen2", binwidth = 1000) + 
    labs(x = "Steps", y = "Frequency", title = "Daily steps",
         subtitle = "Number of steps per day of the individual") + 
    theme_minimal()
total_steps[, .(mean_steps = mean(steps, na.rm = TRUE), 
                  median_steps = median(steps, na.rm = TRUE))]

interval_steps <- activity_data[, lapply(.SD, mean, na.rm = TRUE), 
                             .SDcols = c("steps"), by = .(interval)]
interval_steps %>% 
    ggplot(aes(interval, steps)) +
    geom_line(col = "purple", size = 0.5) + 
    labs(title = "Average daily steps", subtitle = "Avg. daily steps per interval") +
    theme_minimal()

interval_steps$interval[which.max(interval_steps$steps)]

sum(is.na(activity_data$steps))

# Replace NA with mean for 5 mins interval
for(i in 1:nrow(activity_data)) {
    if (is.na(activity_data$steps[i])) {
        interval <- activity_data$interval[i]
        activity_data$steps[i] <- 
            interval_steps$steps[which(interval_steps$interval == interval)]
    }
}

data.table::fwrite(activity_data, file = "tidy_data.csv", quote = FALSE)

total_steps_new_data <- activity_data[, c(lapply(.SD, sum)), .SDcols = c("steps"), 
                          by = .(date)]
total_steps_new_data %>% ggplot(aes(steps)) +
    geom_histogram(fill = "seagreen2", binwidth = 1000) + 
    labs(x = "Steps", y = "Frequency", title = "Daily steps",
         subtitle = "Number of steps per day of the individual") + 
    theme_minimal()
total_steps_new_data[, .(mean_steps = mean(steps), median_steps = median(steps))]

# Weekday or weekend
activity_data <- fread("activity.csv")
activity_data[, date := as.POSIXct(date, format = "%Y-%m-%d")][, day_of_week := weekdays(date)]
activity_data[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = day_of_week), "time_of_week"] <- "weekday"
activity_data[grepl(pattern = "Saturday|Sunday", x = day_of_week), "time_of_week"] <- "weekend"
activity_data[, time_of_week := as.factor(time_of_week)]

interval_steps <- activity_data[, c(lapply(.SD, mean, na.rm = TRUE)), 
                                .SDcols = c("steps"), 
                                by = .(interval, time_of_week)]

# for(i in 1:nrow(activity_data)) {
#     if (is.na(activity_data$steps[i])) {
#         interval <- activity_data$interval[i]
#         activity_data$steps[i] <- 
#             interval_steps$steps[which(interval_steps$interval == interval)]
#     }
# }




interval_steps %>%
    ggplot(aes(interval, steps, col = time_of_week)) + 
    geom_line() + 
    labs(title = "Average daily steps by time of week", 
         subtitle = "Avg. daily steps per interval") +
    theme_minimal()

