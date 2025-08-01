library(ggplot2)
library(scales)
library(tidyverse)

#Importing summary dataset :
summary <- read.csv("All_Trip_File/summary.csv")
head(summary)

##To make week days in-order :

summary <- all_trips_v2 %>%
  mutate(day_of_week = factor(day_of_week,
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                              ordered = TRUE)) %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length),
            .groups = "drop")

#Visualize number of rides by user type
ggplot(data = summary, aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(title = "Number of Rides by User Type and Weekday",
       x = "Day of Week",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal()

summary <- mutate(summary,average_duration_min = (average_duration / 60))
summary
ggplot(data = summary, aes(x = day_of_week, y = average_duration_min, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  labs(title = "Average duration by User Type and Weekday",
       x = "Day of Week",
       y = "Average Duration in minutes",
       fill = "User Type") +
  theme_minimal()
write.csv(summary , "summary.csv")
