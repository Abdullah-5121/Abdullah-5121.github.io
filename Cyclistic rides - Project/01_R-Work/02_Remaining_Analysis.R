library(tidyverse)
library(ggplot2)
library(readxl)
library(writexl)

#Import All Trip File :
all_trips <- read_excel("All_Trip_File/all_trips.xlsx")
View(all_trips)

#For Inspecting the structure :
str(all_trips)

#There are some entries when bike was taken by head quarter or ride_length was
#less then zero so we have to remove it 
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0) , ]
View(all_trips_v2[all_trips_v2$start_station_name == "HQ QR" ,]) #We can see that it is successfully removed

all_trips_v2 <- mutate(all_trips_v2,ride_length_min = round(ride_length/60,2))
glimpse(all_trips_v2)
#To find Avg , median , max and min on all_trips_v2 :
summary(all_trips_v2$ride_length)#Seconds
summary(all_trips_v2$ride_length_min)#minutes


#Compare Members & Casual users :
aggregate(all_trips_v2$ride_length_min ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length_min ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length_min ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length_min ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length_min ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#Analyze ridership data by type and week days :
summary <-
all_trips_v2 %>% 
  mutate(day_of_week = factor(day_of_week,
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                              ordered = TRUE)) %>%
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual)
write_xlsx(all_trips_v2 , "All_Trip_File/all_trips_v2.xlsx")
write.csv(summary,"summary.csv")

##Continued ::
