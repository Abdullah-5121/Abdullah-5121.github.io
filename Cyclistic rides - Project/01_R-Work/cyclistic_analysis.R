#Loading Libraries :
library(tidyverse)
library(conflicted)
library(readxl)
#To remain safe from conflicts :
conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")

#Importing data :
d1_2019 <- read_excel("E:/Data Analysis/Program # 8/Module # 1/CaseStudy 1/Prepared_Data/01_Cyclistic_2019(excel_processed).xlsx")
d2_2020 <- read_excel("E:/Data Analysis/Program # 8/Module # 1/CaseStudy 1/Prepared_Data/02_Cyclistic_2020(excel_processed).xlsx")
glimpse(d1_2019)
glimpse(d2_2020)
colnames(d1_2019)
colnames(d2_2020)

#Renaming col_names to join both datasets :
(d1_2019 <- rename(d1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

#Now that coloumns which have same characteristics are same now :
glimpse(d1_2019)
glimpse(d2_2020)

#As we can notice that ride_id,readable_type in 2019 is dbl while in 2020 it is chr 
#we can change the ride_id of 2019 to character so that they can fit properly
#To change it we can use :

d1_2019 <-mutate(d1_2019 , ride_id = as.character(ride_id), rideable_type = as.character(rideable_type ))

#Now to combine both data_Frames :
all_trips <- bind_rows(d1_2019 , d2_2020)
glimpse(all_trips)

#To drop unnecessary columns :
all_trips_v2 <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,tripduration))
glimpse(all_trips_v2)

all_trips <- all_trips_v2
#Now our data is allset in one big dataframe now we have to clean the data :

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame? #Rows & Columns
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips) 
glimpse(all_trips)
View(all_trips)

#To see how many observations fall under each usertype
table(all_trips$member_casual)

#Subscriber and members both are same Customer and casual are same
#So we make our data consistent by replacing sub with mem :
all_trips <- mutate(all_trips , 
                    member_casual = recode(member_casual , 
                                                       "Subscriber" = "member" ,
                                                       "Customer" = "casual"))
glimpse(all_trips)
table(all_trips$member_casual)

# Extract date, month, year, day of week
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(all_trips$started_at, "%m")
all_trips$year <- format(all_trips$started_at, "%Y")
all_trips$day_of_week <- weekdays(all_trips$started_at)


View(all_trips)
table(all_trips$day_of_week)

#To calculate difference from start time and End time :
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at )
glimpse(all_trips)

###Continued

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

