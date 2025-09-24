######################
#Cleaning Dataset
######################
library(tidyverse)
library(readr)
library(janitor)
#Loading datasets :
daily_activity <- read.csv("E:/06_R_Work/01_CaseStudies/03_Casestudy 3/01_Datasets/dailyActivity_merged.csv")

#Examining the dataset :
View(daily_activity)
head(daily_activity)
glimpse(daily_activity)
colnames(daily_activity)


#TO check how much unique rows exist vs how much rows are :
n_distinct(daily_activity)
count(daily_activity)

#To check if null values exist :
sum(is.na(daily_activity))

#Converting date column in  dataset to appropriate date datatype :
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate , format = "%m/%d/%Y")

#Cleaning column_names :
daily_activity <- clean_names(daily_activity)

glimpse(daily_activity)

write.csv(daily_activity , "E:/06_R_Work/01_CaseStudies/03_Casestudy 3/01_daily_activity_cleaned.csv")


########################
#Analysis Part :
########################
library(tidyverse)
library(dplyr)
library(skimr)
library(readr)
library(ggplot2)

#Loading the cleaned Datasets :
daily_activity <- read_csv("E:/06_R_Work/01_CaseStudies/03_Casestudy 3/01_daily_activity_cleaned.csv")

#Inspecting :
View(daily_activity)
glimpse(daily_activity)
skim_without_charts(daily_activity)
colnames(daily_activity)

##############################
#General Overview :
##############################
n_distinct(daily_activity$id) #33 users
nrow(daily_activity)
#TO check how much days per user records are stored :
users <- daily_activity %>% 
  group_by(id) %>% 
  summarize(records = n())
mean(users$records) # 28 days per users on average

##############################
#Activity levels :
##############################
mean(daily_activity$total_steps) #7637.911
mean(daily_activity$sedentary_minutes) #991.2106
mean(daily_activity$calories) #2303.61
summary(daily_activity$total_steps)


##############################
#Inspecting Relationships :
##############################

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Totel Steps vs Calories Burn Visualization:
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

ggplot(daily_activity , aes(x = total_steps , y = calories)) +
  geom_point() +
  geom_smooth(method = lm , se = TRUE) +
  labs(title = "Total Steps vs Calories Burn" , x = "Total Steps" , y = "Calories Burn") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold")) 


#The more users walk the more they burn calories

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Sedentary Minutes vs Calories Burn Visualization:
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ggplot(daily_activity , aes(x = sedentary_minutes , y = calories)) +
  geom_point() +
  geom_smooth(method = lm , se = TRUE) +
  labs(title = "Sedentary Minutes vs Calories Burn" , x = "Sedentary Minutes" , y = "Calories Burn") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold"))
#More the sedentary_minutes Low the calories are burned

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Very Active Distance vs Calories Burn Visualization:
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ggplot(daily_activity , aes(x = very_active_distance , y = calories)) +
  geom_point() +
  geom_smooth(method = lm , se = TRUE) +
  labs(title = "Actively Covered Distance vs Calories" , x = "Actively Covered Distance" , y = "Calories Burn") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold"))

cor(daily_activity$very_active_distance, daily_activity$calories, use = "complete.obs") #0.49
#It gives moderate positive correlation → the farther people move at high intensity, the more calories they burn.

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Actively Spent Mins vs Calories Burn Visualization:
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ggplot(daily_activity , aes(x = very_active_minutes , y = calories )) +
  geom_jitter() +
  geom_smooth(method = lm , se = TRUE) +
  labs(title = "Actively Spent Minutes vs Calories Burn" , x = "Actively Spent Minutes" , y =  "Calories Burn") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold")) 

cor(daily_activity$very_active_minutes, daily_activity$calories, use = "complete.obs") #0.61
#It gives stronger positive correlation → time spent at high intensity explains calories burned even better than distance.

##############################
#Visualizing Acitve , Fair , Lighter Distance & Minutes with calories :
##############################

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Calories VS Different types of Distane
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

distance_long <- daily_activity %>% 
  select(c(id,calories,very_active_distance , moderately_active_distance , light_active_distance)) %>% 
  pivot_longer(cols = c(very_active_distance , moderately_active_distance , light_active_distance),
               names_to = "distance_type",
               values_to = "distance")
View(distance_long)

ggplot(distance_long, aes(x = distance, y = calories, color = distance_type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE,colour = "black" ) +
  facet_wrap(~distance_type , labeller = as_labeller(c(
    c("light_active_distance" = "Lightly Active Distance" , 
      "moderately_active_distance" = "Fairly Active Distance" , 
      "very_active_distance" = "Very Active Distance")
  ))) +
  labs(title = "Calories vs Different Types of Distance",
       x = "Distance (miles)",
       y = "Calories Burned" ,
       color = " ") +
  scale_colour_discrete(labels = c("light_active_distance" = "Lightly Active Distance" , 
                                   "moderately_active_distance" = "Fairly Active Distance" , 
                                   "very_active_distance" = "Very Active Distance")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold")) 


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Calories VS Different types of Minutes
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
minutes_long <- daily_activity %>% 
  select(c(id,calories,very_active_minutes , fairly_active_minutes , lightly_active_minutes)) %>% 
  pivot_longer(cols = c(very_active_minutes , fairly_active_minutes , lightly_active_minutes),
               names_to = "minutes_type",
               values_to = "minutes")
View(minutes_long)

#Creating levels so that visualiztions will be in Sequence 
minutes_long$minutes_type <- factor(
  minutes_long$minutes_type,
  levels = c("lightly_active_minutes", "fairly_active_minutes", "very_active_minutes")
)
 
ggplot(minutes_long, aes(x = minutes, y = calories, color =minutes_type)) +
  geom_jitter(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE,colour = "black" ) +
  facet_wrap(~minutes_type , labeller = as_labeller(c(
    "lightly_active_minutes" = "Lightly Active Minutes" , 
    "fairly_active_minutes" = "Fairly Active Minutes" , 
    "very_active_minutes" = "Very Active Minutes"
  ))) +
  labs(title = "Calories vs Different Types of Minutes Spent",
       x = "Time (minutes)",
       y = "Calories Burned" , 
       color = " ") +
  scale_colour_discrete(labels = c("lightly_active_minutes" = "Lightly Active Minutes" , 
                                   "fairly_active_minutes" = "Fairly Active Minutes" , 
                                   "very_active_minutes" = "Very Active Minutes"))+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold" , size = 8))

#######################################################
#Extracting Days to compare each day work efficiently :
#######################################################
colnames(daily_activity)
daily_activity <- daily_activity %>% 
  mutate(day_of_week = wday(activity_date, label = TRUE , abbr = FALSE))
View(daily_activity)


daily_activity %>% 
  group_by(day_of_week) %>% 
  summarize(avg_steps = mean(total_steps))

daily_activity$day_of_week <- factor(
  daily_activity$day_of_week ,
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Total Steps Covered Each Day by Users :
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

ggplot(daily_activity , aes(x = day_of_week , y = total_steps,fill = day_of_week)) +
  geom_col() +
  labs(title = "Total Steps Coverd Each Day" , x = "Days" , y = "Total Steps") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold", size = 8) ,
        legend.position = "none")
#More steps on Tuesday and least steps on Sunday

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Avg Calories Burned Each Day :
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
glimpse(daily_activity)
min <- daily_activity %>% 
  group_by(day_of_week) %>% 
  summarize(Average_Calories_Burn = mean(calories))
ggplot(min , aes(x = day_of_week , y = Average_Calories_Burn ,fill = day_of_week)) +
  geom_col() +
  labs(title = "Average Calories Burned Each Day" ,x = "Days" , y = "Calories Burned") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold" , size = 8) ,
        legend.position = "none")
          
#Almost same for every Day 


#######################################
#Classifying Users Based on there Average Steps:
#######################################
user_summary <- daily_activity %>% 
  group_by(id) %>% 
  summarize(avg_steps = mean(total_steps) ,
            avg_calories = mean(calories)) %>% 
  mutate(user_type = case_when(
    avg_steps < 5000 ~ "Sedentary" , 
    avg_steps >= 5000 & avg_steps < 7500 ~ "Lightly Active" ,
    avg_steps >= 7500 & avg_steps < 10000 ~ "Fairly Active" ,
    avg_steps >=10000 ~ "Very Active"
  ))
View(user_summary)
user_summary %>% 
  count(user_type)


final_summary <- user_summary %>%
  group_by(user_type) %>%
  summarize(
    user_count = n(), 
    average_steps = mean(avg_steps) 
  )

final_summary$user_type <- factor(final_summary$user_type, 
                                  levels = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"))


ggplot(final_summary, aes(x = user_type, y = average_steps, fill = user_type)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(user_count, " Users")), 
    vjust = 1.5,      # Adjusts vertical position to be inside the bar
    color = "black",  # Sets text color to white for better contrast
    fontface = "bold",
    size = 3) +
  labs(
    title = "User Activity Analysis",
    subtitle = "Average steps and number of users for each activity type",
    x = "User Activity Type",
    y = "Average Daily Steps"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold" , hjust = 0.5) ,
    plot.subtitle = element_text(hjust = 0.5) ,
          axis.text.x = element_text(face = "bold", size = 8) ,
    legend.position = "none") # Hide the legend as it's redundant
View(daily_activity)
colnames(daily_activity)

##########################
#Exproting CSV For Tableau
##########################
tableau_ready <- daily_activity %>% 
  select(-c(...1,tracker_distance,logged_activities_distance))
View(tableau_ready)
write.csv(tableau_ready,"Tableau_Ready.csv")
