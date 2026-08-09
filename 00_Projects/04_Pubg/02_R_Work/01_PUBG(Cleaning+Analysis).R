##########################
#Cleaning Dataset :
##########################
library(tidyverse)
library(readr)
library(janitor)
library(dplyr)

#Loading the Dataset : 
pubg_dataset <- read.csv("E:/06_R_Work/01_CaseStudies/04_Casestudy 4/01_Dataset/PUBG_PC_Dataset.csv")
glimpse(pubg_dataset)
View(pubg_dataset)
colnames(pubg_dataset)

#Inspecting for nulls :
sum(is.na(pubg_dataset)) #Only 1 Null value
pubg_dataset <- na.omit(pubg_dataset)

#Cleaning column names :
pubg_dataset <- clean_names(pubg_dataset)
colnames(pubg_dataset)

#Checking Unique vs Total rows : 
count(pubg_dataset) #4446965 rows
n_distinct(pubg_dataset) #Each row is distinct

#Checking Datatype of Columns :
str(pubg_dataset) #Everything is correct
View(pubg_dataset)

#Removing Impossible values Exist :
pubg_dataset <- pubg_dataset %>% 
  filter(kills >= 0,)

#Converting the win_place_per from 0-1 to 0-100 :
pubg_dataset <- pubg_dataset %>%
  mutate(win_place_perc = win_place_perc * 100)


#To check if any Invalide Entry Exists :
invalid_summary <- pubg_dataset %>%
  summarise(
    invalid_assists = sum(assists < 0),
    invalid_boosts = sum(boosts < 0),
    invalid_damage_dealt = sum(damage_dealt < 0),
    invalid_dbnos = sum(dbn_os < 0),
    invalid_headshot_kills = sum(headshot_kills < 0),
    invalid_heals = sum(heals < 0),
    invalid_kills = sum(kills < 0),
    invalid_kill_streaks = sum(kill_streaks < 0),
    invalid_longest_kill = sum(longest_kill < 0),
    invalid_revives = sum(revives < 0),
    invalid_team_kills = sum(team_kills < 0),
    invalid_road_kills = sum(road_kills < 0),
    invalid_vehicle_destroys = sum(vehicle_destroys < 0),
    invalid_weapons_acquired = sum(weapons_acquired < 0),
    invalid_walk_distance = sum(walk_distance < 0),
    invalid_ride_distance = sum(ride_distance < 0),
    invalid_swim_distance = sum(swim_distance < 0),
    invalid_match_duration = sum(match_duration <= 0),
    invalid_max_place = sum(max_place <= 0),
    invalid_num_groups = sum(num_groups <= 0),
    invalid_win_place_perc = sum(win_place_perc < 0 | win_place_perc > 1)
  )

View(invalid_summary)
#Dataset is already Cleaned and organized 

#Now selecting only useful columns for our analysis :
pubg_dataset <- pubg_dataset %>%
  select(
    assists, boosts, damage_dealt, dbn_os, headshot_kills, heals,
    kills, kill_streaks, longest_kill, revives,
    ride_distance, walk_distance, swim_distance,
    team_kills, road_kills, vehicle_destroys,
    weapons_acquired, match_type, win_place_perc ,rank_points
  )
colnames(pubg_dataset) #8 columns Removed

#Combining the distance columns :
pubg_dataset <- pubg_dataset %>%
  mutate(total_distance = ride_distance + walk_distance + swim_distance)

##########################
#Analysis part:
##########################

library(ggplot2)
library(dplyr)

View(pubg_dataset)


##############################################################
#Finding the average kills and winning percntage by match type
##############################################################
summary<- pubg_dataset %>% 
  group_by(match_type) %>% 
  summarize(avg_kills = mean(kills) , avg_wining_per = mean(win_place_perc))
summary[which.max(summary$avg_kills),]
summary[which.max(summary$avg_wining_per),]
#Most avg kills players can do are in Solo(tpp-fpp) i.e: 7.51   & Also have more winning chances of 56%

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Average Kills by Match Type :
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ggplot(summary, aes(x = reorder(match_type, -avg_kills), y = avg_kills, fill = match_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_kills, 2)),  # show values rounded to 2 decimals
           vjust = -0.5,                      # position slightly above bars
           size = 2.8) +
  labs(title = "Average Kills by Match Type", x = "Match Type", y = "Average Kills") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1) , legend.position = "none")
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Average Wining percentage by Match Type :
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ggplot(summary , aes(x = reorder(match_type, -avg_wining_per),y = avg_wining_per , fill = match_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_wining_per, 2)),  # show values rounded to 2 decimals
            vjust = -0.5,                      # position slightly above bars
            size = 2.8) +
  labs(title = "Average Wining percentage by Match Type", x = "Match Type", y = "Average Winning Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),legend.position = "none")


################################################
#Relationship between Kills and Win percentage : 
################################################


#Created a random sample from the bigger dataset of 50k rows :
pubg_sample_data <- read.csv("E:/06_R_Work/01_CaseStudies/04_Casestudy 4/pubg_sample_dataset.csv")

#Removing the outliars from sample dataset :
pubg_filtered_dataset <- pubg_sample_data %>% 
  filter(kills<=25 , rank_points <=3000 , damage_dealt <=3000)

cor(pubg_filtered_dataset$kills , pubg_filtered_dataset$win_place_perc , use = "complete.obs") 
#0.4225429
#Shows a moderate +ve Corelation (More kills tends to have higher chances of win)

ggplot(pubg_filtered_dataset, aes(x = win_place_perc, y = kills)) +
  geom_point(alpha = 0.3 , size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),linewidth = 1.5, se = FALSE) +
  theme_minimal()
#Shows a trend line of kills influencing wining percentage
#Means Higher the kills are the more winning percentage will be

#############################################################
#Kills , damage and covered distance influence rank points :
#############################################################
#Checking corelation from the main dataset
cor(pubg_dataset$kills , pubg_dataset$rank_points) #0.008199408
cor(pubg_dataset$damage_dealt , pubg_dataset$rank_points) #-0.001459623
cor(pubg_dataset$total_distance , pubg_dataset$rank_points) #0.03389147
#We can clearly see that there is a very low corelation for thses matrices with rank points

library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)
colnames(pubg_sample_data)
glimpse(pubg_sample_data)

#$$$$$$$$$$$$$$$$$$$$
#Visualizing Findings
#$$$$$$$$$$$$$$$$$$$$

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Kills vs Wining Place Percentage :
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
p1 <- ggplot(pubg_filtered_dataset , aes(x = rank_points , y = kills)) +
  geom_point(alpha = 0.3 , size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),linewidth = 1.5,color = "red", se = FALSE) +
  labs(title = "How kills influence wining chances" , x = "Winning Chances" , y= "Kills") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold")) 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Damage vs Winig Place Percentage :
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
p2 <- ggplot(pubg_filtered_dataset , aes(x = rank_points , y = damage_dealt)) +
  geom_point(alpha = 0.3 , size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),linewidth = 1.5,color = "red", se = FALSE) +
  labs(title = "How damage influence wining chances" , x = "Winning Chances" , y= "Damage Dealt") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold")) 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Traveled Distance vs Winnig Place Percentage :
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
p3 <- ggplot(pubg_filtered_dataset , aes(x = rank_points , y = total_distance)) +
  geom_point(alpha = 0.3 , size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),linewidth = 1.5,color = "red", se = FALSE) +
  labs(title = "How traveled distance influence wining chances" , x = "Winning Chances" , y= "Distance Travelled") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold")) 

#$$$$$$$$$$$$$$$$$$$$
#Creating Dashboard :
#$$$$$$$$$$$$$$$$$$$$
library(patchwork)
(p1 | p2) / p3 +
  plot_annotation(title = "Influence of Player Metrics on Rank Points",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)))

#Insight:
#Player performance metrics (kills, damage dealt, and distance traveled) show very
#weak correlation with rank points.
#This indicates that ranking is influenced by a broader scoring system — 
#including survival duration, placement, consistency, and game-specific algorithms
#rather than just raw performance.  


###########################################################################
#Does Weapons Acquired have any direct relationship with Winning Percentage
###########################################################################

glimpse(pubg_filtered_dataset)
cor(pubg_filtered_dataset$weapons_acquired, pubg_filtered_dataset$win_place_perc)
#0.583444
#Shows a Strong Positive Corelation
max(pubg_filtered_dataset$weapons_acquired)


pubg_filtered_dataset <- pubg_filtered_dataset %>%
  mutate(
    weapon_bins = cut(
      weapons_acquired,
      breaks = c(-1, 2, 5, 10, 20, 50, 100),   # start from -1 so 0 is included
      labels = c("0-2", "3-5", "6-10", "11-20", "21-50", "51-100"),
      include.lowest = TRUE,
      right = TRUE
    )
  )
table(is.na(pubg_filtered_dataset$weapon_bins))
ggplot(pubg_filtered_dataset, aes(x = weapon_bins, y = win_place_perc)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Winning Percentage by Weapons Acquired",
       x = "Weapons Acquired (Binned)", y = "Winning Percentage") +
  theme_minimal()


summary(pubg_filtered_dataset$weapons_acquired)

#Insights :
#Players who collect 11–20 weapons have the highest win-place percentage, meaning they balance looting and combat effectively.
#Too few weapons (<5) → poor performance (less equipped).
#Too many weapons (>20) → slight drop, likely due to over-looting and less focus on survival.


############################################
#Categorizing players based on damage delth
############################################
pubg_filtered_dataset$player_type <- cut(
  pubg_filtered_dataset$damage_dealt,
  breaks = c(-Inf, 85, 187, Inf),
  labels = c("Noob", "Average", "Pro")
)
aggregate(win_place_perc ~ player_type, data = pubg_filtered_dataset, mean)

summary <- pubg_filtered_dataset %>% 
  group_by(player_type) %>% 
  summarize(win_chance = mean(win_place_perc) )

ggplot(summary, aes(x = player_type, y = win_chance, fill = player_type )) +
  geom_col(width = 0.6,show.legend = FALSE ) +
  labs(
    title = "Players Types Categorized Based on Damage Dealt",
    subtitle = "Comparison of Winning Chances by Player Type",
    x = "Player Type",
    y = "Winning Chance (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(face = "bold")
  )


#####################
#Tableau CSV :
#####################
write.csv(pubg_filtered_dataset, "E:/06_R_Work/01_CaseStudies/04_Casestudy 4/cleaned_pubg_dataset_Tableau.csv", row.names = FALSE)


  
