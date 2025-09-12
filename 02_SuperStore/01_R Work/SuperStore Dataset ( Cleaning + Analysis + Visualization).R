library(tidyverse)
library(readr)
library(janitor)

#Loading Dataset :
superstore <- read.csv("Sample - Superstore.csv")

#Inspecting Dataset :
View(superstore)
head(superstore)
colnames(superstore)
str(superstore)
glimpse(superstore)

#Cleaning column names :
superstore<- clean_names(superstore)

#Checking nulls or duplicates if any :
sum(is.na(superstore))
duplicated(superstore)

#Converting date column from chr to date :
superstore$order_date <- as.character(superstore$order_date)
superstore$order_date <- as.Date(superstore$order_date, format = "%m/%d/%Y")
superstore$ship_date <- as.Date(superstore$ship_date , format = "%m/%d/%Y")
superstore_v2 <- superstore
glimpse(superstore_v2)

#making the data in the chr columns lowercase :
glimpse(superstore)
superstore$region <- tolower(superstore$region)
superstore$category <- tolower(superstore$category)
superstore$sub_category <- tolower(superstore$sub_category)

#Removing extra spaces :
superstore$country <- trimws(superstore$country)
superstore$category <- trimws(superstore$category)
glimpse(superstore)
duplicated(superstore)

#Removing unnecessary columns
colnames(superstore)
superstore <- select(superstore , -c(customer_id,order_id,product_id ))

#Creating useful columns for calculation :
superstore <- mutate(superstore, profit_ratio = round(profit/sales,2)*100 )
superstore <- mutate(superstore , days_to_ship = ship_date-order_date)
superstore<- mutate(superstore , revenue = sales*(1-discount))
glimpse(superstore)
superstore_v2 <- superstore
superstore<-superstore_v2

#Saving the file 
write.csv(superstore,"Cleaned_Superstore.csv")

table(superstore$segment)

#Continue :
superstore <- read.csv("E:/06_R_Work/01_CaseStudies/02_CaseStudy  2/02_SuperStore/Cleaned_Superstore.csv")
View(superstore)

#Checking trends :
by_segment <- superstore %>% 
  group_by(segment) %>% 
  summarize(avg_profit_ratio = mean(profit_ratio) , avg_revenue = mean(revenue))

by_region <- superstore %>% 
  group_by(region) %>% 
  summarize(avg_profit_ratio = mean(profit_ratio) , avg_revenue = mean(revenue))

by_category <- superstore %>% 
  group_by(category) %>% 
  summarize(avg_profit_ratio = mean(profit_ratio) , avg_revenue = mean(revenue))

by_ship_mode <- superstore %>% 
  group_by(ship_mode) %>% 
  summarize(avg_profit_ratio = mean(profit_ratio) , avg_revenue = mean(revenue))

#Checking :
head(by_segment)
head(by_region)
head(by_category)
head(by_ship_mode)


#Converting data that can be visualized more precisely
by_segment_long <- by_segment %>%
  pivot_longer(cols = c(avg_profit_ratio, avg_revenue),
               names_to = "metric",
               values_to = "value")
head(by_segment_long)

by_region_long <- by_region %>% 
  pivot_longer(cols = c(avg_profit_ratio , avg_revenue) ,
               names_to = "metric" ,
               values_to = "value")
head(by_region_long)

by_category_long <- by_category %>% 
  pivot_longer(cols = c(avg_profit_ratio , avg_revenue) ,
               names_to = "metric" ,
               values_to = "value")
head(by_category_long)

by_ship_mode_long <- by_ship_mode %>% 
  pivot_longer(cols = c(avg_profit_ratio , avg_revenue) ,
               names_to = "metric" ,
               values_to = "value")
head(by_ship_mode_long)

#Visualizing data to understand more easily :
library(ggplot2)

#By Segment :
ggplot(by_segment_long, aes(x = segment, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Profit Ratio vs Revenue by Segment",x = "Segment" ,y = "Profit Ratio / Revenue", fill = "Metric" ) +
  scale_fill_manual(values = c("avg_profit_ratio" = "green","avg_revenue" = "blue"),labels = c("avg_profit_ratio" = "Profit Ratio (%)","avg_revenue" = "Revenue ($)")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold", size = 8)) 
ggsave("01_By Segment.png", plot = last_plot(), width = 8, height = 6, dpi = 300 , bg = "white")


#By region :
ggplot(by_region_long , aes(x = region , y = value , fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Profit Ratio vs Revenue by Region" , x = "Region" , y = "Profit Ratio / Revenue") +
  scale_fill_manual(values = c("avg_profit_ratio" = "green" , avg_revenue = "blue") , labels = c("avg_profit_ratio" = "Profit Ratio (%)" , "avg_revenue" = "Revenue ($)")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold", size = 8)) 
ggsave("02_By Region.png", plot = last_plot(), width = 8, height = 6, dpi = 300 , bg = "white")

#By Category :
ggplot(by_category_long , aes(x = category , y = value , fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Profit Ratio vs Revenue by Category" , x = "Category" , y = "Profit Ratio / Revenue") +
  scale_fill_manual(values = c("avg_profit_ratio" = "green" , avg_revenue = "blue") , labels = c("avg_profit_ratio" = "Profit Ratio (%)" , "avg_revenue" = "Revenue ($)")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold", size = 8)) 
ggsave("03_By Cayegory.png", plot = last_plot(), width = 8, height = 6, dpi = 300 , bg = "white")
 
#By Ship_Mode :
ggplot(by_ship_mode_long , aes(x = ship_mode , y = value , fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Profit Ratio vs Revenue by Ship Mode" , x = "Ship Mode" , y = "Profit Ratio / Revenue") +
  scale_fill_manual(values = c("avg_profit_ratio" = "green" , avg_revenue = "blue") , labels = c("avg_profit_ratio" = "Profit Ratio (%)" , "avg_revenue" = "Revenue ($)")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold" , hjust = 0.5) ,
        axis.text.x = element_text(face = "bold", size = 8)) 
ggsave("04_Ship Mode.png", plot = last_plot(), width = 8, height = 6, dpi = 300, bg = "white")

