#DAMON NG KHAI WENG TP064820
#Hypothesis: The price of properties in Kuala Lumpur is influenced by number of car parks, number of rooms, size, and location.
#Objective: To investigate the relationship between number of rooms and property price.
#import libraries and dataset
require(ggplot2)
require(ggExtra)
require(dplyr)
data <- read.csv("kl_property_data_filtered.csv")

#further cleaning of data
#remove index if it exists
if("X" %in% names(data)){
  data <- data %>% 
    select(-X)
}
names(data) <- tolower(names(data))# lowercase all column names
## exploratory data analysis
nrow(data)#total records
nrow(data) * 0.01# 1 percent of records
# summary of rooms
summary(data)
room.q1 <- quantile(data$rooms, 0.25)
room.q3 <- quantile(data$rooms, 0.75)
room.iqr <- room.q3 - room.q1
room.lower_bound <- room.q1 - 1.5 * room.iqr
room.upper_bound <- room.q3 + 1.5 * room.iqr
room.mean <- mean(data$rooms)
room.median <- median(data$rooms)
room.mode <- as.integer(names(sort(table(data$rooms), decreasing = TRUE))[1])
#summary of price
price.q1 <- quantile(data$price, 0.25)
price.q3 <- quantile(data$price, 0.75)
price.iqr <- price.q3 - price.q1
price.lower_bound <- price.q1 - 1.5 * price.iqr
price.upper_bound <- price.q3 + 1.5 * price.iqr
price.mean <- mean(data$price)
price.median <- median(data$price)
price.mode <- as.integer(names(sort(table(data$price), decreasing = TRUE))[1])










#Analysis 1: How does the number of rooms of a property in KL influence the price of a property?
#check distribution and correlation of price and rooms with count plot and box plot
#check ratio of outliers to total records
outlier_ratio <- data %>%
  mutate(kind = ifelse((between(rooms, room.lower_bound, room.upper_bound)&
                          between(price, price.lower_bound, price.upper_bound)), "normal", "outlier"))%>%
  group_by(kind) %>%
  summarise(count = n(), percentage = round(n()/nrow(data) * 100,2))
outlier_ratio
donutHole = 2
ggplot(outlier_ratio, aes(x=donutHole, y=percentage, fill = kind)) + 
  geom_col() +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  xlim(c(0.2, donutHole+0.5))+
  theme_void() +
  labs(title="Percentage of outliers in the data")


#find the median price of each number of rooms
#find the outliers of number of rooms
outliers_removed <- data %>% #remove outliers
  filter(between(rooms, room.lower_bound, room.upper_bound))%>%
  filter(between(price, price.lower_bound, price.upper_bound))
#find the price of each number of rooms with and without outliers
g<-ggplot(data, aes(x=rooms, y=price)) + 
  geom_point(alpha= 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Overall number of rooms in a property compared to its price", x="Number of rooms", y="Price (RM)")+
  scale_y_continuous(labels = scales::comma_format())+
  theme(text = element_text(size = 18))
ggMarginal(g, type = "boxplot", fill="transparent")
g<-ggplot(outliers_removed, aes(x=rooms, y=price)) + 
  geom_point(alpha= 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Normal number of rooms in a property compared to its price", x="Number of rooms", y="Price (RM)")+
  scale_y_continuous(labels = scales::comma_format())+
  theme(text = element_text(size = 18))
ggMarginal(g, type = "boxplot", fill="transparent")
cor(data$rooms, data$price)
cor(outliers_removed$rooms, outliers_removed$price)
#find the median price of each number of rooms
median_price_to_rooms <- data %>% 
  group_by(rooms) %>% 
  summarise(median_price = median(price)) %>%
  arrange(desc(median_price))
median_price_to_rooms_no_outliers <- outliers_removed %>% 
  group_by(rooms) %>% 
  summarise(median_price = median(price)) %>%
  arrange(desc(median_price))

ggplot(median_price_to_rooms, aes(x=rooms, y=median_price)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Number of rooms in a property compared to its price", x="Number of rooms", y="Price (RM)")+
  geom_text(aes(label = round(median_price,2)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma_format())
ggplot(median_price_to_rooms_no_outliers, aes(x=rooms, y=median_price)) + 
  geom_point(col = "tomato4") + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Normal number of rooms in a property compared to its price", x="Number of rooms", y="Price (RM)")+
  geom_text(aes(label = round(median_price,2)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma_format()) 

#correlation between price and rooms
cor(median_price_to_rooms$median_price, median_price_to_rooms$rooms)
cor(median_price_to_rooms_no_outliers$median_price, median_price_to_rooms_no_outliers$rooms)










#Analysis 2: Are there any special cases (outliers) where specific number of rooms have a lot more expensive price than the average
#find the q3 of price for each number of rooms
price_summary <- data %>% 
  group_by(rooms) %>% 
  summarise(q3 = quantile(price, 0.75), q1 = quantile(price, 0.25), iqr = q3 - q1, upper_bound = q3 + 1.5 * iqr)
#find the upper limit of price for each number of rooms using q3
question2 <- data %>% left_join(price_summary, by = "rooms") %>% 
  mutate(above_upper_bound = ifelse(price > upper_bound, "yes", "no"),)%>%
  group_by(rooms, above_upper_bound)
# Create the stacked bar chart
question2 %>%
  ggplot(aes(x = factor(rooms), fill = above_upper_bound)) +
  geom_bar(stat = "count") +
  labs(title = "Expensive properties by Number of Rooms", x = "Number of Rooms", y = "Total Records")+
  geom_text(aes(label = scales::percent(..count../sum(..count..), accuracy = 1)), stat = "count", position = position_stack(vjust = 0.5))+
  coord_flip()
#find the overPriced properties
question2 %>% 
  filter(above_upper_bound == "yes") %>% 
  group_by(rooms) %>% 
  summarise(count = n(), percentage = round(n()/nrow(data) * 100,2))
#find the number of overpriced properties
sum(question2$above_upper_bound == "yes")










#Analysis 3 Does the presence of extra rooms have any significant effect on the properties price?
#combine property subtypes into one category
#check all unique property types
datadup <- data 
unique(datadup$property.type) %>% 
  sort()
datadup$property.type <- ifelse(grepl("Terrace/Link House", datadup$property.type),"Terrace/Link House", data$property.type)
datadup$property.type <- ifelse(grepl("Cluster House", datadup$property.type),"Cluster House", datadup$property.type)
datadup$property.type <- ifelse(grepl("Condominium", datadup$property.type),"Condominium", datadup$property.type)
datadup$property.type <- ifelse(grepl("Flat", datadup$property.type),"Flat", datadup$property.type)
datadup$property.type <- ifelse(grepl("Residential Land", datadup$property.type),"Residential Land", datadup$property.type)
datadup$property.type <- ifelse(grepl("Townhouse", datadup$property.type),"Townhouse", datadup$property.type)
datadup$property.type <- ifelse(grepl("Serviced Residence", datadup$property.type),"Serviced Residence", datadup$property.type)
datadup$property.type <- ifelse(grepl("Apartment", datadup$property.type),"Apartment", datadup$property.type)
datadup$property.type <- ifelse(grepl("Bungalow Land", datadup$property.type),"Bungalow Land", datadup$property.type)
datadup$property.type <- ifelse(datadup$property.type %in% c("Bungalow (Corner)","Bungalow (Duplex)","Bungalow (EndLot)","Bungalow (Intermediate)","Bungalow (Penthouse)","Bungalow (Triplex)"), "Bungalow", datadup$property.type)
datadup$property.type <- ifelse(grepl("Semi-detached House", datadup$property.type),"Semi-detached House", datadup$property.type)

#does the presence of extra rooms have any affect on price
ggplot(datadup, aes(x=extra_rooms, y=price)) + 
  geom_count(col = "tomato3") + 
  facet_wrap(~property.type) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Extra rooms for each property type compared to its price", x="Extra rooms", y="Price (RM)")+
  scale_y_log10(labels = scales::comma_format())
summary(lm(data$price ~ data$extra_rooms))
cor(data$price,data$extra_rooms)

