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
