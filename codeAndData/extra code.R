##extra code

#presented in faceted scatter plot
ggplot(datadup, aes(x=factor(rooms), y=price, color = property.type)) + 
  geom_point() + 
  facet_wrap(~property.type) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma_format())+
  labs(title="Number of rooms in a property compared to its price", x="Number of rooms", y="Price (RM)")
#does extra rooms have any correlation with price
cor(data$extra_rooms, data$price)


#	find the special cases where there are a lot more or less rooms than the average
data %>% 
  group_by(rooms) %>% 
  summarise(mean_price = mean(price), median_price = median(price), min_price = min(price), max_price = max(price), count = n()) %>% 
  filter(rooms < 3 | rooms > 10) %>% 
  arrange(desc(count))

#percentage of the data that is considered an outlier
#find the number of records for each number of rooms outside of the normal range
outliers = data %>% 
  filter(!between(rooms, room.lower_bound,room.upper_bound)) 
outliers %>% 
  group_by(rooms) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

