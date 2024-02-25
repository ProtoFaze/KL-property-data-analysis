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
 