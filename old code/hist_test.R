rest_mean <- sample_train %>% 
  group_by(restaurant_id) %>% 
  summarise(avg_viol = mean(severe_count))

summary(sample_train$severe_count)
hist_test <- sample_valid %>% left_join(rest_mean, by = "restaurant_id")

fit <- lm(severe_count ~ avg_viol, data = hist_test)
summary(fit)
