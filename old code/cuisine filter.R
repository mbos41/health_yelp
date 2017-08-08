load("data/restaurants.Rda")
italian_filter <- restaurants %>% 
  filter(str_detect(categories, "Chinese")) %>% 
  select(restaurant_id)



filter_data2 <- filter_data %>% inner_join(italian_filter, by="restaurant_id")