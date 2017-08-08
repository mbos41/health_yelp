library(jsonlite)
library(tidyverse)
library(stringr)
setwd("Documents/COMP_790/health_yelp")

business_data <- stream_in(file("data/yelp_academic_dataset_business.json"))
business_flat <- flatten(business_data)
business_tbl <- as.data.frame(business_flat)

id_map <- read.csv("data/restaurant_ids_to_yelp_ids.csv") %>% 
  mutate(business_id = yelp_id_0) %>% 
  select(business_id, restaurant_id)
restaurants <- business_tbl %>% inner_join(id_map, by ="business_id")

save(restaurants, file = "data/restaurants.Rda")
