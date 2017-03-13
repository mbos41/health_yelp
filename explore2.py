import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
os.getcwd()
os.chdir('/Users/michaelbostwick/Documents/COMP_790/comp790/final project/')
#! head -n 1 data/yelp_academic_dataset_review.json

# Build dictionary to map Boston restaurant ids to Yelp ids
id_map = pd.read_csv("data/restaurant_ids_to_yelp_ids.csv")
id_dict = {}

for i, row in id_map.iterrows():
    boston_id = row["restaurant_id"]
    
    non_null_mask = pd.notnull(row.ix[1:])
    yelp_ids = row[1:][non_null_mask].values
    
    for yelp_id in yelp_ids:
        id_dict[yelp_id] = boston_id

#id_dict['5Kdf1DGbRScRk6Cx3jaX8w']

with open("data/yelp_academic_dataset_review.json", 'r') as review_file:
    review_json = '[' + ','.join(review_file.readlines()) + ']'

# read in the json as a DataFrame
reviews = pd.read_json(review_json)
reviews.drop(['review_id', 'type', 'user_id', 'votes'], inplace = True, axis = 1)

map_to_boston_ids = lambda yelp_id: id_dict[yelp_id] if yelp_id in id_dict else np.nan
reviews.business_id = reviews.business_id.map(map_to_boston_ids)

# Rename column names to join with Boston data
reviews.columns = ["restaurant_id", "date", "stars", "text"]

# Drop restaurants not in Boston data
reviews = reviews[pd.notnull(reviews.restaurant_id)]

print(reviews.head())

boston = pd.read_csv("data/AllViolations.csv")
