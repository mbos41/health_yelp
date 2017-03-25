import numpy as np
import pandas as pd
import os
os.getcwd()
os.chdir('/Users/michaelbostwick/Documents/COMP_790/health_yelp/')
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
reviews = pd.DataFrame(reviews[pd.notnull(reviews.restaurant_id)])

#reviews.to_csv('reviews.csv')

#print(reviews.head())

all_violations = pd.read_csv("data/AllViolations.csv").head(100)
viol_sorted = all_violations.sort_values(['restaurant_id', 'date'])
viol_sorted['prev_date_temp'] = viol_sorted.date.shift(1)
viol_sorted['prev_rest'] = viol_sorted.restaurant_id.shift(1)
viol_sorted['prev_date'] = np.where(viol_sorted.prev_rest == viol_sorted.restaurant_id, viol_sorted.prev_date_temp, np.nan)         
viol_sorted.drop(['prev_date_temp', 'prev_rest'], inplace = True, axis = 1)

df = pd.merge(viol_sorted, reviews, how='left', left_on='restaurant_id', right_on='restaurant_id')
df = df[(df['date_y']>=df['prev_date']) & (df['date_y']<=df['date_x'])]
df['pos_text'] = np.where(df.stars > 3, df.text, '')
df['neg_text'] = np.where(df.stars > 3, '', df.text)

pos_data = pd.DataFrame(df_sorted.groupby(['restaurant_id', 'date_x'])['pos_text'].apply(lambda x: "%s" % ' '.join(x)))
neg_data = pd.DataFrame(df_sorted.groupby(['restaurant_id', 'date_x'])['neg_text'].apply(lambda x: "%s" % ' '.join(x)))
agg_data = pd.merge(pos_data, neg_data, how='outer', left_index=True, right_index=True)

analysis_data = pd.merge(viol_sorted, agg_data, how='left', left_on=['restaurant_id', 'date'], right_index=True)
analysis_data['pos_text_temp'] = analysis_data.pos_text.shift(1)
analysis_data['neg_text_temp'] = analysis_data.neg_text.shift(1)
analysis_data.pos_text = np.where(pd.isnull(analysis_data.pos_text), analysis_data.pos_text_temp, analysis_data.pos_text)
analysis_data.neg_text = np.where(pd.isnull(analysis_data.neg_text), analysis_data.neg_text_temp, analysis_data.neg_text)

analysis_data.to_csv('data/analysis_data.csv')
