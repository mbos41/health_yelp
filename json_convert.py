#import json
import pandas as pd

with open("data/yelp_academic_dataset_review.json", 'r') as review_file:
    # the file is not actually valid json since each line is an individual
    # dict -- we will add brackets on the very beginning and ending in order
    # to make this an array of dicts and join the array entries with commas
    review_json = '[' + ','.join(review_file.readlines()) + ']'
    #json.dump("data/reviews.json", review_file)
    

# read in the json as a DataFrame
reviews = pd.read_json(review_json)
print(reviews.head())
    
    
    