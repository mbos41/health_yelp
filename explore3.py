
# coding: utf-8

# In[1]:

import os
os.getcwd()
os.chdir('/Users/michaelbostwick/Documents/COMP_790/health_yelp/')

import pandas as pd
import numpy as np

reviews = pd.read_csv('reviews.csv')
reviews.head()


# In[2]:

from matplotlib import pyplot as plt

with plt.style.context('fivethirtyeight'):
    plt.hist(reviews.stars, 20, normed = 1, facecolor='green', alpha=0.75)
    
plt.show()


# In[3]:

violations = pd.read_csv("data/AllViolations.csv")
violations.head()


# In[4]:

viol_sorted = violations.sort_values(['restaurant_id', 'date'])
viol_sorted.head(10)
#violations_sorted.tail()


# In[33]:

viol_sorted['prev_date_temp'] = viol_sorted.date.shift(1)
viol_sorted['prev_rest'] = viol_sorted.restaurant_id.shift(1)
viol_sorted['prev_date'] = np.where(viol_sorted.prev_rest == viol_sorted.restaurant_id, viol_sorted.prev_date_temp, np.nan)         
viol_sorted.prev_date = viol_sorted.prev_date.fillna('2000-01-01')
viol_sorted.drop(['prev_date_temp', 'prev_rest'], inplace = True, axis = 1)
viol_sorted.head(100)   



# In[7]:

viol_per_rest = violations.groupby(['restaurant_id']).count()
viol_per_rest.head()


# In[8]:

plt.hist(viol_per_rest.date, 20, normed = 1, facecolor='green', alpha=0.75)    
plt.show()


# In[14]:

pos_text = 'afgadgdfsg'
neg_text = 'fgdfgdfgd'
pos_text2 = 'sdsfdsdfgdfsg'
neg_text2 = 'reterdfgd'
rev_dict = {}
rev_dict['a'] = [pos_text, neg_text]
rev_dict['b'] = [pos_text2, neg_text2]

pd.DataFrame.from_dict(rev_dict, orient = 'index')


# In[45]:

df = pd.merge(viol_sorted, reviews, how='inner', left_on='restaurant_id', right_on='restaurant_id')
df = df[(df['date_y']>=df['prev_date']) & (df['date_y']<=df['date_x'])]
#df_sorted = df.sort_values(['restaurant_id', 'date_x'])
#df_sorted.head(20)


# In[48]:

agg_text = df.groupby(['restaurant_id', 'date_x'])['text'].apply(lambda x: "[%s]" % ' '.join(x))
agg_text[0]

