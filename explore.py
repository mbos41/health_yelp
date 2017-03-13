import os
os.getcwd()
os.chdir('/Users/michaelbostwick/Documents/COMP_790/')

import pandas as pd
import numpy as np

violations = pd.read_csv('AllViolations.csv')
violations.rename(columns = {'*':'minor', '**':'major', '***':'severe'}, inplace = True)
violations.head()
violations.minor.describe()
violations['year'] = violations.date.str.slice(0,4)
violations['month'] = violations.date.str.slice(5,7)
violations['day'] = violations.date.str.slice(8,10)

pd.value_counts(violations.year)


from matplotlib import pyplot as plt

with plt.style.context('fivethirtyeight'):
    plt.hist(violations.minor, 20, normed = 1, facecolor='green', alpha=0.75)
    #plt.hist(violations.major, 100, normed = 1, facecolor='yellow', alpha=0.75)
    #plt.hist(violations.severe, 100, normed = 1, facecolor='red', alpha=0.75)
    
plt.axis([0, 5, 0, 1])  
plt.show()


