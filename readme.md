# Hidden warnings; using topic modeling on Yelp reviews to predict health inspection violations

A deep dive into the predictive power of Supervised Latent Dirichlet Allocation on text 
reviews to detect food safety concerns. Analysis and [paper](https://github.com/mgbostwick/health_yelp/blob/master/paper/hidden_warnings.pdf)
developed for Advanced Machine Learning (COMP 790) at UNC - Chapel Hill. 

Data used in the analysis is available at [Driven Data](https://www.drivendata.org/competitions/5/keeping-it-fresh-predict-restaurant-inspections/page/33/)

## Description

### create_data.py
1. Reads in JSON review file and converts to DataFrame
2. Merges reviews onto inspection file by aggregating all reviews written between previous inspection and current inspection
3. Outputs data as csv file

### modeling.R
1. Converts review text to term frequency matrix using a few different criteria for vocabulary formulation
2. Fits a Supervised Latent Dirichlet Allocation (SLDA) model to the data with inspection violation as the response variable
3. Analyzes and plots results

### synthetic.R
1. Generates synthetic data from Dirichlet distributions
2. Evaluates performance of SLDA on synthetic data at different parameter levels
3. Compares discriminative power of LASSO and Random Forest on synthetic data

### /paper
* Contains Latex files and images used to generate [paper](https://github.com/mgbostwick/health_yelp/blob/master/paper/hidden_warnings.pdf)
