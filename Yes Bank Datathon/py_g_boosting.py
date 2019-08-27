# -*- coding: utf-8 -*-
"""
Created on Mon Nov  5 18:37:09 2018

@author: Akshar Sharma
"""
%matplotlib inline 
import matplotlib.pylab as plt 
import numpy as np 
from scipy import sparse 
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import AdaBoostRegressor
from sklearn.decomposition import PCA 
from sklearn.cross_validation import ShuffleSplit, train_test_split 
from sklearn import metrics 
from sklearn.learning_curve import learning_curve
from sklearn.linear_model import LinearRegression
from sklearn.grid_search import GridSearchCV 
from pprint import pprint 
import pandas as pd 
from pandas.tools.plotting import scatter_matrix 
import urllib 
import requests 
import zipfile 
import StringIO 
import seaborn 
import os
os.chdir('D:\DataAnalytics_Files\Yes_Bank_Datathon\challenge-3\prediction')
train_master = pd.read_csv("Yes_Bank_Trainp.csv")
test_master = pd.read_csv("Yes_Bank_Test_int.csv")

y = train_master.credit_amount
X = train_master.drop('credit_amount', axis=1)
X_train, X_test, y_train, y_test = train_test_split(X, y,test_size=0.3)
X.dtypes
categorical_features_indices = np.where(X.dtypes != np.float)[0]

#importing library and building model
from catboost import CatBoostRegressor
model=CatBoostRegressor(iterations=110000, depth=5, learning_rate=1, loss_function='RMSE')
model.fit(X_train, y_train,cat_features=categorical_features_indices,eval_set=(X_test, y_test),plot=True)

submission = pd.DataFrame()
submission['Item_Identifier'] = test['Item_Identifier']
submission['Outlet_Identifier'] = test['Outlet_Identifier']
submission['Item_Outlet_Sales'] = model.predict(test_master)
submission.to_csv("Submission.csv")

'''
# fit estimator
est = GradientBoostingRegressor(loss='quantile', learning_rate=0.0001, n_estimators=50, max_features='log2', min_samples_split=2, max_depth=1)
est.fit(X_train, y_train)

# predict class labels
pred = est.predict(X_test)

# score on test data (accuracy)
acc = est.score(X_test, y_test)
print('ACC: %.4f' % acc)

# predict class probabilities
est.predict_proba(X_test)[0]



#def cross_validate_best_known():
#    '''
#        import and clean the tractor data, then do a corss validation on each of the three models we are
#        training here. A RandomForest, a GradientBoost, and an AdaBoost backed by a DecisionTree. Print
#        the scores.
#
#        The parameters we're using here are the "best" that we've found so far using a grid search.
#    '''
#    train_master = pd.read_csv("Yes_Bank_Trainp.csv")
#    test_master = pd.read_csv("Yes_Bank_Test_int.csv")
#    X = train_master
#    y = train_master.pop('Credit_Amount')
#
#    rf = RandomForestRegressor(max_features=2, min_samples_split=4, n_estimators=50, min_samples_leaf=2)
#    gb = GradientBoostingRegressor(loss='quantile', learning_rate=0.0001, n_estimators=50, max_features='log2', min_samples_split=2, max_depth=1)
#    ab = AdaBoostRegressor(ada_tree_backing, learning_rate=0.1, loss='square', n_estimators=1000)
#
# validation.cross_validate_scores([rf, gb, ab], X, y)