Dataset: Google Analytics data of Google Merchandise Store website. It's visit-level data, including userid, time, geo_info, pageviews, hits, referrer, ad_click, ... Link for the competition and dataset (https://www.kaggle.com/c/ga-customer-revenue-prediction/)

Objective: Predict the total purchase a user has made during the visits in the test set.

Customer traffic dataset was analyzed and pre-processed in R Studio platform to predict the natural log of sum of all transactions per user. Used Ensemble learning techniques from H20.ai (open source Leader in ML and AI) to train and run on the processed data to achieve a considerably lower error rate.

Please notice that this is my code for the competition before its relaunch in early Nov. (There is a data leakage identified in late Oct, so everything about this competition has been modified, including rule, dataset, and prediction objectives)

Overall it was a good learning experience, as I have been playing with the Google Analytics data off late at work to understand user behaviors, it’s actually great to have a chance to try predicting sales with those web data from Google Analytics.
