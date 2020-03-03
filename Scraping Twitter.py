# -*- coding: utf-8 -*-
"""
Created on Wed Sep 25 15:11:20 2019

@author: jpolancoroque
"""

import json
import csv
import xlsxwriter
import tweepy
from tweepy import OAuthHandler

consumer_key = "EGFqmNfqVF4s00amDatcJGBlK"
consumer_secret = "iZ0iiy2QuE5ynS3Fg3eYpl3IFSlvIvs83O46KimcmVWxutRzuM"
access_token = "178419031-jWpacM41MZPYVhgVrcRcC3YFkXOfgVUSi0cF51Na"
access_token_secret = "QXgyjLCZuLE7SYjROONixIuluz6LKq4gGItvZ0OYMZfIt"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)

def tweet_to_xlsx(tweet):
    tweet_list = []
    tweet_list.append([tweet.user.screen_name, tweet.text])
  # tweet_list.append(tweet.text)
    workbook = xlsxwriter.Workbook('tweet.xlsx')
    worksheet = workbook.add_worksheet()
    row = 0
    col = 0
    for user, tweet in tweet_list:
        worksheet.write(row, col, user)
        worksheet.write(row, col + 1, tweet)
        row += 1
    workbook.close()

results = api.search(q=name, lang=lang, count=tweetCount)
for tweet in results:
    print(tweet.user.screen_name, "Twittou:", tweet.text)
    tweet_to_xlsx(tweet)
    
    
