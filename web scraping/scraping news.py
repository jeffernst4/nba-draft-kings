# -*- coding: utf-8 -*-
"""
Created on Wed May 15 13:18:48 2019

@author: jeff.ernst
"""

# https://www.cbssports.com/fantasy/basketball/players/news/all/8600/

# scrape each link on each page, follow link to the page and then scrape news and data

# https://blog.datarobot.com/using-datarobot-to-predict-nba-player-performance

# https://towardsdatascience.com/predicting-wine-quality-using-text-reviews-8bddaeb5285d

# http://digital-thinking.de/deep-learning-combining-numerical-and-text-features-in-deep-neural-networks/

# https://shirinsplayground.netlify.com/2019/01/text_classification_keras_data_prep/

# Import libraries
import csv
import io
import pandas as pd
import re
import requests
import urllib.request

from bs4 import BeautifulSoup
from datetime import datetime, timedelta
from lxml import html

url = 'https://www.cbssports.com/fantasy/basketball/players/news/all/{pageNumber}/'.format(
        pageNumber = 502
        )

# page = urllib.request.urlopen(url).read()

response = requests.get(url=url)

response.raise_for_status()

page = response.content

tree = html.fromstring(page)

rows = tree.xpath('//ul[@id="playerNewsContent"]/li/div/div[2]/div/div/p[1]')

[@id="playerNewsContent"]

//*[@id="playerNewsContent"]/li[1]/div/div[2]/div/div/p[1]/text()