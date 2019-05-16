
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

teamAbbreviations = {
    'atl': 'ATLANTA HAWKS',
    'bkn': 'BROOKLYN NETS',
    'bos': 'BOSTON CELTICS',
    'cha': 'CHARLOTTE HORNETS',
    'chi': 'CHICAGO BULLS',
    'cle': 'CLEVELAND CAVALIERS',
    'dal': 'DALLAS MAVERICKS',
    'den': 'DENVER NUGGETS',
    'det': 'DETROIT PISTONS',
    'gsw': 'GOLDEN STATE WARRIORS',
    'hou': 'HOUSTON ROCKETS',
    'ind': 'INDIANA PACERS',
    'lac': 'LOS ANGELES CLIPPERS',
    'lal': 'LOS ANGELES LAKERS',
    'mem': 'MEMPHIS GRIZZLIES',
    'mia': 'MIAMI HEAT',
    'mil': 'MILWAUKEE BUCKS',
    'min': 'MINNESOTA TIMBERWOLVES',
    'nor': 'NEW ORLEANS PELICANS',
    'nyk': 'NEW YORK KNICKS',
    'okc': 'OKLAHOMA CITY THUNDER',
    'orl': 'ORLANDO MAGIC',
    'phi': 'PHILADELPHIA 76ERS',
    'pho': 'PHOENIX SUNS',
    'por': 'PORTLAND TRAIL BLAZERS',
    'sac': 'SACRAMENTO KINGS',
    'sas': 'SAN ANTONIO SPURS',
    'tor': 'TORONTO RAPTORS',
    'uta': 'UTAH JAZZ',
    'was': 'WASHINGTON WIZARDS',
    'WES': 'WEST'
    }

gameAbbreviations = {
    'draftkings': 'df',
    'fanduel': 'fd',
    'yahoo': 'yh'
}

# Create parse player salaries function
def ParsePlayerSalaries(row):
    return {
        "position": str(row[0].text_content()),
        "name": str(row[1].text_content()),
        "fantasy_points": str(row[2].text_content()),
        "salary": str(row[3].text_content()),
        "team": teamAbbreviations.get(row[4].text_content()),
        "opponent": str(row[5].text_content()),
        "score": str(row[6].text_content()),
        "minutes_played": str(row[7].text_content()),
        "stats": str(row[8].text_content()),
    }

def ScrapePlayerSalaries(startDate, endDate, game):

    # Format dates
    startDate = datetime.strptime(startDate, '%m/%d/%Y')
    endDate = datetime.strptime(endDate, '%m/%d/%Y')
    currentDate = startDate
    
    while (endDate - currentDate).days >= 0:
        
        print('Scraping player salaries for ' + currentDate.strftime('%Y-%m-%d'))
    
        url = 'http://rotoguru1.com/cgi-bin/hyday.pl?game={game}&mon={month}&day={day}&year={year}'.format(
                game=gameAbbreviations.get(game),
                day=currentDate.day,
                month=currentDate.month,
                year=currentDate.year
            )
        
        # page = urllib.request.urlopen(url).read()
        
        response = requests.get(url=url)
        
        response.raise_for_status()
        
        page = response.content
        
        tree = html.fromstring(page)
        
        rows = tree.xpath('//table/tr')
        
        
        
        totals = []
        
        for row in rows:
            #print(str(row[3].text_content()))
            #if re.search("\$[0-9]+,[0-9]{3}", str(row[3].text_content()))
            #    totals.append(parse_player_season_totals(row))
            if len(row) == 9:
                totals.append(ParsePlayerSalaries(row))
        # return totals
        
        csv_columns = ['position',
                       'name',
                       'fantasy_points',
                       'salary',
                       'team',
                       'opponent',
                       'score',
                       'minutes_played',
                       'stats']
        
        csv_file = 'data/player salaries/{game}/player salaries [{year}-{month}-{day}].csv'.format(
                game=game,
                day=f"{currentDate.day:02d}",
                month=f"{currentDate.month:02d}",
                year=currentDate.year
                )
        
        with open(csv_file, 'w') as csvfile:
            writer = csv.DictWriter(csvfile, lineterminator='\n', fieldnames=csv_columns)
            writer.writeheader()
            for data in totals:
                writer.writerow(data)
                
        currentDate = currentDate + timedelta(days=1)
        
ScrapePlayerSalaries('01/01/2014', '05/14/2019', 'draftkings')
ScrapePlayerSalaries('01/01/2014', '05/14/2019', 'fanduel')
ScrapePlayerSalaries('01/01/2016', '01/01/2017', 'yahoo')