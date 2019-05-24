# -*- coding: utf-8 -*-
"""
Created on Tue May 21 13:40:58 2019

@author: jeff.ernst
"""

# Import libraries
import csv
import re
import requests
from datetime import datetime, timedelta
from lxml import html

# Specify team abbreviations
teamAbbreviations = {
    'ATL': 'ATLANTA HAWKS',
    'BOS': 'BOSTON CELTICS',
    'BRK': 'BROOKLYN NETS',
    'CHA': 'CHARLOTTE BOBCATS',
    'CHI': 'CHICAGO BULLS',
    'CHO': 'CHARLOTTE HORNETS',
    'CLE': 'CLEVELAND CAVALIERS',
    'DAL': 'DALLAS MAVERICKS',
    'DEN': 'DENVER NUGGETS',
    'DET': 'DETROIT PISTONS',
    'GSW': 'GOLDEN STATE WARRIORS',
    'HOU': 'HOUSTON ROCKETS',
    'IND': 'INDIANA PACERS',
    'LAC': 'LOS ANGELES CLIPPERS',
    'LAL': 'LOS ANGELES LAKERS',
    'MEM': 'MEMPHIS GRIZZLIES',
    'MIA': 'MIAMI HEAT',
    'MIL': 'MILWAUKEE BUCKS',
    'MIN': 'MINNESOTA TIMBERWOLVES',
    'NJN': 'NEW JERSEY NETS',
    'NOH': 'NEW ORLEANS HORNETS',
    'NOK': 'NEW ORLEANS/OKLAHOMA CITY HORNETS',
    'NOP': 'NEW ORLEANS PELICANS',
    'NYK': 'NEW YORK KNICKS',
    'OKC': 'OKLAHOMA CITY THUNDER',
    'ORL': 'ORLANDO MAGIC',
    'PHI': 'PHILADELPHIA 76ERS',
    'PHO': 'PHOENIX SUNS',
    'POR': 'PORTLAND TRAIL BLAZERS',
    'SAC': 'SACRAMENTO KINGS',
    'SAS': 'SAN ANTONIO SPURS',
    'SEA': 'SEATTLE SUPERSONICS',
    'TOR': 'TORONTO RAPTORS',
    'UTA': 'UTAH JAZZ',
    'WAS': 'WASHINGTON WIZARDS'
    }

seasons = {
    '1994-95': '1995',
    '1995-96': '1996',
    '1996-97': '1997',
    '1997-98': '1998',
    '1998-99': '1999',
    '1999-00': '2000',
    '2000-01': '2001',
    '2001-02': '2002',
    '2002-03': '2003',
    '2003-04': '2004',
    '2004-05': '2005',
    '2005-06': '2006',
    '2006-07': '2007',
    '2007-08': '2008',
    '2008-09': '2009',
    '2009-10': '2010',
    '2010-11': '2011',
    '2011-12': '2012',
    '2012-13': '2013',
    '2013-14': '2014',
    '2014-15': '2015',
    '2015-16': '2016',
    '2016-17': '2017',
    '2017-18': '2018',
    '2018-19': '2019',
    '2019-20': '2020',
    '2020-21': '2021',
    '2021-22': '2022',
    '2022-23': '2023',
    '2023-24': '2024',
    '2024-25': '2025'    
    }

# Create parse player salaries function
def ParsePlayerHistory(playerId, playerName, row):
    return {
        "slug": playerId,
        "name": playerName,
        "season": seasons.get(row[0].text_content()),
        "team": teamAbbreviations.get(row[2].text_content(), row[2].text_content()),
    }

# Create scrape team rosters season function
def ScrapePlayerHistory(playerHistory, playerId, playerName):
    
    # Create url
    url = ('https://www.basketball-reference.com/players/{firstLetter}/{playerId}.html'.format(
            firstLetter=playerId[0],
            playerId=playerId))
    
    # Request the webpage
    response = requests.get(url=url)
    
    # Get content from webpage
    page = response.content
    
    # Parse webpage with html
    tree = html.fromstring(page)
    
    # Find all rows in table
    rows = tree.xpath('//table[@id="per_game"]/tbody/tr')
    
    # Scrape every row of the table
    for row in rows:
        
        # Parse each row and append it to the team rosters list
        playerHistory.append(ParsePlayerHistory(playerId, playerName, row))
    
    # Return team rosters
    return(playerHistory)
    
# Create scrape team rosters function
def ScrapeAllPlayerHistories():
    
    # Create empty lists
    playerIds = []
    playerNames = []

    with open('data/player names/player names.csv', 'r') as file:
        reader = csv.reader(file)
        next(reader)
        for row in reader:
            if (row[0] not in playerIds):
                playerIds.append(row[0])
                playerNames.append(row[1])
            
    # Create empty player history list
    playerHistory = []
            
    for row in range(0, len(playerIds)):
        
        # Print data being scraped
        print('Scraping player history for ' + playerIds[row] + ' (' + '{0:.2%}'.format((row + 1) / len(playerIds)) + ')')
        
        # Scrape player history
        playerHistory = ScrapePlayerHistory(playerHistory, playerIds[row], playerNames[row])
               
    # Specify column names
    columnNames = ['slug',
                   'name',
                   'season',
                   'team']
    
    # Specify file name
    fileName = 'data/player history/player history.csv'
    
    # Create csv
    with open(fileName, 'w') as file:
        
        # Set up csv writer
        writer = csv.DictWriter(file, lineterminator='\n', fieldnames=columnNames)
        
        # Create file headers
        writer.writeheader()
        
        # Copy data from player salaries
        for data in playerHistory:
            
            # Copy data to rows
            writer.writerow(data)