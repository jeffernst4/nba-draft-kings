# -*- coding: utf-8 -*-
"""
Created on Mon May 20 13:16:46 2019

@author: jeff.ernst
"""

# clean up code

# go to each team page
# get link to each season until year is less than 2010
# paste player names into list for every team
# at the end of the each year, paste into csv

# Import libraries
import csv
import re
import requests
from datetime import datetime, timedelta
from lxml import html

# Scrape team rosters
ScrapeTeamRosters(2010, 2019)

# Specify team abbreviations
teamAbbreviations = {
    'ATL': 'ATLANTA HAWKS',
    'BOS': 'BOSTON CELTICS',
    'BRK': 'BROOKLYN NETS',
    'CHA': 'CHARLOTTE HORNETS',
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
    'NJN': 'BROOKLYN NETS',
    'NOH': 'NEW ORLEANS PELICANS',
    'NOP': 'NEW ORLEANS PELICANS',
    'NYK': 'NEW YORK KNICKS',
    'OKC': 'OKLAHOMA CITY THUNDER',
    'ORL': 'ORLANDO MAGIC',
    'PHI': 'PHILADELPHIA 76ERS',
    'PHO': 'PHOENIX SUNS',
    'POR': 'PORTLAND TRAIL BLAZERS',
    'SAC': 'SACRAMENTO KINGS',
    'SAS': 'SAN ANTONIO SPURS',
    'TOR': 'TORONTO RAPTORS',
    'UTA': 'UTAH JAZZ',
    'WAS': 'WASHINGTON WIZARDS'
    }

# Create parse player salaries function
def ParseTeamRosters(row, team):
    return {
        "number": str(row[0].text_content()),
        "name": str(row[1].text_content()),
        "position": str(row[2].text_content()),
        "height": str(row[3].text_content()),
        "weight": str(row[4].text_content()),
        "birth_date": str(row[5].text_content()),
        "experience": str(row[7].text_content()),
        "college": str(row[8].text_content()),
        "team": teamAbbreviations.get(team),
    }

# Create scrape team rosters season function
def ScrapeTeamRostersSeason(teamRosters, link, team):
    
    url = ('https://www.basketball-reference.com' + link).format(
            link=link,
            )
    
    # Request the webpage
    response = requests.get(url=url)
    
    # Get content from webpage
    page = response.content
    
    # Parse webpage with html
    tree = html.fromstring(page)
    
    # Find all rows in table
    rows = tree.xpath('//*[@id="roster"]/tbody/tr')
    
    # Scrape every row of the table
    for row in rows:
        
        # Parse each row and append it to the team rosters list
        teamRosters.append(ParseTeamRosters(row, team))
    
    # Return team rosters
    return(teamRosters)
    
# Create scrape team rosters function
def ScrapeTeamRosters(startYear, endYear):
    
    # Format dates
    year = startYear
    
    # Scrape each year
    while(endYear - year) >= 0:
        
        # Print data being scraped
        print('Scraping team rosters for ' + str(year))
        
        # Create url
        url = 'https://www.basketball-reference.com/leagues/NBA_{year}.html'.format(
                year=year
            )
        
        # Request the webpage
        response = requests.get(url=url)
        
        # Get content from webpage
        page = response.content
        
        # Parse webpage with html
        tree = html.fromstring(page)
        
        # Create empty team rosters list
        teamRosters = []
        
        # Scrape teams for both divisions
        for division in ['W', 'E']:
            
            # Find all rows in table
            rows = tree.xpath('//table[@id="divs_standings_{division}"]/tbody/tr[@class="full_table"]/th/a'.format(
                    division=division
                    ))
            
            # Scrape every row of the table
            for row in rows:
                
                # Get link for team page
                link = row.get('href')
                
                # Get team name
                team = re.sub('\\/', '', re.search('\\/[A-Z]{3}\\/', link)[0])
                
                # Scrape team roster
                teamRosters = ScrapeTeamRostersSeason(teamRosters, link, team)
               
        # Specify column names
        columnNames = ['number',
                       'name',
                       'position',
                       'height',
                       'weight',
                       'birth_date',
                       'experience',
                       'college',
                       'team']
        
        # Specify file name
        fileName = 'data/team rosters/team rosters [{year}].csv'.format(
                    year=year
                    )
        
        # Create csv
        with open(fileName, 'w') as file:
            
            # Set up csv writer
            writer = csv.DictWriter(file, lineterminator='\n', fieldnames=columnNames)
            
            # Create file headers
            writer.writeheader()
            
            # Copy data from player salaries
            for data in teamRosters:
                
                # Copy data to rows
                writer.writerow(data)
        
    
        
        
        # Add one year to the year
        year = year + 1
    