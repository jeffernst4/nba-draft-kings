
# only run for csvs that don't exist already

# get list of all playerId's on playerboxscores data
# Scrape teams for every playerid greater than 2015 season
# Match name + team + season to get player Id's
# Match salary by date + playerId to playerBoxScores
# -------see if any playerId doesn't have a match
# Fill in all player records when they had salaries, but didn't play, fill all stats with 0

# Import libraries
import csv
import requests
from datetime import datetime, timedelta
from lxml import html

# Specify team abbreviations
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

# Specify game abbreviations
gameAbbreviations = {
    'draftkings': 'dk',
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

# Create scrape player salaries function
def ScrapePlayerSalaries(startDate, endDate, game):

    # Format dates
    startDate = datetime.strptime(startDate, '%m/%d/%Y')
    endDate = datetime.strptime(endDate, '%m/%d/%Y')
    date = startDate
    
    # Scrape each date
    while (endDate - date).days >= 0:
        
        # Print data being scraped
        print('Scraping player salaries for ' + date.strftime('%Y-%m-%d'))
        
        # Create url
        url = 'http://rotoguru1.com/cgi-bin/hyday.pl?game={game}&mon={month}&day={day}&year={year}'.format(
                game=gameAbbreviations.get(game),
                day=date.day,
                month=date.month,
                year=date.year
            )
        
        # Request the webpage
        response = requests.get(url=url)
        
        # Get content from webpage
        page = response.content
        
        # Parse webpage with html
        tree = html.fromstring(page)
        
        # Find all rows in table
        rows = tree.xpath('//table/tr')
        
        # Create an empty totals list
        playerSalaries = []
        
        # Scrape every row of the table
        for row in rows:
            
            # Check the length of the row
            if len(row) == 9:
                
                # Parse each row and append it to the player salaries list
                playerSalaries.append(ParsePlayerSalaries(row))
        
        # Specify the name of the columns
        columnNames = ['position',
                       'name',
                       'fantasy_points',
                       'salary',
                       'team',
                       'opponent',
                       'score',
                       'minutes_played',
                       'stats']
        
        # Create file name
        fileName = 'data/player salaries/{game}/player salaries [{year}-{month}-{day}].csv'.format(
                    game=game,
                    day=f"{date.day:02d}",
                    month=f"{date.month:02d}",
                    year=date.year
                    )
        
        # Create csv
        with open(fileName, 'w') as file:
            
            # Set up csv writer
            writer = csv.DictWriter(file, lineterminator='\n', fieldnames=columnNames)
            
            # Create file headers
            writer.writeheader()
            
            # Copy data from player salaries
            for data in playerSalaries:
                
                # Copy data to rows
                writer.writerow(data)
        
        # Add one day to the date    
        date = date + timedelta(days=1)
        
# Scrape player salaries
ScrapePlayerSalaries('05/01/2019', '05/31/2019', 'draftkings')
ScrapePlayerSalaries('05/01/2019', '05/21/2019', 'fanduel')
ScrapePlayerSalaries('05/01/2019', '05/21/2019', 'yahoo')