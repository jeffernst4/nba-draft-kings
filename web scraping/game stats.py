
# only run for csvs that don't exist already

# Import libraries
import basketball_reference_web_scraper
import time
import datetime
from basketball_reference_web_scraper import client
from basketball_reference_web_scraper.data import OutputType
from basketball_reference_web_scraper.data import Team
from datetime import datetime, timedelta

# Create box score web scraper function
def ScrapeBoxScores(scraperFunction, fileType, startDate, endDate):
    
    # Format dates
    startDate = datetime.strptime(startDate, '%m/%d/%Y')
    endDate = datetime.strptime(endDate, '%m/%d/%Y')
    date = startDate
    
    # Scrape each date
    while (endDate - date).days >= 0:
        
        # Print data being scraped
        print('Scraping ' + fileType + ' box scores for ' + date.strftime('%Y-%m-%d'))
        
        # Scrape data
        scraperFunction(day=date.day,
                        month=date.month,
                        year=date.year,
                        output_type=OutputType.CSV,
                        output_file_path='data/' + fileType + ' box scores/' + fileType + ' box scores [{}-{}-{}].csv'.format(date.year, f"{date.month:02d}", f"{date.day:02d}"))
        
        # Add one day to the date
        date = date + timedelta(days=1)

# Create season web scraper function
def ScrapeSeasonData(scraperFunction, fileType, startYear, endYear):
    
    # Format dates
    year = startYear
    
    # Scrape each year
    while(endYear - year) >= 0:
        
        # Print data being scraped
        print('Scraping ' + fileType + ' for ' + str(year))
        
        # Scrape data
        scraperFunction(season_end_year=year,
                        output_type=OutputType.CSV,
                        output_file_path='data/' + fileType + '/' + fileType + ' [{}].csv'.format(year))
        
        # Add one year to the year
        year = year + 1

# Scrape player and team box scores
ScrapeBoxScores(client.player_box_scores, 'player', '05/01/2019', '05/21/2019')
ScrapeBoxScores(client.team_box_scores, 'team', '05/01/2019', '05/21/2019')

# Scrape seasons schedules and player season totals
ScrapeSeasonData(client.season_schedule, 'season schedules', 2010, 2019)
ScrapeSeasonData(client.players_season_totals, 'player season totals', 2010, 2019)
