
# Import libraries
import basketball_reference_web_scraper
import time
import datetime
from datetime import datetime, timedelta
from basketball_reference_web_scraper import client
from basketball_reference_web_scraper.data import OutputType
from basketball_reference_web_scraper.data import Team

# Create box score web scraper function
def ScrapeBoxScores(function, fileType, startDate, endDate):
    
    # Format dates
    startDate = datetime.strptime(startDate, '%m/%d/%Y')
    endDate = datetime.strptime(endDate, '%m/%d/%Y')
    currentDate = startDate
    
    # Scrape each date
    while (endDate - currentDate).days >= 0:
        print('Scraping ' + fileType + ' box scores for ' + currentDate.strftime('%Y-%m-%d'))
        function(day=currentDate.day,
                 month=currentDate.month,
                 year=currentDate.year,
                 output_type=OutputType.CSV,
                 output_file_path='data/' + fileType + ' box scores/' + fileType + ' box scores [{}-{}-{}].csv'.format(currentDate.year, f"{currentDate.month:02d}", f"{currentDate.day:02d}"))
        currentDate = currentDate + timedelta(days=1)

# Create season web scraper function
def ScrapeSeasonData(function, fileType, startYear, endYear):
    
    # Format dates
    currentYear = startYear
    
    # Scrape each year
    while(endYear - currentYear) >= 0:
        print('Scraping ' + fileType + ' for ' + str(currentYear))
        function(season_end_year=currentYear,
                 output_type=OutputType.CSV,
                 output_file_path='data/' + fileType + '/' + fileType + ' [{}].csv'.format(currentYear))
        currentYear = currentYear + 1

# Scrape player and team box scores
ScrapeBoxScores(client.player_box_scores, 'player', '01/01/2010', '05/02/2019')
ScrapeBoxScores(client.team_box_scores, 'team', '01/01/2010', '05/02/2019')

# Scrape seasons schedules and player season totals
ScrapeSeasonData(client.season_schedule, 'season schedules', 2010, 2019)
ScrapeSeasonData(client.players_season_totals, 'player season totals', 2010, 2019)
