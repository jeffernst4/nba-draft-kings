
# Import libraries

import basketball_reference_web_scraper
import time
import datetime
from datetime import datetime, timedelta

from basketball_reference_web_scraper import client
from basketball_reference_web_scraper.data import OutputType
from basketball_reference_web_scraper.data import Team


# Scrape player box scores
startDate = "04/03/2019"
startDate = datetime.strptime(startDate, "%m/%d/%Y")
currentDate = startDate


while (datetime.now() - currentDate).days > 0:
    print(currentDate)
    client.player_box_scores(day=currentDate.day,
                             month=currentDate.month,
                             year=currentDate.year,
                             output_type=OutputType.CSV,
                             output_file_path='C:/Users/jeff.ernst/Documents/Personal/Basketball Analysis/Data/Player Box Scores/Player Box Scores [{}-{}-{}].csv'.format(currentDate.year, f"{currentDate.month:02d}", f"{currentDate.day:02d}"))
    currentDate = currentDate + timedelta(days=1)
    time.sleep(2)


# Scrape team box scores
startDate = "04/03/2019"
startDate = datetime.strptime(startDate, "%m/%d/%Y")
currentDate = startDate

while (datetime.now() - currentDate).days > 0:
    print(currentDate)
    client.team_box_scores(day=currentDate.day,
                             month=currentDate.month,
                             year=currentDate.year,
                             output_type=OutputType.CSV,
                             output_file_path='C:/Users/jeff.ernst/Documents/Personal/Basketball Analysis/Data/Team Box Scores/Team Box Scores [{}-{}-{}].csv'.format(currentDate.year, f"{currentDate.month:02d}", f"{currentDate.day:02d}"))
    currentDate = currentDate + timedelta(days=1)
    time.sleep(2)


# Scrape season schedules
currentYear = 2010

while(datetime.now().year - currentYear) >= 0:
    print(currentYear)
    client.players_season_totals(season_end_year=currentYear,
                                 output_type=OutputType.CSV,
                                 output_file_path='C:/Users/Jeff/Documents/Basketball Analysis/Data/Season Schedules/Season Schedules [{}].csv'.format(currentYear))
    currentYear = currentYear + 1
    time.sleep(2)


# Scrape player season totals
currentYear = 2010

while(datetime.now().year - currentYear) >= 0:
    print(currentYear)
    client.players_season_totals(season_end_year=currentYear,
                                output_type=OutputType.CSV,
                                output_file_path='C:/Users/Jeff/Documents/Basketball Analysis/Data/Player Season Totals/Player Season Totals [{}].csv'.format(currentYear))
    currentYear = currentYear + 1
    time.sleep(2)
