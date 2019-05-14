https://github.com/jaebradley/basketball_reference_web_scraper/blob/v4/basketball_reference_web_scraper/parsers/players_season_totals.py

https://srome.github.io/Parsing-HTML-Tables-in-Python-with-BeautifulSoup-and-pandas/

import pandas as pd
import io
from bs4 import BeautifulSoup
import urllib.request
from lxml import html

url = 'http://rotoguru1.com/cgi-bin/hyday.pl?game=dk&mon=MONTH&day=DAY&year=YEAR'

mon = "1"
day = "1"
yr = "2017"

soup = BeautifulSoup(urllib.request.urlopen(url.replace("MONTH", mon).replace("DAY", day).replace("YEAR", yr)).read())

table = soup.find_all('table')[7]

rows = table.find_all('tr')

row = rows[3].find_all('td')

str(row[1].get_text())

### USE lxml and html instead of beautiful soup

def parse_player_season_totals(row):
    return {
        "position": str(row[1].get("data-append-csv")),
        "name": str(row[1].text_content()),
        "fantasy_points": parse_positions(row[2].text_content()),
        "salary": str_to_int(row[3].text_content(), default=None),
        "team": TEAM_ABBREVIATIONS_TO_TEAM.get(row[4].text_content()),
        "opponent": str_to_int(row[5].text_content()),
        "score": str_to_int(row[6].text_content()),
        "minutes_played": str_to_int(row[7].text_content()),
        "stats": str_to_int(row[8].text_content()),
    }

totals = []

for row in rows:
    # Basketball Reference includes a "total" row for players that got traded
    # which is essentially a sum of all player team rows
    # I want to avoid including those, so I check the "team" field value for "TOT"
    if row[4].text_content() != "TOT":
        totals.append(parse_player_season_totals(row))
return totals

columns = table.find_all('tr')[7].find_all('td')

new_table = pd.DataFrame(columns=range(0,9), index = [0]) # I know the size 

row_marker = 1
column_marker = 0
for column in columns:
    new_table.iat[row_marker,column_marker] = column.get_text()
    column_marker += 1


    
row_marker = 0
for row in table.find_all('tr')[7]:
    column_marker = 0
    columns = row.find_all('td')
    print(row.find_all('td'))
    
    
    for column in columns:
        new_table.iat[row_marker,column_marker] = column.get_text()
        column_marker += 1



# days = list(map(str, range(1, 31)))
# months = list(map(str, range(1, 12)))
# years = list(map(str, range(2017, 2018)))

all_games = pd.DataFrame()
for yr in years:
    for mon in months:
        for day in days:
            soup = BeautifulSoup(urllib.request.urlopen(url.replace("MONTH", mon).replace("DAY", day).replace("YEAR", yr)).read())
            all_games = pd.concat([all_games, pd.read_csv(io.StringIO(soup.find("pre").text), sep = ";")])

all_games

table = soup.find_all('table')[7]

new_table = pd.DataFrame(columns=range(0,9), index = [0]) # I know the size 
    
row_marker = 0
for row in table.find_all('tr')[7]:
    column_marker = 0
    columns = row.find_all('td')
    print(row.find_all('td'))
    
    
    for column in columns:
        new_table.iat[row_marker,column_marker] = column.get_text()
        column_marker += 1