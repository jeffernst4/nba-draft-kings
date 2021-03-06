
### General -----

# Maybe replace fantasy points per min with something shorter

# ungroup() all group_by()'s

# Maybe remove fantasy_points_per_min (is there any need to have this anymore?)
# and model to predict fantasy points per min

# Find a way to add in new salary data from draft kings export and load it into
# engineered features

# Change stdDev to sd?



### Web Scraping -----

# Get player news from cbs to add to player salaries for predicting if players
# are playing and amount of minutes

# Scrape other projections to compare against my projections
# http://www.dailyfantasyfuel.com/nba/projections/draftkings/2017-05-15/

# Scrape draft king positions for every player using player ids (find a place for this file)
# http://rotoguru1.com/cgi-bin/playrh.cgi?4224



### Config -----

# Make stats and rules for fantasy points



### Data Transformation -----

# Order columns same - date, opponent, slug, etc.

# Identify playoff games and possibly omit, see if they're harder to predict

# Match up names from draft kings to basketball reference

# Omit data that can't be used, like seasons >= 2016 since 2015 doesn't have
# players that didn't play (12 missing players vs 10k+ missing players), keep
# all dates until end to have data for league opponent multipliers

# Remove extra columns in player box scores, and order columns

# Remove team stats and add player box scores stats used in team stats in
# feature engineering to team box scores transformation (rebounds, 3 pointers,
# etc.)

# There are some players that are in the player box scores, but not in the
# player salaries, wait until modeling to omit them since they can be useful to
# create rolling player averages

# Remove tot from player histories

# Maybe only load seasons >= 2015 for all data sources

# Convert location, position, etc to factors to be used for modeling



### Feature Engineering -----

# Calculate opponent multipliers

# Make minutes played std deviation using general std deviation formula

# Rename player stats for fantasy points per min to fantasy points per min

# Change function variables to be meaningful, like feature periods

# Maybe consider adding in defense against position or comparing fantasy points
# method with/without matchup comparisons, test out all ways to calculate matchup
# comparisons, maybe method for calculating fantasy points shouldn't be used in
# minutes prediction

# Maybe take into account location, see if individual players, clusters of
# players, teams, or all players are affected by location

# Create bag of words news metric
# https://cran.r-project.org/web/packages/ngram/vignettes/ngram-guide.pdf
# https://www.dataquest.io/blog/natural-language-processing-with-python/



### Model -----

# Add train and test set for OLS and fix prediction for fantasy points per min ols formula

# Make sure outcome variable for playing likelihood is categorical, so that
# randomforest converts to classification

# Create a dataset for each model before modeling

# Our projections are generated using sophisticated models that take into
# account factors such as: per minute averages, opponents defense, vegas
# odds/lines, rest - fantasycruncher

# Minutes played predictors
# - news
# - minutes played (rolling), maybe use % of total time to account for overtime
# - fantasy points (rolling)
# - salary / difference of salary
# - difference of skill with opponent, maybe use vegas line difference (closer
# games = better players play more and higher chance of overtime)
# - age, height and weight (all factors in getting injured) use team rosters for this
# - rest (higher rest = lower chance of being rested/injured), difference
# between being injured/rested?
# - number of teammates who are injured (weighted by skill and position)
# - starter in last game (is this better than just minutes played), use player
# salaries data



### Prediction -----

# Calculate fantasy_points_prediction earlier and use points_per_min_prediction

# Generalize prediction formulas

# Predicted fantasy points doesn't include double doubles or triple doubles

# Test out modeling turnovers with 12 minutes vs 6 minutes minimum, possibly put
# this in config since it needs to be calculated with std deviation



### Simulation -----

# Capitalize list names for stats per min with no underscores

# Make point values for draft kings based on formula



### Optimization -----

# Calculate required winning scores for every threshold
# https://rotogrinders.com/articles/what-it-really-takes-to-win-an-nba-gpp-1210935

# Get top 1000 lineups to maximize expected fantasy points
# https://stackoverflow.com/questions/34480151/r-combinations-of-a-dataframe-with-constraints/34480260










