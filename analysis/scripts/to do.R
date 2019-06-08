
### General -----

# Remove all group_by() potentially

# Remove player_ from engineered features

# Maybe replace fantasy points per min with something shorter

# ungroup() all group_by()'s

# Find a better place for standard deviation calculations outside of feature
# engineering



### Web Scraping -----

# Get player news from cbs to add to player salaries for predicting if players
# are playing and amount of minutes

# Scrape other projections to compare against my projections
# http://www.dailyfantasyfuel.com/nba/projections/draftkings/2017-05-15/



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

# Rename player stats for fantasy points per min to fantasy points per min

# Change function variables to be meaningful, like feature periods

# Should the residual be created in the feature engineering or earlier?

# Maybe consider adding in defense against position or comparing fantasy points
# method with/without matchup comparisons, test out all ways to calculate matchup
# comparisons, maybe method for calculating fantasy points shouldn't be used in
# minutes prediction

# Maybe take into account location, see if individual players, clusters of
# players, teams, or all players are affected by location

# Maybe calculate opponent multipliers separately if used in both fp_per_min and
# fp

# Use team box scores intead of team stats

# Create bag of words news metric
# https://cran.r-project.org/web/packages/ngram/vignettes/ngram-guide.pdf
# https://www.dataquest.io/blog/natural-language-processing-with-python/

# Formula to replace nan's with earlier values:
# analysis$fp_10[is.nan(analysis$fp_10)] <- fp_20[is.nan(analysis$fp_10)], do
# this for both stats per min and salary change, MAKE THIS SIMPLER

# Generalize standard deviation calculation for feature



### Model -----

# Add train and test set for OLS and fix prediction for fantasy points per min ols formula

# Make sure outcome variable for playing likelihood is categorical, so that
# randomforest converts to classification

# Create a dataset for each model before modeling

# Our projections are generated using sophisticated models that take into
# account factors such as: per minute averages, opponents defense, vegas
# odds/lines, rest - fantasycruncher

# Add in double_doubles and triple_doubles to player fantasy points feature

# Minutes played predictors
# - news
# - minutes played (rolling), maybe use % of total time to account for overtime
# - fantasy points (rolling)
# - salary / difference of salary
# - difference of skill with opponent, maybe use vegas line difference (closer
# games = better players play more and higher chance of overtime)
# - age
# - rest (higher rest = lower chance of being rested/injured), difference
# between being injured/rested?
# - number of teammates who are injured (weighted by skill and position)
# - starter in last game (is this better than just minutes played), use player
# salaries data


### Optimization

# Calculate required winning scores for every threshold
# https://rotogrinders.com/articles/what-it-really-takes-to-win-an-nba-gpp-1210935

# Get top 1000 lineups to maximize expected fantasy points
# https://stackoverflow.com/questions/34480151/r-combinations-of-a-dataframe-with-constraints/34480260










