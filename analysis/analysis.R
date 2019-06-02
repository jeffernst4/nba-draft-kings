
# Setup -----

# Load libraries
library(dplyr)
library(lpSolve)
library(lubridate)
library(plyr)
library(randomForest)
library(reshape2)
library(stringr)
library(tcltk2)
library(zoo)

# Load scripts
sapply(c("data load", "data transformation", "feature engineering", "modeling"), function(script)
  source(paste0("analysis/scripts/", script, ".R")))


# Data Load -----

# # Aggregate data
# data <- list(
#   NameCorrections = list(PlayerSalaries = dataLoad$NameCorrections("player salaries")),
#   PlayerHistories = dataLoad$PlayerHistories(),
#   PlayerSalaries = list(
#     DraftKings = dataLoad$PlayerSalaries(gameType = "draftkings"),
#     FanDuel = dataLoad$PlayerSalaries(gameType = "fanduel"),
#     Yahoo = dataLoad$PlayerSalaries(gameType = "yahoo")
#   ),
#   PlayerBoxScores = dataLoad$GameStats(dataType = "player box scores"),
#   TeamBoxScores = dataLoad$GameStats(dataType = "team box scores"),
#   PlayerSeasonTotals = dataLoad$GameStats(dataType = "player season totals"),
#   SeasonSchedules = dataLoad$GameStats(dataType = "season schedules")
# )

# # Save data
# save(data, file = "analysis/data/data.RData")

# Load data
load("analysis/data/data.RData")


# Data Transformation -----

# Transform season schedules
data$SeasonSchedules <-
  dataTransformation$SeasonSchedules(data$SeasonSchedules)

# Join season schedules with team box scores
data$TeamBoxScores <-
  join(data$TeamBoxScores, data$SeasonSchedules[, c("date", "team", "points", "opponent")])

# Transform team box scores
data$TeamBoxScores <-
  dataTransformation$TeamBoxScores(data$TeamBoxScores)

# Transform player season totals
data$PlayerSeasonTotals <-
  dataTransformation$PlayerSeasonTotals(data$PlayerSeasonTotals)

# Transform player history
data$PlayerHistories <-
  dataTransformation$PlayerHistories(data$PlayerHistories)

# Transform player salaries
data$PlayerSalaries <-
  lapply(c("DraftKings", "FanDuel", "Yahoo"), function(x)
    dataTransformation$PlayerSalaries(data$PlayerSalaries[[x]], x, data$NameCorrections$PlayerSalaries))

# Rename player salary list
names(data$PlayerSalaries) <- c("DraftKings", "FanDuel", "Yahoo")

# Join all player salaries
data$PlayerSalaries <-
  join(
    join(
      data$PlayerSalaries$DraftKings,
      data$PlayerSalaries$FanDuel,
      type = "full"
    ),
    data$PlayerSalaries$Yahoo,
    type = "full"
  )

# Join player histories with player salaries
data$PlayerSalaries <-
  join(data$PlayerSalaries, unique(data$PlayerHistories[, c("slug", "name", "team")]), type = "inner")

# Join player box scores with player salaries
data$PlayerSalaries <-
  join(data$PlayerSalaries, unique(data$PlayerBoxScores[, c("team", "date", "location", "opponent", "outcome")]))

# Join player salaries with player box scores
data$PlayerBoxScores <- join(data$PlayerBoxScores, data$PlayerSalaries, type = "full")

# Transform player box scores
data$PlayerBoxScores <-
  dataTransformation$PlayerBoxScores(data$PlayerBoxScores)

# Join season schedules with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$SeasonSchedules[, c("date", "team", "season")])

# Join player season totals with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$PlayerSeasonTotals[, c("slug", "season", "team", "position")])

# Transform team stats
data$TeamStats <-
  dataTransformation$TeamStats(data$PlayerBoxScores)

# Create player analysis
data$Analysis <-
  data$PlayerBoxScores[, c("date",
                           "season",
                           "slug",
                           "name",
                           "position",
                           "team",
                           "opponent",
                           "location",
                           "played_game",
                           "minutes_played",
                           "salary_draftkings",
                           "salary_fanduel",
                           "salary_yahoo",
                           "fantasy_points",
                           "fantasy_points_per_min")]


# Feature Engineering -----

# Create player minutes
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$Player_Minutes(data$PlayerBoxScores))

# Create player stats
data$Analysis <-
  join(
    data$Analysis,
    FeatureEngineering$Player_Stats(
      data$PlayerBoxScores,
      data$TeamStats
    )
  )

# Create player stats per min
data$Analysis <-
  join(
    data$Analysis,
    FeatureEngineering$Player_Stats_Per_Min(
      data$PlayerBoxScores,
      data$TeamStats
    )
  )


# Modeling -----

# Create empty list
model <- list(
  PlayingLikelihood = Modeling$RandomForest(
    data = data$Analysis,
    outcome = "played_game",
    predictors = c(
      "salary_draftkings",
      "salary_draftkings_change_1",
      "salary_draftkings_change_3",
      "salary_draftkings_change_5",
      "player_salary_change_10",
      "player_salary_change_20",
      "player_minutes_20",
      "player_minutes_10",
      "player_minutes_5",
      "player_minutes_3",
      "player_minutes_1"
    ),
    sample = 20000,
    ntree = 100
  ),
  PlayingTime = Modeling$RandomForest(
    data = na.omit(data$Analysis),
    outcome = "minutes_played",
    predictors = c(
      "salary_draftkings",
      "salary_fanduel",
      "salary_yahoo",
      "player_fp_20",
      "player_fp_10",
      "player_fp_5",
      "player_fp_4",
      "player_fp_3",
      "player_fp_2",
      "player_fp_1",
      "player_minutes_20",
      "player_minutes_15",
      "player_minutes_10",
      "player_minutes_9",
      "player_minutes_8",
      "player_minutes_7",
      "player_minutes_6",
      "player_minutes_3",
      "player_minutes_2",
      "player_minutes_1"
    ),
    sample = 2000,
    ntree = 250
  ),
  FantasyPoints = Modeling$RandomForest(
    data = na.omit(data$Analysis),
    outcome = "fantasy_points_per_min",
    predictors = c(
      "player_fp_per_min_1",
      "player_fp_per_min_2",
      "player_fp_per_min_3",
      "player_fp_per_min_4",
      "player_fp_per_min_5",
      "player_fp_per_min_10",
      "player_fp_per_min_20",
      "salary_yahoo",
      "salary_draftkings",
      "salary_fanduel"
    ),
    sample = 5000,
    ntree = 250
  )
)








analysis <- data$Analysis

analysis$minute_predictions <- predict(model$RandomForest, analysis)

analysis$fp_predictions <- predict(model$RandomForest2, analysis)

analysis$total_predictions <- analysis$minute_predictions * analysis$fp_predictions

summary(lm(fantasy_points ~ total_predictions, analysis))

modelValidation <- analysis[, c("name", "date", "fantasy_points", "total_predictions", "fantasy_points_per_min", "fp_predictions", "minutes_played", "minute_predictions")]

modelValidation$residuals <- abs(modelValidation$total_predictions - modelValidation$fantasy_points)

summary(modelValidation$residuals[modelValidation$minutes_played == 0])

summary(modelValidation$residuals[modelValidation$minutes_played != 0])

# Prediction -----

# Load data
data$Salaries <-
  read.csv("data/draft kings/dk salaries.csv", stringsAsFactors = FALSE, encoding = "utf-8")
data$Abbreviations <-
  read.csv("data/draft kings/abbreviations.csv", stringsAsFactors = FALSE, encoding = "utf-8")
data$NameCorrections <-
  read.csv("data/draft kings/name corrections.csv", stringsAsFactors = FALSE, encoding = "utf-8")

# Join salaries and abbreviations
data$Salaries <- join(data$Salaries, data$Abbreviations)

# Lower case all column names
names(data$Salaries) <- tolower(names(data$Salaries))

# Upper case all team names
data$Salaries$team <- toupper(data$Salaries$team)

# Clean player names
data$Salaries$name <- gsub(" Jr.", "", data$Salaries$name)
data$Salaries$name <- gsub(" III", "", data$Salaries$name)
data$Salaries$name <- gsub(" IV", "", data$Salaries$name)

# Filter name corrections
data$NameCorrections <-
  data$NameCorrections[data$NameCorrections$new_name != "",]

# Join salaries and name corrections
data$Salaries <- join(data$Salaries, data$NameCorrections)

# Replace name with new name
data$Salaries$name[!is.na(data$Salaries$new_name)] <-
  data$Salaries$new_name[!is.na(data$Salaries$new_name)]

# Filter salaries
data$Salaries <-
  data$Salaries[, c("name", "team", "new_name", "roster.position", "salary")]

# Filter player box scores
data$PlayerBoxScores <-
  data$PlayerBoxScores[, c("name", "team", "date", "fantasy_points")]

# Join player box scores to salaries
data$Salaries <- join(data$Salaries, data$PlayerBoxScores)

predictions <- do.call(data.frame,
        aggregate(fantasy_points ~ name + roster.position + team + salary, data$Salaries, function(x)
          c(
            player_fp_1 = mean(tail(x, 1)),
            player_fp_2 = mean(tail(x, 2)),
            player_fp_3 = mean(tail(x, 3)),
            player_fp_4 = mean(tail(x, 4)),
            player_fp_5 = mean(tail(x, 5)),
            player_fp_6 = mean(tail(x, 6)),
            player_fp_7 = mean(tail(x, 7)),
            player_fp_8 = mean(tail(x, 8)),
            player_fp_9 = mean(tail(x, 9)),
            player_fp_10 = mean(tail(x, 10)),
            player_fp_15 = mean(tail(x, 15)),
            player_fp_20 = mean(tail(x, 20))
          )))

names(predictions) <- gsub("fantasy_points.", "", names(predictions))

predictions$predictions <- predict(model$OLS, predictions)

availablePositions <-
  do.call("bind_rows", lapply(str_split(predictions$roster.position, "/"), table))

availablePositions <-
  na.omit(melt(
    cbind(predictions[, c("name", "team")], availablePositions),
    id.vars = c("name", "team")
  ))

availablePositions <- subset(availablePositions, select = -value)

names(availablePositions) <- gsub("variable", "position", names(availablePositions))

predictions <- join(predictions, availablePositions)












# Optimization -----

injuries <- read.csv("data/draft kings/injuries.csv")

predictions$injury[predictions$name %in% c(injuries$name, "DeAndre Jordan", "Julius Randle", "Jamal Murray")] <- 1

direction <- 'max'
objective.in <- predictions$predictions
const.mat <- rbind(1 * (predictions$salary),
                   1 * (predictions$name != ""),
                   1 * (predictions$position == "PG"),
                   1 * (predictions$position == "SG"),
                   1 * (predictions$position == "SF"),
                   1 * (predictions$position == "PF"),
                   1 * (predictions$position == "C"),
                   1 * (predictions$position == "G"),
                   1 * (predictions$position == "F"),
                   1 * (predictions$position == "UTIL"),
                   t(model.matrix(~ -1 + name, predictions)),
                   1 * (predictions$injury == 1),
                   deparse.level = 0)
const.dir <- c('<=', '<=', '=', '=', '=', '=', '=', '=', '=', '=', rep("<=", length(unique(predictions$name))), "=")
const.rhs <- c(50000, 8, 1, 1, 1, 1, 1, 1, 1, 1, rep(1, length(unique(predictions$name))), 0)


lp_solution <- lp(
  direction = direction,
  objective.in = objective.in,
  const.mat = const.mat,
  const.dir = const.dir,
  const.rhs = const.rhs,
  all.bin = TRUE,
  num.bin.solns = 1)

predictionssolution <- predictions[lp_solution$solution == TRUE,]

predictionssolution$position <- factor(predictionssolution$position, levels = c("PG", "SG", "SF", "PF", "C", "G", "F", "UTIL"))

predictionssolution[order(predictionssolution$position), ]



# Analyzing difference in stats by opponent

test <-
  aggregate(
    cbind(
      points,
      three_pointers_made,
      rebounds,
      assists,
      steals,
      blocks,
      turnovers,
      double_doubles,
      triple_doubles
    ) ~ opponent + date + season,
    playerBoxScores,
    sum
  )

test2 <-
  aggregate(
    cbind(
      points,
      three_pointers_made,
      rebounds,
      assists,
      steals,
      blocks,
      turnovers,
      double_doubles,
      triple_doubles
    ) ~ opponent,
    test[test$season == 2019, ],
    mean
  )

player <- "Tobias Harris"

mean(playerBoxScores$points[playerBoxScores$name == player &
                              playerBoxScores$opponent %in% c("INDIANA PACERS",
                                                              "MEMPHIS GRIZZLIES",
                                                              "MIAMI HEAT",
                                                              "UTAH JAZZ",
                                                              "ORLANDO MAGIC")])

mean(playerBoxScores$points[playerBoxScores$name == player &
                              playerBoxScores$opponent %in% c(
                                "ATLANTA HAWKS",
                                "WASHINGTON WIZARDS",
                                "NEW ORLEANS PELICANS",
                                "PHOENIX SUNS",
                                "SACRAMENTO KINGS"
                              )])













