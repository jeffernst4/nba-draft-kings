
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
sapply(c("config", "data load", "data transformation", "feature engineering", "modeling"), function(script)
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
  lapply(config$SalaryTypes, function(x)
    dataTransformation$PlayerSalaries(data$PlayerSalaries[[x]], x, data$NameCorrections$PlayerSalaries))

# Rename player salary list
names(data$PlayerSalaries) <- config$SalaryTypes

# Join all player salaries
data$PlayerSalaries <-
  Reduce(function(x, y)
    join(x, y, type = "full"), data$PlayerSalaries)

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
  dataTransformation$PlayerBoxScores(data$PlayerBoxScores, config)

# Join season schedules with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$SeasonSchedules[, c("date", "team", "season")])

# Join player season totals with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$PlayerSeasonTotals[, c("slug", "season", "team", "position")])

# # Transform team stats
# data$TeamStats <-
#   dataTransformation$TeamStats(data$PlayerBoxScores)

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
                           config$StatsNew,
                           paste0(config$StatsNew, "_per_min"),
                           paste0("salary_", tolower(config$SalaryTypes)),
                           "fantasy_points",
                           "fantasy_points_per_min")]


# Feature Engineering -----

# Create salary change
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$SalaryChange(data$PlayerBoxScores, config))

# Create player minutes
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$MinutesPlayed(data$PlayerBoxScores, config))

# # Create player stats
# data$Analysis <-
#   join(
#     data$Analysis,
#     FeatureEngineering$Player_Stats(
#       data$PlayerBoxScores,
#       data$TeamStats
#     )
#   )

# # Create fantasy points per min
# data$Analysis <-
#   join(
#     data$Analysis,
#     FeatureEngineering$FantasyPointsPerMin(
#       data$PlayerBoxScores,
#       data$TeamBoxScores,
#       config
#     )
#   )

# Create stats per min
data$Analysis <-
  join(
    data$Analysis,
    FeatureEngineering$StatsPerMin(
      data$PlayerBoxScores,
      data$TeamBoxScores,
      config
    )
  )


# Modeling -----

# Create modeling data
data$Modeling <- list(
  
  PlayedGame = data$Analysis[, c(
    "played_game",
    "salary_draftkings",
    "salary_draftkings_change_1",
    "salary_draftkings_change_3",
    "salary_draftkings_change_5",
    "salary_draftkings_change_10",
    # "salary_draftkings_change_20",
    "minutes_played_1",
    "minutes_played_3",
    "minutes_played_5",
    "minutes_played_10"
    # "minutes_played_20"
  )],
  
  MinutesPlayed = data$Analysis[data$Analysis$played_game == "1", c(
    "minutes_played",
    "salary_draftkings",
    # "salary_draftkings_change_1",
    "salary_draftkings_change_3",
    # "salary_draftkings_change_5",
    "salary_draftkings_change_10",
    # "salary_draftkings_change_20",
    "minutes_played_1",
    "minutes_played_3",
    "minutes_played_5",
    "minutes_played_10"
    # "minutes_played_20"
  )],
  
  FantasyPoints = data$Analysis[data$Analysis$played_game == "1", c(
    "fantasy_points_per_min",
    # "salary_draftkings",
    # "salary_draftkings_change_1",
    # # "salary_draftkings_change_3",
    # "salary_draftkings_change_5",
    # "salary_draftkings_change_10",
    config$StatsNew,
    "two_pointers_made_per_min_10",
    "three_pointers_made_per_min_10",
    "rebounds_per_min_10",
    "assists_per_min_10",
    "steals_per_min_10",
    "blocks_per_min_10",
    "turnovers_per_min_10"
    # "location",
    # "season",
    # "position"
  )],
  
  Rebounds = data$Analysis[data$Analysis$played_game == "1", c(
    "rebounds_per_min",
    "rebounds_per_min_10"
  )]
  
)

# Create empty list
model <- list(
  PlayedGame = Modeling$RandomForest(
    data = data$Modeling$PlayedGame,
    outcome = "played_game",
    sampleSize = 10000,
    ntree = 100,
    train = FALSE
  ),
  MinutesPlayed = Modeling$RandomForest(
    data = data$Modeling$MinutesPlayed,
    outcome = "minutes_played",
    sampleSize = 5000,
    ntree = 100,
    train = FALSE
  ),
  # MinutesPlayed = Modeling$OLS(
  #   data = data$Modeling$MinutesPlayed,
  #   outcome = "minutes_played"
  # ),
  FantasyPoints = Modeling$RandomForest(
    data = data$Modeling$FantasyPoints,
    outcome = "fantasy_points_per_min",
    sampleSize = 5000,
    ntree = 100,
    train = TRUE
  ),
  FantasyPoints = Modeling$OLS(
    data = data$Modeling$FantasyPoints,
    outcome = "fantasy_points_per_min"
  ),
  Rebounds = Modeling$RandomForest(
    data = data$Modeling$Rebounds,
    outcome = "rebounds_per_min",
    sampleSize = 5000,
    ntree = 100,
    train = TRUE
  )
)




# Prediction
data$Analysis$played_game_prediction <-
  predict(model$PlayedGame$Model, data$Analysis, type = "prob")[, "1"]

data$Analysis$minutes_played_prediction <-
  predict(model$MinutesPlayed$Model, data$Analysis)

data$Analysis$fantasy_points_per_min_prediction <-
  predict(model$FantasyPoints, data$Analysis)

# Create minutes played standard deviation
data$Analysis <-
  join(
    data$Analysis,
    FeatureEngineering$MinutesPlayedStdDev(data$Analysis)
  )

# Create fantasy points per min standard deviation
data$Analysis <-
  join(
    data$Analysis,
    FeatureEngineering$FantasyPointsPerMinStdDev(data$Analysis)
  )





simulationData <- data$Analysis[data$Analysis$date == "2019-04-01", ]

# sum(
#   simulationData$minutes_played_prediction[!is.na(simulationData$minutes_played_prediction) &
#                                              !is.na(simulationData$minutes_played_std_dev)] * simulationData$played_game_prediction[!is.na(simulationData$minutes_played_prediction) &
#                                                                                                                                       !is.na(simulationData$minutes_played_std_dev)]
# )
# 
# sum(simulationData$minutes_played[!is.na(simulationData$minutes_played_prediction) &
#                                     !is.na(simulationData$minutes_played_std_dev)])


played_game_simulation <-
  lapply(simulationData$played_game_prediction, function(x)
    rbinom(10000, 1, x))

minutes_played_simulation <-
  mapply(
    function(x, y)
      rnorm(10000, x, y),
    simulationData$minutes_played_prediction,
    simulationData$minutes_played_std_dev,
    SIMPLIFY = FALSE
  )

fantasy_points_per_min_simulation <-
  mapply(
    function(x, y)
      rnorm(10000, x, y),
    simulationData$fantasy_points_per_min_prediction,
    simulationData$fantasy_points_per_min_std_dev,
    SIMPLIFY = FALSE
  )

test <- mapply(function(x, y, z)
  x * y * z,
  played_game_simulation,
  minutes_played_simulation,
  fantasy_points_per_min_simulation,
  SIMPLIFY = FALSE)

# test <- unlist(test)
# 
# test[test < 0] <- 0
# 
# 
# quantile(simulationData$minutes_played[!is.na(simulationData$minutes_played_prediction) & !is.na(simulationData$minutes_played_std_dev)], probs = seq(0,1, .05))
# 
# minplayedsum <- sum(simulationData$minutes_played[!is.na(simulationData$minutes_played_prediction) & !is.na(simulationData$minutes_played_std_dev)])
# 
# quantile(unlist(test), probs = seq(0, 1, .05), na.rm = TRUE)
# 
# testsum <- sum(unlist(test), na.rm = TRUE) / 10000
# 
# minplayedsum
# testsum
# 
# testsum / minplayedsum







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













