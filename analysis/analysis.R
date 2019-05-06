
# do player averages with location taken into account

# See if kris and isacc humphries 

# put in correct order in optimizaiton part

# rename 3 pointers

# Clean up code



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


# Data Load -----

# Create a list of data load functions
dataLoad <- list(
  
  GameStats = function(fileType) {
    
    # Create empty data list
    dataList <- list()
    
    # Create file list
    fileList <-
      list.files(path = paste0("data/", fileType, "/"),
                 pattern = "*.csv")
    
    # Create progress bar
    progressBar <- tkProgressBar("Loading Data", "Loading...",
                                 0, 1, 0)
    
    # Load data to a list of data frames
    for(i in 1:length(fileList)) {
      
      if (fileType %in% c("player box scores", "team box scores")) {
        
        # Extract date
        fileDate <-
          as.Date(str_extract(fileList[i], "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
                  format = "%Y-%m-%d")
        
      } else if (fileType %in% c("player season totals", "season schedules")) {
        
        # Extract date
        fileDate <- str_extract(fileList[i], "[0-9]{4}")
        
      }
      
      # Read data
      data <-
        read.csv(
          paste0(
            "data/",
            fileType,
            "/",
            fileList[i]
          ),
          stringsAsFactors = FALSE,
          encoding = "utf-8"
        )
      
      # Check if data has more than one row
      if (nrow(data) > 0) {
        
        # Add date column to data
        data$date <- fileDate
        
        # Add data to list
        dataList <- c(dataList, list(data))
        
      }
      
      # Update progress bar
      setTkProgressBar(progressBar,
                       i / length(fileList),
                       paste0("Loading ", fileType, " box scores"),
                       sprintf("%d%% done", round(i * 100 / length(fileList))))
      
    }
    
    # Close progress bar
    close(progressBar)
    
    # Combine list into a single data frame
    data <- do.call("rbind", dataList)
    
    # Return data
    return(data)
    
  }
  
)

# Aggregate data
data <- list(
  PlayerBoxScores = dataLoad$GameStats(
    fileType = "player box scores"
  ),
  TeamBoxScores = dataLoad$GameStats(
    type = "team box scores"
  ),
  PlayerSeasonTotals = dataLoad$GameStats(
    fileType = "player season totals"
  ),
  SeasonSchedules = dataLoad$GameStats(
    fileType = "season schedules"
  )
)


# Team Data Transformation -----

dataTransformation <- list(
  
  # Transform player box scores
  PlayerBoxScores = function(playerBoxScores) {
    
    # Convert seconds to minutes
    playerBoxScores$seconds_played <- playerBoxScores$seconds_played / 60
    
    # Rename column
    names(playerBoxScores) <- gsub("seconds_played", "minutes_played", names(playerBoxScores))
    
    # Calculate made two points field goals
    playerBoxScores$made_two_point_field_goals <-
      playerBoxScores$made_field_goals - playerBoxScores$made_three_point_field_goals
    
    # Calculate attempted two point field goals
    playerBoxScores$attempted_two_point_field_goals <-
      playerBoxScores$attempted_field_goals - playerBoxScores$attempted_three_point_field_goals
    
    # Calculate rebounds
    playerBoxScores$rebounds <- playerBoxScores$offensive_rebounds + playerBoxScores$defensive_rebounds
    
    # Calculate points
    playerBoxScores$points <-
      colSums(apply(
        X = playerBoxScores[, c("made_two_point_field_goals",
                                "made_three_point_field_goals",
                                "made_free_throws")],
        MARGIN = 1,
        FUN = function(x)
          x * c(2, 3, 1)
      ))
    
    # Identify double-doubles
    playerBoxScores$double_doubles <-
      rowSums(playerBoxScores[, c("points", "rebounds", "assists", "blocks", "steals")] > 10) >= 2
    
    # Identify double-doubles
    playerBoxScores$triple_doubles <-
      rowSums(playerBoxScores[, c("points", "rebounds", "assists", "blocks", "steals")] > 10) >= 3
    
    # Calculate fantasy points
    playerBoxScores$fantasy_points <-
      colSums(apply(
        X = playerBoxScores[, c(
          "points",
          "made_three_point_field_goals",
          "rebounds",
          "assists",
          "steals",
          "blocks",
          "turnovers",
          "double_doubles",
          "triple_doubles"
        )],
        MARGIN = 1,
        FUN = function(x)
          x * c(1, 0.5, 1.25, 1.5, 2, 2, -0.5, 1.5, 3)
      ))
    
    # Create team stats data frame
    teamStats <-
      aggregate(
        cbind(
          minutes_played,
          attempted_field_goals,
          attempted_free_throws,
          turnovers
        ) ~ date + team,
        playerBoxScores,
        sum
      )
    
    # Calculate team minutes per possession
    teamStats$team_minutes_per_possession <-
      #teamStats$minutes_played / 
      (
        teamStats$attempted_field_goals + 0.44 * teamStats$attempted_free_throws + teamStats$turnovers
      )
    
    # Filter team stats
    teamStats <-
      teamStats[, c("date", "team", "team_minutes_per_possession")]
    
    # Join team stats with player box scores
    playerBoxScores <- join(playerBoxScores, teamStats)
    
    # Calculate usage rate
    playerBoxScores$usage_rate <-
      (
        playerBoxScores$attempted_field_goals + 0.44 * playerBoxScores$attempted_free_throws + playerBoxScores$turnovers
      ) / playerBoxScores$team_minutes_per_possession
    #/ (5 * playerBoxScores$minutes_played)
    
    # Return player box scores
    return(playerBoxScores)
    
  },
  
  # Transform team box scores
  TeamBoxScores = function(teamBoxScores) {
    
    # Calculate possessions
    teamBoxScores$possessions <-
      0.96 * (
        teamBoxScores$attempted_field_goals + teamBoxScores$turnovers + 0.44 * teamBoxScores$attempted_free_throws - teamBoxScores$offensive_rebounds
      )
    
    # Create opponent version of team box scores
    oppTeamBoxScores <- teamBoxScores[, c("date", "team", "points")]
    
    # Rename columns
    names(oppTeamBoxScores) <-
      gsub("team", "opponent", names(oppTeamBoxScores))
    names(oppTeamBoxScores) <-
      gsub("points", "opp_points", names(oppTeamBoxScores))
    
    # Join opponent team box scores with team box scores
    teamBoxScores <- join(teamBoxScores, oppTeamBoxScores)
    
    # Calculate offensive efficiency
    teamBoxScores$offensive_efficiency <-
      teamBoxScores$points / teamBoxScores$possessions
    
    # Calculate defensive efficiency
    teamBoxScores$defensive_efficiency <-
      teamBoxScores$opp_points / teamBoxScores$possessions
    
    # Return team box scores
    return(teamBoxScores)
    
  },
  
  # Transform player season totals
  PlayerSeasonTotals = function(playerSeasonTotals) {
    
    # Rename column
    names(playerSeasonTotals)[names(playerSeasonTotals) == "positions"] <-
      "position"
    
    # Return player season totals
    return(playerSeasonTotals)
    
  },
  
  # Transform season schedules
  SeasonSchedules = function(seasonSchedules) {
    
    # Convert start_time to include time zone attribute
    seasonSchedules$start_time <-
      as.POSIXct(strptime(seasonSchedules$start_time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
    
    # Convert start time to date
    seasonSchedules$start_time <- as.Date(seasonSchedules$start_time, tz = "US/Eastern")
    
    # Change column name
    names(seasonSchedules)[names(seasonSchedules) == "start_time"] <- "date"
    
    # Reshape home games
    homeSchedule <- melt(
      seasonSchedules[, names(seasonSchedules)[-grep("away_team_score", names(seasonSchedules))]],
      id.vars = c("date", "season", "away_team", "home_team_score"),
      variable.name = "location",
      value.name = "team"
    )
    
    # Reshape away games
    awaySchedule <- melt(
      seasonSchedules[, names(seasonSchedules)[-grep("home_team_score", names(seasonSchedules))]],
      id.vars = c("date", "season", "home_team", "away_team_score"),
      variable.name = "location",
      value.name = "team"
    )
    
    # Rename columns
    names(homeSchedule) <- gsub("home_team_score", "points", names(homeSchedule))
    names(awaySchedule) <- gsub("away_team_score", "points", names(awaySchedule))
    names(homeSchedule) <- gsub("away_team", "opponent", names(homeSchedule))
    names(awaySchedule) <- gsub("home_team", "opponent", names(awaySchedule))
    
    # Combine home and away schedules
    seasonSchedules <- rbind(homeSchedule, awaySchedule)
    
    # Change location text
    seasonSchedules$location <- toupper(gsub("_team", "", seasonSchedules$location))
    
    # Return season schedules
    return(seasonSchedules)
    
  }
  
)

# Transform season schedules
data$SeasonSchedules <-
  teamDataTransformation$SeasonSchedules(data$SeasonSchedules)

# Join season schedules with team box scores
data$TeamBoxScores <-
  join(data$TeamBoxScores, data$SeasonSchedules[, c("date", "team", "points", "opponent")])

# Transform team box scores
data$TeamBoxScores <-
  teamDataTransformation$TeamBoxScores(data$TeamBoxScores)

# Transform player season totals
data$PlayerSeasonTotals <-
  playerDataTransformation$PlayerSeasonTotals(data$PlayerSeasonTotals)

# Transform player box scores
data$PlayerBoxScores <-
  playerDataTransformation$PlayerBoxScores(data$PlayerBoxScores)

# Join season schedules with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$SeasonSchedules[, c("date", "team", "season")])

# Join player season totals with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$PlayerSeasonTotals[, c("slug", "season", "team", "position")])

# Create analysis data frame
data$Analysis <-
  data$PlayerBoxScores[, c("date",
                           "season",
                           "slug",
                           "name",
                           "position",
                           "team",
                           "opponent",
                           "location",
                           "fantasy_points")]


# Feature Engineering -----

FeatureEngineering <- list(
  
  # Calculate player fantasy points
  Player_FP = function(playerBoxScores) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("slug", "date", "fantasy_points")]
    
    # Create rolling averages
    playerBoxScores <- playerBoxScores %>%
      group_by(slug) %>%
      dplyr::mutate(
        player_fp_1 = c(NA, head(
          rollmean(fantasy_points, 1, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_2 = c(NA, head(
          rollmean(fantasy_points, 2, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_3 = c(NA, head(
          rollmean(fantasy_points, 3, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_4 = c(NA, head(
          rollmean(fantasy_points, 4, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_5 = c(NA, head(
          rollmean(fantasy_points, 5, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_6 = c(NA, head(
          rollmean(fantasy_points, 6, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_7 = c(NA, head(
          rollmean(fantasy_points, 7, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_8 = c(NA, head(
          rollmean(fantasy_points, 8, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_9 = c(NA, head(
          rollmean(fantasy_points, 9, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_10 = c(NA, head(
          rollmean(fantasy_points, 10, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_15 = c(NA, head(
          rollmean(fantasy_points, 15, na.pad = TRUE, align = "right"), -1
        )),
        player_fp_20 = c(NA, head(
          rollmean(fantasy_points, 20, na.pad = TRUE, align = "right"), -1
        ))
      )
    
    # Remove column
    playerBoxScores <-
      subset(playerBoxScores, select = -fantasy_points)
    
    # Return player box scores
    return(playerBoxScores)
    
  },
  
  # Calculate player minutes
  Player_Minutes = function(playerBoxScores) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("slug", "date", "minutes_played")]
    
    # Create rolling averages
    playerBoxScores <- playerBoxScores %>%
      group_by(slug) %>%
      dplyr::mutate(
        player_minutes_3 = c(NA, head(
          rollmean(minutes_played, 3, na.pad = TRUE, align = "right"), -1
        )),
        player_minutes_10 = c(NA, head(
          rollmean(minutes_played, 10, na.pad = TRUE, align = "right"), -1
        ))
      )
    
    # Remove column
    playerBoxScores <-
      subset(playerBoxScores, select = -minutes_played)
    
    # Return player box scores
    return(playerBoxScores)
    
  },
  
  # Calculate opponent fantasy points allowed by position
  Opponent_FP_Position = function(playerBoxScores) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("opponent", "date", "position", "fantasy_points")]
    
    # Calculate mean fantasy points per game
    playerBoxScores <-
      aggregate(fantasy_points ~ opponent + date + position, playerBoxScores, mean)
    
    # Create rolling averages
    playerBoxScores <- playerBoxScores %>%
      group_by(opponent, position) %>%
      dplyr::mutate(
        opp_fp_position_3 = c(NA, head(
          rollmean(fantasy_points, 3, na.pad = TRUE, align = "right"), -1
        )),
        opp_fp_position_10 = c(NA, head(
          rollmean(fantasy_points, 10, na.pad = TRUE, align = "right"), -1
        ))
      )
    
    # Remove column
    playerBoxScores <-
      subset(playerBoxScores, select = -fantasy_points)
    
    # Return player box scores
    return(playerBoxScores)
    
  },
  
  # Calculate team offensive efficiency
  Team_Offensive_Efficiency = function(teamBoxScores) {
    
    # Filter player box scores
    teamBoxScores <-
      teamBoxScores[, c("team", "date", "offensive_efficiency")]
    
    # Create rolling averages
    teamBoxScores <- teamBoxScores %>%
      group_by(team) %>%
      dplyr::mutate(
        team_offeff_3 = c(NA, head(
          rollmean(offensive_efficiency, 3, na.pad = TRUE, align = "right"), -1
        )),
        team_offeff_10 = c(NA, head(
          rollmean(offensive_efficiency, 10, na.pad = TRUE, align = "right"), -1
        ))
      )
    
    # Remove column
    teamBoxScores <-
      subset(teamBoxScores, select = -offensive_efficiency)
    
    # Return team box scores
    return(teamBoxScores)
    
  },
  
  # Calculate opponent defensive efficiency
  Opponent_Defensive_Efficiency = function(teamBoxScores) {
    
    # Filter player box scores
    teamBoxScores <-
      teamBoxScores[, c("team", "date", "defensive_efficiency")]
    
    # Create rolling averages
    teamBoxScores <- teamBoxScores %>%
      group_by(team) %>%
      dplyr::mutate(
        opp_defeff_3 = c(NA, head(
          rollmean(defensive_efficiency, 3, na.pad = TRUE, align = "right"), -1
        )),
        opp_defeff_10 = c(NA, head(
          rollmean(defensive_efficiency, 10, na.pad = TRUE, align = "right"), -1
        ))
      )
    
    # Remove column
    teamBoxScores <-
      subset(teamBoxScores, select = -defensive_efficiency)
    
    # Rename column
    names(teamBoxScores) <- gsub("team", "opponent", names(teamBoxScores))
    
    # Return team box scores
    return(teamBoxScores)
    
  },
  
  # Calculate player usage rate
  Player_Usage_Rate = function(playerBoxScores) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("slug", "date", "usage_rate")]
    
    # Create rolling averages
    playerBoxScores <- playerBoxScores %>%
      group_by(slug) %>%
      dplyr::mutate(
        player_usagerate_3 = c(NA, head(
          rollmean(usage_rate, 3, na.pad = TRUE, align = "right"), -1
        )),
        player_usagerate_10 = c(NA, head(
          rollmean(usage_rate, 10, na.pad = TRUE, align = "right"), -1
        ))
      )
    
    # Remove column
    playerBoxScores <-
      subset(playerBoxScores, select = -usage_rate)
    
    # Return player box scores
    return(playerBoxScores)
    
  }
  
)

# Create player fantasy points
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$Player_FP(data$PlayerBoxScores))

# Calculate player minutes
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$Player_Minutes(data$PlayerBoxScores))

# Calculate opponent fantasy points by position
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$Opponent_FP_Position(data$PlayerBoxScores))

# Calculate team offensive efficiency
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$Team_Offensive_Efficiency(data$TeamBoxScores))

# Calculate opponent defensive efficiency
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$Opponent_Defensive_Efficiency(data$TeamBoxScores))

# Calculate player usage rate
data$Analysis <-
  join(data$Analysis,
       FeatureEngineering$Player_Usage_Rate(data$PlayerBoxScores))


# Model -----

# Create empty list
model <- list()

# Specify outcome variable
model$Outcome <- "fantasy_points"

# Specify predictor variables
model$Predictors <-
  c(
    "player_fp_1",
    "player_fp_2",
    "player_fp_3",
    "player_fp_4",
    "player_fp_5",
    "player_fp_6",
    "player_fp_10",
    "player_fp_20"
    # "player_minutes_3",
    # "player_minutes_10",
    # "opp_fp_position_3",
    # "opp_fp_position_10",
    # "team_offeff_3",
    # "team_offeff_10",
    # "opp_defeff_3",
    # "opp_defeff_10",
    # "player_usagerate_3",
    # "player_usagerate_10"
  )

# Create linear model
model$OLS <- lm(as.formula(paste0(
  model$Outcome, " ~ ", paste(model$Predictors, collapse = " + ")
)),
data$Analysis)

# # Create random forest model
# model$RandomForest <- randomForest(as.formula(paste0(
#   model$Outcome, " ~ ", paste(model$Predictors, collapse = " + ")
# )),
# data = na.omit(data$Analysis[sample(257015, 5000), ]),
# importance = TRUE)


# Prediction -----

# Load data
data$Salaries <-
  read.csv("Data/Draft Kings/DKSalaries.csv", stringsAsFactors = FALSE, encoding = "utf-8")
data$Abbreviations <-
  read.csv("Data/Draft Kings/Abbreviations.csv", stringsAsFactors = FALSE, encoding = "utf-8")
data$NameCorrections <-
  read.csv("Data/Draft Kings/Name Corrections.csv", stringsAsFactors = FALSE, encoding = "utf-8")

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

injuries <- read.csv("Data/Draft Kings/injuries.csv")

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
      made_three_point_field_goals,
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
      made_three_point_field_goals,
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



# working on adjusted stats by team -----

test <-
  aggregate(
    cbind(
      points,
      made_three_point_field_goals,
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

test <- test %>%
  group_by(opponent) %>%
  dplyr::mutate(
    points_25 = c(NA, head(
      rollmean(points, 25, na.pad = TRUE, align = "right"), -1
    )),
    three_pointers_25 = c(NA, head(
      rollmean(made_three_point_field_goals, 25, na.pad = TRUE, align = "right"), -1
    )),
    rebounds_25 = c(NA, head(
      rollmean(rebounds, 25, na.pad = TRUE, align = "right"), -1
    )),
    assists_25 = c(NA, head(
      rollmean(assists, 25, na.pad = TRUE, align = "right"), -1
    )),
    steals_25 = c(NA, head(
      rollmean(steals, 25, na.pad = TRUE, align = "right"), -1
    )),
    blocks_25 = c(NA, head(
      rollmean(blocks, 25, na.pad = TRUE, align = "right"), -1
    )),
    turnovers_25 = c(NA, head(
      rollmean(turnovers, 25, na.pad = TRUE, align = "right"), -1
    )),
    double_doubles_25 = c(NA, head(
      rollmean(double_doubles, 25, na.pad = TRUE, align = "right"), -1
    )),
    triple_doubles_25 = c(NA, head(
      rollmean(triple_doubles, 25, na.pad = TRUE, align = "right"), -1
    ))
  )

# the calculation above calculates rolling 25 for each stat for every team for every date they play

# calculate mean last 25 games for each team for every possible date for each stat

test <- test %>%
  group_by() %>%
  dplyr::mutate(
    points_100 = c(NA, head(
      rollmean(points, 100, na.pad = TRUE, align = "right"), -1
    )),
    three_pointers_100 = c(NA, head(
      rollmean(made_three_point_field_goals, 100, na.pad = TRUE, align = "right"), -1
    )),
    rebounds_100 = c(NA, head(
      rollmean(rebounds, 100, na.pad = TRUE, align = "right"), -1
    )),
    assists_100 = c(NA, head(
      rollmean(assists, 100, na.pad = TRUE, align = "right"), -1
    )),
    steals_100 = c(NA, head(
      rollmean(steals, 100, na.pad = TRUE, align = "right"), -1
    )),
    blocks_100 = c(NA, head(
      rollmean(blocks, 100, na.pad = TRUE, align = "right"), -1
    )),
    turnovers_100 = c(NA, head(
      rollmean(turnovers, 100, na.pad = TRUE, align = "right"), -1
    )),
    double_doubles_100 = c(NA, head(
      rollmean(double_doubles, 100, na.pad = TRUE, align = "right"), -1
    )),
    triple_doubles_100 = c(NA, head(
      rollmean(triple_doubles, 100, na.pad = TRUE, align = "right"), -1
    ))
  )

# calculate [team rolling 25] / [mean rolling 25 for all teams] for every stat

test$points_multiplier <- test$points_25 / test$points_100
test$three_pointers_multiplier <- test$three_pointers_25 / test$three_pointers_100
test$rebounds_multiplier <- test$rebounds_25 / test$rebounds_100
test$assists_multiplier <- test$assists_25 / test$assists_100
test$steals_multiplier <- test$steals_25 / test$steals_100
test$blocks_multiplier <- test$blocks_25 / test$blocks_100
test$turnovers_multiplier <- test$turnovers_25 / test$turnovers_100
# test$double_doubles_multiplier <- test$double_doubles_25 / test$double_doubles_100
# test$triple_doubles_multiplier <- test$triple_doubles_25 / test$triple_doubles_100

# multiply that factor by each rolling average for every stat





