
# do player averages with location taken into account

# See if kris and isacc humphries 

# put in correct order in optimizaiton part

# Clean up code

# find out what the roster is for each game to determine who was missing from
# each game and try to predict if player will play in the game

# Order columns same, date opponent slug, etc...

# make stats and amounts for fantasy points as config

# remove all group_by()

# identify playoff games and possibly remove, see if they're harder to predict

# Get player news from cbs to add to player salaries for predicting time played

# Match up names from basketball reference, draft kings, and rotoguru

# Fix names of people who have weird prefix/suffix's

# Use player salaries to find players who didn't play


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
                       paste0("Loading ", fileType),
                       sprintf("%d%% done", round(i * 100 / length(fileList))))
      
    }
    
    # Close progress bar
    close(progressBar)
    
    # Combine list into a single data frame
    data <- do.call("rbind", dataList)
    
    # Return data
    return(data)
    
  },
  
  PlayerSalaries = function(fileType, gameType) {
    
    # Create empty data list
    dataList <- list()
    
    # Create file list
    fileList <-
      list.files(path = paste0("data/", fileType, "/", gameType, "/"),
                 pattern = "*.csv")
    
    # Create progress bar
    progressBar <- tkProgressBar("Loading Data", "Loading...",
                                 0, 1, 0)
    
    # Load data to a list of data frames
    for(i in 1:length(fileList)) {
        
      # Extract date
      fileDate <-
        as.Date(str_extract(fileList[i], "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
                format = "%Y-%m-%d")
      
      # Read data
      data <-
        read.csv(
          paste0("data/",
                 fileType,
                 "/",
                 gameType,
                 "/",
                 fileList[i]),
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
                       paste0("Loading ", gameType, " ", fileType),
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
    fileType = "team box scores"
  ),
  PlayerSeasonTotals = dataLoad$GameStats(
    fileType = "player season totals"
  ),
  SeasonSchedules = dataLoad$GameStats(
    fileType = "season schedules"
  ),
  PlayerSalaries = list(
    DraftKings = dataLoad$PlayerSalaries(
      fileType = "player salaries",
      gameType = "draftkings"
    ),
    FanDuel = dataLoad$PlayerSalaries(
      fileType = "player salaries",
      gameType = "fanduel"
    ),
    Yahoo = dataLoad$PlayerSalaries(
      fileType = "player salaries",
      gameType = "yahoo"
    )
  )
)


# Data Transformation -----

dataTransformation <- list(
  
  # Transform player box scores
  PlayerBoxScores = function(playerBoxScores) {
    
    # Rename columns
    names(playerBoxScores) <- gsub("made_field_goals", "field_goals_made", names(playerBoxScores))
    names(playerBoxScores) <- gsub("attempted_field_goals", "field_goals_attempted", names(playerBoxScores))
    names(playerBoxScores) <- gsub("made_three_point_field_goals", "three_pointers_made", names(playerBoxScores))
    names(playerBoxScores) <- gsub("attempted_three_point_field_goals", "three_pointers_attempted", names(playerBoxScores))
    names(playerBoxScores) <- gsub("made_free_throws", "free_throws_made", names(playerBoxScores))
    names(playerBoxScores) <- gsub("attempted_free_throws", "free_throws_attempted", names(playerBoxScores))
    
    # Convert seconds to minutes
    playerBoxScores$seconds_played <- playerBoxScores$seconds_played / 60
    
    # Rename column
    names(playerBoxScores) <- gsub("seconds_played", "minutes_played", names(playerBoxScores))
    
    # Calculate made two points field goals
    playerBoxScores$two_pointers_made <-
      playerBoxScores$field_goals_made - playerBoxScores$three_pointers_made
    
    # Calculate attempted two point field goals
    playerBoxScores$two_pointers_attempted <-
      playerBoxScores$field_goals_attempted - playerBoxScores$three_pointers_attempted
    
    # Calculate rebounds
    playerBoxScores$rebounds <- playerBoxScores$offensive_rebounds + playerBoxScores$defensive_rebounds
    
    # Calculate points
    playerBoxScores$points <-
      colSums(apply(
        X = playerBoxScores[, c("two_pointers_made",
                                "three_pointers_made",
                                "free_throws_made")],
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
          "three_pointers_made",
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
          field_goals_attempted,
          free_throws_attempted,
          turnovers
        ) ~ date + team,
        playerBoxScores,
        sum
      )
    
    # Calculate team minutes per possession
    teamStats$team_minutes_per_possession <-
      #teamStats$minutes_played / 
      (
        teamStats$field_goals_attempted + 0.44 * teamStats$free_throws_attempted + teamStats$turnovers
      )
    
    # Filter team stats
    teamStats <-
      teamStats[, c("date", "team", "team_minutes_per_possession")]
    
    # Join team stats with player box scores
    playerBoxScores <- join(playerBoxScores, teamStats)
    
    # Calculate usage rate
    playerBoxScores$usage_rate <-
      (
        playerBoxScores$field_goals_attempted + 0.44 * playerBoxScores$free_throws_attempted + playerBoxScores$turnovers
      ) / playerBoxScores$team_minutes_per_possession
    #/ (5 * playerBoxScores$minutes_played)
    
    # Return player box scores
    return(playerBoxScores)
    
  },
  
  # Transform player salaries
  PlayerSalaries = function(playerSalaries) {
    
    # Format na's
    playerSalaries$salary <- gsub("N/A", NA, playerSalaries$salary)
    
    # Format salaries to numeric
    playerSalaries$salary <-
      as.numeric(gsub('[$,]', '', playerSalaries$salary))
    
    # Format names
    playerSalaries$name <-
      gsub("\\^", "", sub("(\\w+),\\s(\\w+)", "\\2 \\1", playerSalaries$name))
    
    # Return player box scores
    return(playerSalaries)
    
  },
  
  # Transform team box scores
  TeamBoxScores = function(teamBoxScores) {
    
    # Rename columns
    names(teamBoxScores) <- gsub("made_field_goals", "field_goals_made", names(teamBoxScores))
    names(teamBoxScores) <- gsub("attempted_field_goals", "field_goals_attempted", names(teamBoxScores))
    names(teamBoxScores) <- gsub("made_three_point_field_goals", "three_pointers_made", names(teamBoxScores))
    names(teamBoxScores) <- gsub("attempted_three_point_field_goals", "three_pointers_attempted", names(teamBoxScores))
    names(teamBoxScores) <- gsub("made_free_throws", "free_throws_made", names(teamBoxScores))
    names(teamBoxScores) <- gsub("attempted_free_throws", "free_throws_attempted", names(teamBoxScores))
    
    # Create opponent version of team box scores
    oppTeamBoxScores <- teamBoxScores[, c("date", "team", "points")]
    
    # Rename columns
    names(oppTeamBoxScores) <-
      gsub("team", "opponent", names(oppTeamBoxScores))
    names(oppTeamBoxScores) <-
      gsub("points", "opponent_points", names(oppTeamBoxScores))
    
    # Join opponent team box scores with team box scores
    teamBoxScores <- join(teamBoxScores, oppTeamBoxScores)
    
    # Return team box scores
    return(teamBoxScores)
    
  },
  
  # Transform player season totals
  PlayerSeasonTotals = function(playerSeasonTotals) {
    
    # Rename columns
    names(playerSeasonTotals) <-
      gsub("positions", "position", names(playerSeasonTotals))
    names(playerSeasonTotals) <-
      gsub("date", "season", names(playerSeasonTotals))
    
    # Return player season totals
    return(playerSeasonTotals)
    
  },
  
  # Transform season schedules
  SeasonSchedules = function(seasonSchedules) {
    
    # Rename columns
    names(seasonSchedules) <-
      gsub("date", "season", names(seasonSchedules))
    
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
    
  },
  
  TeamStats = function(playerBoxScores) {
    
    # Create team stats
    teamStats <-
      aggregate(
        cbind(
          points,
          three_pointers_made,
          rebounds,
          assists,
          steals,
          blocks,
          turnovers
        ) ~ location + team + opponent + season + date,
        playerBoxScores,
        sum
      )
    
    # Return team stats
    return(teamStats)
    
  }
  
)

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

# Transform player box scores
data$PlayerBoxScores <-
  dataTransformation$PlayerBoxScores(data$PlayerBoxScores)

# Transform player salaries
data$PlayerSalaries <-
  dataTransformation$PlayerSalaries(data$PlayerSalaries$DraftKings)

# Join season schedules with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$SeasonSchedules[, c("date", "team", "season")])

# Join player season totals with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$PlayerSeasonTotals[, c("slug", "season", "team", "position")])

# Join player salaries with player box scores
data$PlayerBoxScores <-
  join(data$PlayerBoxScores, data$PlayerSalaries[, c("date", "name", "team", "salary")], type = "left")

# Create team stats
data$TeamStats <-
  dataTransformation$TeamStats(data$PlayerBoxScores)

# Create player analysis
data$PlayerAnalysis <-
  data$PlayerBoxScores[, c("date",
                           "season",
                           "slug",
                           "name",
                           "position",
                           "team",
                           "opponent",
                           "location",
                           "minutes_played",
                           "salary",
                           "fantasy_points")]

# Create team analysis
data$OpponentAnalysis <-
  data$TeamStats[, c("opponent", "date", "season", "location")]


# Feature Engineering -----

FeatureEngineering <- list(
  
  # Calculate player fantasy points
  Player_Minutes = function(playerBoxScores) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("slug", "date", "minutes_played")]
    
    # Create rolling averages
    playerMinutes <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(sapply(c(1:10, 15, 20), function(x)
          paste0(
            "player_minutes_",
            x,
            " = c(NA, head(rollmean(minutes_played, ",
            x,
            ", na.pad = TRUE, align = 'right'), -1))",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Return player stats
    return(playerMinutes)
    
  },
  
  # Calculate opponent multipliers
  Opponent_Multipliers = function(teamStats) {
    
    # Create stats analysis
    statsAnalysis <- teamStats[, c("opponent", "date", "season", "location")]
    
    # Specify stats
    stats <- c(
      "points",
      "three_pointers_made",
      "rebounds",
      "assists",
      "steals",
      "blocks",
      "turnovers"
    )
    
    # Filter team stats
    teamStats <-
      teamStats[, c("opponent", "date", "season", "location", stats)]
    
    # Create opponent stats
    opponentStats <- eval(parse(
      text = paste0(
        "teamStats %>% group_by(opponent, location) %>% dplyr::mutate(",
        paste0(
          "opponent_",
          stats,
          "_25 = c(NA, head(rollmean(",
          stats,
          ", 25, na.pad = TRUE, align = 'right'), -1))",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    opponentStats <-
      opponentStats[, !(names(opponentStats) %in% stats)]
    
    # Join opponent stats
    statsAnalysis <- join(statsAnalysis, opponentStats)
    
    # Create league stats
    leagueStats <- eval(parse(
      text = paste0(
        "teamStats %>% group_by() %>% dplyr::mutate(",
        paste0(
          "league_",
          stats,
          "_100 = c(NA, head(rollmean(",
          stats,
          ", 100, na.pad = TRUE, align = 'right'), -1))",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    leagueStats <-
      leagueStats[, !(names(leagueStats) %in% stats)]
    
    # Join league stats
    statsAnalysis <- join(statsAnalysis, leagueStats)
    
    # Calculate opponent multipliers
    opponentMultipliers <- eval(parse(
      text = paste0(
        "statsAnalysis %>% group_by() %>% dplyr::mutate(",
        paste0(
          stats,
          "_multiplier = opponent_",
          stats,
          "_25 / league_",
          stats,
          "_100",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    opponentMultipliers <-
      opponentMultipliers[,!(names(opponentMultipliers) %in% c(
        stats,
        paste0("opponent_", stats, "_25"),
        paste0("league_", stats, "_100")
      ))]
    
    # Return opponent multipliers
    return(opponentMultipliers)
    
  },
  
  # Calculate player fantasy points
  Player_Stats = function(playerBoxScores, opponentAnalysis) {
    
    # Specify stats
    stats <- c(
      "points",
      "three_pointers_made",
      "rebounds",
      "assists",
      "steals",
      "blocks",
      "turnovers"
    )
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("slug", "date", "opponent", "location", stats)]
    
    # Filter opponent analysis
    opponentAnalysis <-
      opponentAnalysis[, c("date", "opponent", "location", paste0(stats, "_multiplier"))]
    
    # Join opponent analysis
    playerBoxScores <- join(playerBoxScores, opponentAnalysis)
    
    # Create rolling averages
    playerStats <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(sapply(c(1:10, 15, 20), function(x)
          paste0(
            "player_",
            stats,
            "_",
            x,
            " = c(NA, head(rollmean(",
            stats,
            ", ",
            x,
            ", na.pad = TRUE, align = 'right'), -1)) * ",
            stats,
            "_multiplier",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Remove columns
    playerStats <-
      playerStats[,!(names(playerStats) %in% c(stats, paste0(stats, "_multiplier")))]

    # Create rolling averages
    playerStats <- eval(parse(
      text = paste0(
        "playerStats %>% group_by() %>% dplyr::mutate(",
        paste0(sapply(c(1:10, 15, 20), function(x)
          paste0(
            "player_fp_",
            x,
            " = colSums(apply(X = playerStats[, c(paste0('player_', stats, '_', ",
            x,
            "))], MARGIN = 1, FUN = function(x) x * c(1, 0.5, 1.25, 1.5, 2, 2, -0.5)))",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Remove columns
    playerStats <-
      playerStats[, !(names(playerStats) %in% sapply(c(1:10, 15, 20), function(x)
        paste0("player_", stats, "_", x)))]
    
    # Return player stats
    return(playerStats)
    
  }
  
)

# Create player minutes
data$PlayerAnalysis <-
  join(data$PlayerAnalysis,
       FeatureEngineering$Player_Minutes(data$PlayerBoxScores))

# Create opponent multipliers
data$OpponentAnalysis <-
  FeatureEngineering$Opponent_Multipliers(data$TeamStats)

# Create player stats
data$PlayerAnalysis <-
  join(data$PlayerAnalysis,
       FeatureEngineering$Player_Stats(data$PlayerBoxScores, data$OpponentAnalysis))


# Model -----

# Create empty list
model <- list()

# Specify outcome variable
model$Outcome <- "fantasy_points"

# Specify stats
stats <- c(
  "points",
  "three_pointers_made",
  "rebounds",
  "assists",
  "steals",
  "blocks",
  "turnovers",
  "double_doubles"
)

# Specify predictor variables
model$Predictors <-
  c(
    "player_fp_1",
    "player_fp_2",
    "player_fp_3",
    "player_fp_6",
    "player_fp_10",
    "player_fp_20"
  )

# Create linear model
model$OLS <- lm(as.formula(paste0(
  model$Outcome, " ~ ", paste(model$Predictors, collapse = " + ")
)),
na.omit(data$PlayerAnalysis))

summary(model$OLS)

# # Create random forest model
# model$RandomForest <- randomForest(as.formula(paste0(
#   model$Outcome, " ~ ", paste(model$Predictors, collapse = " + ")
# )),
# data = na.omit(data$Analysis[sample(257015, 5000), ]),
# importance = TRUE)


### MINUTES PLAYED

# predictors: news, minutes_played (last games), fantasy points (last games),
# salary, difference of opponent vs team (maybe?, chance of overtime and also
# better players play more, more difference means worse player will play more),
# account for blowouts and overtimes (maybe use % of total team time rather than
# actual playing time)

# https://cran.r-project.org/web/packages/ngram/vignettes/ngram-guide.pdf

# https://www.dataquest.io/blog/natural-language-processing-with-python/

# Create random forest model
model$RandomForest <-
  randomForest(
    minutes_played ~ salary + player_fp_20 + player_fp_10 + player_fp_5 + player_fp_4 + player_fp_3 + player_fp_2 + player_fp_1 + player_minutes_20 + player_minutes_10 + player_minutes_6 + player_minutes_3 + player_minutes_2 + player_minutes_1,
    data = na.omit(data$PlayerAnalysis[sample(257015, 20000),]),
    importance = TRUE
  )

### R2 currently at .5471 with "player_fp_1", "player_fp_2", "player_fp_3", "player_fp_6", "player_fp_10", "player_fp_20"


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



# working on adjusted stats by team -----

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

test <- test %>%
  group_by(opponent) %>%
  dplyr::mutate(
    points_25 = c(NA, head(
      rollmean(points, 25, na.pad = TRUE, align = "right"), -1
    )),
    three_pointers_25 = c(NA, head(
      rollmean(three_pointers_made, 25, na.pad = TRUE, align = "right"), -1
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
      rollmean(three_pointers_made, 100, na.pad = TRUE, align = "right"), -1
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

# calculate [team rolling 25] / [mean rolling 100 for all teams] for every stat

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





