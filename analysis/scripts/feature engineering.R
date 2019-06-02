
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
            ", fill = NA, align = 'right'), -1))",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Return player stats
    return(playerMinutes)
    
  },
  
  # Calculate player salaries
  Player_Salaries = function(playerBoxScores) {
    
    # Declare salary types
    salaryTypes <- c("draftkings")
    
    # Create rolling averages
    playerBoxScores <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(sapply(c(1, 3, 5, 10, 20), function(x)
          paste0(
            "salary_",
            salaryTypes,
            "_change_",
            x,
            " = round(salary_",
            salaryTypes,
            " / c(NA, head(rollmean(salary_",
            salaryTypes,
            ", ",
            x,
            ", fill = NA, align = 'right', na.rm = TRUE), -1)) - 1, 5)",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    playerBoxScores$salary_draftkings_change_10[is.nan(playerBoxScores$salary_draftkings_change_10)] <-
      playerBoxScores$salary_draftkings_change_20[is.nan(playerBoxScores$salary_draftkings_change_10)]
    
    playerBoxScores$salary_draftkings_change_5[is.nan(playerBoxScores$salary_draftkings_change_5)] <-
      playerBoxScores$salary_draftkings_change_10[is.nan(playerBoxScores$salary_draftkings_change_5)]
    
    playerBoxScores$salary_draftkings_change_3[is.nan(playerBoxScores$salary_draftkings_change_3)] <-
      playerBoxScores$salary_draftkings_change_5[is.nan(playerBoxScores$salary_draftkings_change_3)]
    
    playerBoxScores$salary_draftkings_change_1[is.nan(playerBoxScores$salary_draftkings_change_1)] <-
      playerBoxScores$salary_draftkings_change_3[is.nan(playerBoxScores$salary_draftkings_change_1)]
    
    model1 <- Modeling$RandomForest(
      data = playerBoxScores,
      outcome = "played_game",
      predictors = c(
        "salary_draftkings",
        "salary_draftkings_change_1",
        "salary_draftkings_change_3",
        "salary_draftkings_change_5",
        "salary_draftkings_change_10",
        "salary_draftkings_change_20",
        "player_minutes_20",
        "player_minutes_10",
        "player_minutes_5",
        "player_minutes_3",
        "player_minutes_1"
      ),
      sample = 50000,
      ntree = 100
    )
    
    playerBoxScores$prediction_orig <- predict(model1, playerBoxScores)
    playerBoxScores$prediction_new <- predict(model2, playerBoxScores)
    playerBoxScores$residuals_orig <- playerBoxScores$prediction_orig == playerBoxScores$played_game
    playerBoxScores$residuals_new <- playerBoxScores$prediction_new == playerBoxScores$played_game
    
  },
  
  # Calculate player fantasy points
  Player_Stats = function(playerBoxScores, teamStats) {
    
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
    
    # Create stats analysis
    statsAnalysis <- teamStats[, c("opponent", "date", "location")]
    
    # Filter team stats
    teamStats <-
      teamStats[, c("opponent", "date", "location", stats)]
    
    # Create opponent stats
    opponentStats <- eval(parse(
      text = paste0(
        "teamStats %>% group_by(opponent, location) %>% dplyr::mutate(",
        paste0(
          "opponent_",
          stats,
          "_25 = c(NA, head(rollmean(",
          stats,
          ", 25, fill = NA, align = 'right'), -1))",
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
          ", 100, fill = NA, align = 'right'), -1))",
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
    
    # Join opponent analysis
    playerBoxScores <- join(playerBoxScores, opponentMultipliers)
    
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
            ", fill = NA, align = 'right'), -1)) * ",
            stats,
            "_multiplier",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Remove columns
    playerStats <-
      playerStats[, !(names(playerStats) %in% c(stats, paste0(stats, "_multiplier")))]
    
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
    
  },
  
  # Calculate player fantasy points
  Player_Stats_Per_Min = function(playerBoxScores, teamStats) {
    
    ### remove stats per min for player box scores and replace with just stats
    ### get fp_per_min_1:5,10,15,20 by using rollsum find a way to do last 20,
    ### 40, 80, 120 minutes, otherwise, just use more general games to replace
    ### more short-term games maybe sum up minutes for each date for each player
    ### and pick out minutes that are at each level, I think the first option is
    ### better
    
    ### analysis$fp_10[is.nan(analysis$fp_10)] <- fp_20[is.nan(analysis$fp_10)]
    
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
      playerBoxScores[, c(
        "slug",
        "date",
        "opponent",
        "location",
        "minutes_played",
        stats
      )]
    
    # Create stats analysis
    statsAnalysis <- teamStats[, c("opponent", "date", "location")]
    
    # Filter team stats
    teamStats <-
      teamStats[, c("opponent", "date", "location", "minutes_played", stats)]
    
    # Create opponent stats
    opponentStats <- eval(parse(
      text = paste0(
        "teamStats %>% group_by(opponent, location) %>% dplyr::mutate(",
        paste0(
          "opponent_",
          stats,
          "_per_min_25 = c(NA, head(rollsum(",
          stats,
          ", 25, fill = NA, align = 'right') / rollsum(minutes_played, 25, fill = NA, align = 'right'), -1))",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    opponentStats <-
      opponentStats[, !(names(opponentStats) %in% c("minutes_played", stats))]
    
    # Join opponent stats
    statsAnalysis <- join(statsAnalysis, opponentStats)
    
    # Create league stats
    leagueStats <- eval(parse(
      text = paste0(
        "teamStats %>% group_by() %>% dplyr::mutate(",
        paste0(
          "league_",
          stats,
          "_per_min_100 = c(NA, head(rollsum(",
          stats,
          ", 100, fill = NA, align = 'right') / rollsum(minutes_played, 100, fill = NA, align = 'right'), -1))",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    leagueStats <-
      leagueStats[, !(names(leagueStats) %in% c("minutes_played", stats))]
    
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
          "_per_min_25 / league_",
          stats,
          "_per_min_100",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    opponentMultipliers <-
      opponentMultipliers[,!(names(opponentMultipliers) %in% c(
        paste0("opponent_", stats, "_per_min_25"),
        paste0("league_", stats, "_per_min_100")
      ))]
    
    # Join opponent analysis
    playerBoxScores <- join(playerBoxScores, opponentMultipliers)
    
    # Create rolling averages
    playerStats <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(sapply(c(1:5, 10, 20), function(x)
          paste0(
            "player_",
            stats,
            "_per_min_",
            x,
            " = c(NA, head(rollsum(",
            stats,
            ", ",
            x,
            ", fill = NA, align = 'right') / round(rollsum(minutes_played, ",
            x,
            ", fill = NA, align = 'right'), 5), -1)) * ",
            stats,
            "_multiplier",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Remove columns
    playerStats <-
      playerStats[, !(names(playerStats) %in% c(stats, paste0(stats, "_multiplier")))]
    
    # Create rolling averages
    playerStats <- eval(parse(
      text = paste0(
        "playerStats %>% group_by() %>% dplyr::mutate(",
        paste0(sapply(c(1:5, 10, 20), function(x)
          paste0(
            "player_fp_per_min_",
            x,
            " = colSums(apply(X = playerStats[, c(paste0('player_', stats, '_per_min_', ",
            x,
            "))], MARGIN = 1, FUN = function(x) x * c(1, 0.5, 1.25, 1.5, 2, 2, -0.5)))",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Remove columns
    playerStats <-
      playerStats[, !(names(playerStats) %in% sapply(c(1:5, 10, 20), function(x)
        paste0("player_", stats, "_per_min_", x)))]
    
    # Return player stats
    return(playerStats)
    
  }
  
)
