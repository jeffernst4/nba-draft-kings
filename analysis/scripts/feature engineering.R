
FeatureEngineering <- list(
  
  # Calculate player salaries
  SalaryChange = function(playerBoxScores, config) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("slug", "date", paste0("salary_", tolower(config$SalaryTypes)))]
    
    # Create rolling averages
    salaryChange <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(
          sapply(config$FeaturePeriods$Salary, function(x)
            paste0(
              "salary_",
              tolower(config$SalaryTypes),
              "_change_",
              x,
              " = round(salary_",
              tolower(config$SalaryTypes),
              " / c(NA, head(rollapply(salary_",
              tolower(config$SalaryTypes),
              ", ",
              x + 10,
              ", FUN = function(x) mean(tail(na.omit(x), n = ",
              x,
              ")), fill = NA, partial = TRUE, align = 'right'), -1)) -1, 5)",
              collapse = ", "
            )),
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    salaryChange <-
      salaryChange[, !(names(salaryChange) %in% paste0("salary_", tolower(config$SalaryTypes)))]
    
    # Return player box scores
    return(salaryChange)
    
  },
  
  # Calculate minutes played
  MinutesPlayed = function(playerBoxScores, config) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c("slug", "date", "minutes_played")]
    
    # Create rolling averages
    minutesPlayed <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(sapply(config$FeaturePeriods$MinutesPlayed, function(x)
          paste0(
            "minutes_played_",
            x,
            " = c(NA, head(rollmean(minutes_played, ",
            x,
            ", fill = NA, align = 'right'), -1))",
            collapse = ", "
          )), collapse = ", "),
        ")"
      )
    ))
    
    # Remove columns
    minutesPlayed <-
      minutesPlayed[, !(names(minutesPlayed) %in% "minutes_played")]
    
    # Return player stats
    return(minutesPlayed)
    
  },
  
  # Calculate player fantasy points
  Fantasy_Points = function(playerBoxScores, teamStats) {
    
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
  FantasyPointsPerMin = function(playerBoxScores, teamBoxScores, config) {
    
    ### remove stats per min for player box scores and replace with just stats
    ### get fp_per_min_1:5,10,15,20 by using rollsum find a way to do last 20,
    ### 40, 80, 120 minutes, otherwise, just use more general games to replace
    ### more short-term games maybe sum up minutes for each date for each player
    ### and pick out minutes that are at each level, I think the first option is
    ### better
    
    ### analysis$fp_10[is.nan(analysis$fp_10)] <- fp_20[is.nan(analysis$fp_10)]
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c(
        "slug",
        "date",
        "opponent",
        "minutes_played",
        config$Stats
      )]
    
    # Create stats analysis
    statsAnalysis <- teamBoxScores[, c("opponent", "date")]
    
    # Filter team box scores
    teamBoxScores <-
      teamBoxScores[, c("opponent", "date", "minutes_played", config$Stats)]
    
    # Create opponent stats
    opponentStats <- eval(parse(
      text = paste0(
        "teamBoxScores %>% group_by(opponent) %>% dplyr::mutate(",
        paste0(
          "opponent_",
          config$Stats,
          "_per_min_25 = c(NA, head(rollsum(",
          config$Stats,
          ", 25, fill = NA, align = 'right') / rollsum(minutes_played, 25, fill = NA, align = 'right'), -1))",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    opponentStats <-
      opponentStats[, !(names(opponentStats) %in% c("minutes_played", config$Stats))]
    
    # Join opponent stats
    statsAnalysis <- join(statsAnalysis, opponentStats)
    
    # Create league stats
    leagueStats <- eval(parse(
      text = paste0(
        "teamBoxScores %>% group_by() %>% dplyr::mutate(",
        paste0(
          "league_",
          config$Stats,
          "_per_min_100 = c(NA, head(rollsum(",
          config$Stats,
          ", 100, fill = NA, align = 'right') / rollsum(minutes_played, 100, fill = NA, align = 'right'), -1))",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    leagueStats <-
      leagueStats[, !(names(leagueStats) %in% c("minutes_played", config$Stats))]
    
    # Join league stats
    statsAnalysis <- join(statsAnalysis, leagueStats)
    
    # Calculate opponent multipliers
    opponentMultipliers <- eval(parse(
      text = paste0(
        "statsAnalysis %>% group_by() %>% dplyr::mutate(",
        paste0(
          config$Stats,
          "_multiplier = opponent_",
          config$Stats,
          "_per_min_25 / league_",
          config$Stats,
          "_per_min_100",
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    opponentMultipliers <-
      opponentMultipliers[,!(names(opponentMultipliers) %in% c(
        paste0("opponent_", config$Stats, "_per_min_25"),
        paste0("league_", config$Stats, "_per_min_100")
      ))]
    
    # Join opponent analysis
    playerBoxScores <- join(playerBoxScores, opponentMultipliers)
    
    # Replace minutes played residual with NA when players didn't play
    playerBoxScores[playerBoxScores$minutes_played == 0, c("minutes_played", config$Stats)] <-
      NA
    
    # Create rolling averages
    playerStats <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(
          sapply(config$FeaturePeriods$FantasyPointsPerMin, function(x)
            paste0(
              "player_",
              config$Stats,
              "_per_min_",
              x,
              " = c(NA, head(rollapply(",
              config$Stats,
              ", ",
              x + 10,
              ", FUN = function(x) mean(tail(na.omit(x), n = ",
              x,
              ")), fill = NA, partial = TRUE, align = 'right') / rollapply(minutes_played, ",
              x + 10,
              ", FUN = function(x) mean(tail(na.omit(x), n = ",
              x,
              ")), fill = NA, partial = TRUE, align = 'right'), -1)) * ",
              config$Stats,
              "_multiplier",
              collapse = ", "
            )),
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    playerStats <-
      playerStats[, !(names(playerStats) %in% c("minutes_played", config$Stats, paste0(config$Stats, "_multiplier")))]
    
    # Create rolling averages
    playerStats <- eval(parse(
      text = paste0(
        "playerStats %>% group_by() %>% dplyr::mutate(",
        paste0(
          sapply(config$FeaturePeriods$FantasyPointsPerMin, function(x)
            paste0(
              "fantasy_points_per_min_",
              x,
              " = colSums(apply(X = playerStats[, c(paste0('player_', config$Stats, '_per_min_', ",
              x,
              "))], MARGIN = 1, FUN = function(x) x * c(1, 0.5, 1.25, 1.5, 2, 2, -0.5)))",
              collapse = ", "
            )),
          collapse = ", "
        ),
        ")"
      )
    ))
    
    # Remove columns
    playerStats <-
      playerStats[,!(
        names(playerStats) %in% sapply(config$FeaturePeriods$FantasyPointsPerMin, function(x)
          paste0("player_", config$Stats, "_per_min_", x))
      )]
    
    # Return player stats
    return(playerStats)
    
  },
  
  # Calculate player fantasy points
  StatsPerMin = function(playerBoxScores, teamBoxScores, config) {
    
    # Filter player box scores
    playerBoxScores <-
      playerBoxScores[, c(
        "slug",
        "date",
        "opponent",
        "minutes_played",
        config$StatsNew
      )]
    
    # Replace minutes played residual with NA when players didn't play
    playerBoxScores[playerBoxScores$minutes_played < 6, c("minutes_played", config$StatsNew)] <-
      NA
    
    # Create rolling averages
    statsPerMin <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(mapply(
          function(windowWidth,
                   minWindowWidth,
                   maxWindowWidth)
            paste0(
              config$StatsNew,
              "_per_min_",
              windowWidth,
              " = c(NA, head(rollapply(",
              config$StatsNew,
              ", ",
              maxWindowWidth,
              ", FUN = function(x) sum(tail(na.omit(x)[length(na.omit(x)) >= ",
              minWindowWidth,
              "], n = ",
              windowWidth,
              ")), fill = NA, partial = TRUE, align = 'right') / rollapply(minutes_played, ",
              maxWindowWidth,
              ", FUN = function(x) sum(tail(na.omit(x)[length(na.omit(x)) >= ",
              minWindowWidth,
              "], n = ",
              windowWidth,
              ")), fill = NA, partial = TRUE, align = 'right'), -1))",
              collapse = ", "
            ),
          10,
          7,
          50
        ),
        collapse = ", "),
        ")"
      )
    ))
    
    # Remove columns
    statsPerMin <-
      statsPerMin[, !(names(statsPerMin) %in% c("minutes_played", config$StatsNew))]
    
    # Return stats per min
    return(statsPerMin)
    
  },
  
  # Calculate minutes played standard deviation
  MinutesPlayedStdDev = function(analysis) {
    
    # Filter player box scores
    analysis <-
      analysis[, c("slug",
                   "date",
                   "played_game",
                   "minutes_played",
                   "minutes_played_prediction")]
    
    # Calculate minutes played residual
    analysis$minutes_played_residual <-
      analysis$minutes_played_prediction - analysis$minutes_played
    
    # Replace minutes played residual with NA when players didn't play
    analysis[analysis$minutes_played == 0, c("minutes_played", "minutes_played_residual")] <-
      NA
    
    # Create rolling averages
    minutesPlayedStdDev <-
      analysis %>% group_by(slug) %>% dplyr::mutate(
        minutes_played_std_dev = c(NA, head(
          rollapply(
            minutes_played,
            50,
            FUN = function(x)
              sd(x[length(na.omit(x)) >= 7], na.rm = TRUE),
            partial = TRUE,
            align = 'right'
          ),-1
        )),
        minutes_played_residual_std_dev = c(NA, head(
          rollapply(
            minutes_played_residual,
            50,
            FUN = function(x)
              sd(x[length(na.omit(x)) >= 7], na.rm = TRUE),
            partial = TRUE,
            align = 'right'
          ),-1
        ))
      )
    
    # Combine minutes played residual with minutes played standard deviation
    minutesPlayedStdDev <-
      minutesPlayedStdDev %>% mutate(
        minutes_played_std_dev = coalesce(minutes_played_residual_std_dev, minutes_played_std_dev)
      )
    
    # Remove columns
    minutesPlayedStdDev <-
      minutesPlayedStdDev[,!(
        names(minutesPlayedStdDev) %in% c(
          "played_game",
          "minutes_played",
          "minutes_played_prediction",
          "minutes_played_residual",
          "minutes_played_residual_std_dev"
        ))]
    
    # Return player stats
    return(minutesPlayedStdDev)
    
  },
  
  # Calculate fantasy points per min standard deviation
  FantasyPointsPerMinStdDev = function(analysis) {
    
    # Filter player box scores
    analysis <-
      analysis[, c("slug",
                   "date",
                   "played_game",
                   "fantasy_points_per_min",
                   "fantasy_points_per_min_prediction")]
    
    # Calculate fantasy points per min residual
    analysis$fantasy_points_per_min_residual <-
      analysis$fantasy_points_per_min_prediction - analysis$fantasy_points_per_min
    
    # Replace fantasy points per min residual with NA when players didn't play
    analysis$fantasy_points_per_min_residual[analysis$played_game == "0"] <-
      NA
    
    # Replace fantasy points per min with NA when players don't play
    analysis$fantasy_points_per_min[analysis$played_game == "0"] <-
      NA
    
    # Create rolling averages
    fantasyPointsPerMinStdDev <-
      analysis %>% group_by(slug) %>% dplyr::mutate(
        fantasy_points_per_min_std_dev = c(NA, head(
          rollapply(
            fantasy_points_per_min,
            50,
            FUN = function(x)
              sd(x[length(na.omit(x)) >= 7], na.rm = TRUE),
            partial = TRUE,
            align = 'right'
          ),-1
        )),
        fantasy_points_per_min_residual_std_dev = c(NA, head(
          rollapply(
            fantasy_points_per_min_residual,
            50,
            FUN = function(x)
              sd(x[length(na.omit(x)) >= 7], na.rm = TRUE),
            partial = TRUE,
            align = 'right'
          ),-1
        ))
      )
    
    # Combine fantasy points per min residual with fantasy points per min standard deviation
    fantasyPointsPerMinStdDev <-
      fantasyPointsPerMinStdDev %>% mutate(
        fantasy_points_per_min_std_dev = coalesce(fantasy_points_per_min_residual_std_dev, fantasy_points_per_min_std_dev)
      )
    
    # Remove columns
    fantasyPointsPerMinStdDev <-
      fantasyPointsPerMinStdDev[,!(
        names(fantasyPointsPerMinStdDev) %in% c(
          "played_game",
          "fantasy_points_per_min",
          "fantasy_points_per_min_prediction",
          "fantasy_points_per_min_residual",
          "fantasy_points_per_min_residual_std_dev"
        ))]
    
    # Return fantasy points per min standard deviation
    return(fantasyPointsPerMinStdDev)
    
  },
  
  # Calculate stat per minute standard deviation
  StandardDeviation = function(analysis, stat, minutesPlayedMinimum) {
    
    # Filter player box scores
    analysis <-
      analysis[, c("slug",
                   "date",
                   "minutes_played",
                   paste0(stat, "_per_min"),
                   paste0(stat, "_per_min_prediction"))]
    
    # Calculate stat per minute residual
    analysis[[paste0(stat, "_per_min_residual")]] <-
      analysis[[paste0(stat, "_per_min_prediction")]] - analysis[[paste0(stat, "_per_min")]]
    
    # Replace stat per minute residual with NA when players didn't play
    analysis[analysis$minutes_played < minutesPlayedMinimum, paste0(stat, c("_per_min", "_per_min_residual"))] <-
      NA
    
    # Create rolling averages
    standardDeviation <- eval(parse(text = mapply(
      function(minWindowWidth, maxWindowWidth)
        paste0(
          "analysis %>% group_by(slug) %>% dplyr::mutate(",
          stat,
          "_per_min_std_dev = c(NA, head(rollapply(",
          stat,
          "_per_min, ",
          maxWindowWidth,
          ", FUN = function(x) sd(x[length(na.omit(x)) >= ",
          minWindowWidth,
          "], na.rm = TRUE), partial = TRUE, align = 'right'), -1)),",
          stat,
          "_per_min_residual_std_dev = c(NA, head(rollapply(",
          stat,
          "_per_min_residual, ",
          maxWindowWidth,
          ", FUN = function(x) sd(x[length(na.omit(x)) >= ",
          minWindowWidth,
          "], na.rm = TRUE), partial = TRUE, align = 'right'), -1)))"
        ),
      7,
      50
    )))
    
    # Combine stat per minute residual with stat per minute standard deviation
    standardDeviation <- eval(parse(
      text = paste0(
        "standardDeviation %>% mutate(",
        stat,
        "_per_min_std_dev = coalesce(",
        stat,
        "_per_min_residual_std_dev, ",
        stat,
        "_per_min_std_dev)
      )"
      )
    ))
    
    # Remove columns
    standardDeviation <-
      standardDeviation[, !(names(standardDeviation) %in% c("minutes_played",
                                                            paste0(
                                                              stat,
                                                              c(
                                                                "_per_min",
                                                                "_per_min_prediction",
                                                                "_per_min_residual",
                                                                "_per_min_residual_std_dev"
                                                              )
                                                            )))]
    
    # Return stat per minute standard deviation
    return(standardDeviation)
    
  }
  
)
