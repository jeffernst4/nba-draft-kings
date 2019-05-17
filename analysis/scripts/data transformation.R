
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
  PlayerSalaries = function(playerSalaries, gameType) {
    
    # Format na's
    playerSalaries$salary <- gsub("N/A", NA, playerSalaries$salary)
    
    # Format salaries to numeric
    playerSalaries$salary <-
      as.numeric(gsub('[$,]', '', playerSalaries$salary))
    
    # Rename column
    names(playerSalaries) <- gsub("salary", paste0("salary_", tolower(gameType)), names(playerSalaries))
    
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
