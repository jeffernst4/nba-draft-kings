
dataTransformation <- list(
  
  # Transform player history
  PlayerHistories = function(playerHistories) {
    
    # Return player history
    return(playerHistories)
    
  },
  
  # Transform player salaries
  PlayerSalaries = function(playerSalaries, gameType, nameCorrections) {
    
    # Format salaries to numeric
    playerSalaries$salary <-
      as.numeric(gsub('[$,]', '', playerSalaries$salary))
    
    # Rename column
    names(playerSalaries) <- gsub("salary", paste0("salary_", tolower(gameType)), names(playerSalaries))
    
    # Format names
    playerSalaries$name <-
      gsub("\\^", "", sub("(.+),\\s(.+)", "\\2 \\1", playerSalaries$name))
    
    # Join name corrections with player salaries
    playerSalaries <- join(playerSalaries, nameCorrections, type = "left")
    
    # Replace names with corrected names
    playerSalaries$name[!is.na(playerSalaries$corrected_name)] <-
      playerSalaries$corrected_name[!is.na(playerSalaries$corrected_name)]
    
    # Remove column
    playerSalaries <- subset(playerSalaries, select = -corrected_name)
    
    # Filter player salaries
    playerSalaries <-
      playerSalaries[, c("date", "name", "team", paste0("salary_", tolower(gameType)))]
    
    # Remove all-star games
    playerSalaries <-
      playerSalaries[playerSalaries$team != "WEST", ]
    
    # Remove na's
    playerSalaries <- na.exclude(playerSalaries)
    
    # Return player salaries
    return(playerSalaries)
    
  },
  
  # Transform player box scores
  PlayerBoxScores = function(playerBoxScores, config) {
    
    # Identify stats columns
    statsColumnNames <- c(
      "seconds_played",
      "made_field_goals",
      "attempted_field_goals",
      "made_three_point_field_goals",
      "attempted_three_point_field_goals",
      "made_free_throws",
      "attempted_free_throws",
      "offensive_rebounds",
      "defensive_rebounds",
      "assists",
      "steals",
      "blocks",
      "turnovers",
      "personal_fouls",
      "game_score"
    )
    
    # Replace na's
    playerBoxScores[statsColumnNames][is.na(playerBoxScores[statsColumnNames])] <-
      0
    
    # Rename columns
    names(playerBoxScores) <-
      gsub("made_field_goals",
           "field_goals_made",
           names(playerBoxScores))
    names(playerBoxScores) <-
      gsub("attempted_field_goals",
           "field_goals_attempted",
           names(playerBoxScores))
    names(playerBoxScores) <-
      gsub("made_three_point_field_goals",
           "three_pointers_made",
           names(playerBoxScores))
    names(playerBoxScores) <-
      gsub(
        "attempted_three_point_field_goals",
        "three_pointers_attempted",
        names(playerBoxScores)
      )
    names(playerBoxScores) <-
      gsub("made_free_throws",
           "free_throws_made",
           names(playerBoxScores))
    names(playerBoxScores) <-
      gsub("attempted_free_throws",
           "free_throws_attempted",
           names(playerBoxScores))
    
    # Convert seconds to minutes
    playerBoxScores$seconds_played <-
      playerBoxScores$seconds_played / 60
    
    # Rename column
    names(playerBoxScores) <-
      gsub("seconds_played", "minutes_played", names(playerBoxScores))
    
    # Identify players who played significant minutes
    playerBoxScores$played_game <-
      factor(ifelse(playerBoxScores$minutes_played > 0, 1, 0)) 
    
    # Calculate made two points field goals
    playerBoxScores$two_pointers_made <-
      playerBoxScores$field_goals_made - playerBoxScores$three_pointers_made
    
    # Calculate attempted two point field goals
    playerBoxScores$two_pointers_attempted <-
      playerBoxScores$field_goals_attempted - playerBoxScores$three_pointers_attempted
    
    # Calculate rebounds
    playerBoxScores$rebounds <-
      playerBoxScores$offensive_rebounds + playerBoxScores$defensive_rebounds
    
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
    
    # Identify triple-doubles
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
    
    # # Calculate fantasy points per minute
    # playerBoxScores$fantasy_points_per_min <-
    #   ifelse(
    #     playerBoxScores$minutes_played > 0,
    #     playerBoxScores$fantasy_points / playerBoxScores$minutes_played,
    #     NA
    #   )
    
    # Create stats per minute
    playerBoxScores <- eval(parse(
      text = paste0(
        "playerBoxScores %>% group_by(slug) %>% dplyr::mutate(",
        paste0(
          c(config$StatsNew, "fantasy_points"),
          "_per_min = ifelse(minutes_played > 0, ",
          c(config$StatsNew, "fantasy_points"),
          " / minutes_played, NA)",
          collapse = ","
        ),
        ") %>% ungroup()"
      )
    ))
    
    # Sort player box scores
    playerBoxScores <-
      playerBoxScores[with(playerBoxScores, order(date, slug)),]
    
    # Convert player box scores to data frame
    playerBoxScores <- as.data.frame(playerBoxScores)
    
    # Return player box scores
    return(playerBoxScores)
    
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
    
    # Calculate rebounds
    teamBoxScores$rebounds <-
      teamBoxScores$offensive_rebounds + teamBoxScores$defensive_rebounds
    
    
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
          minutes_played,
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
  
  # TeamRosters = function(teamRosters) {
  #   
  #   # Fix names
  #   teamRosters$name <- gsub("[^[:alnum:]]+\\(TW\\)", "", teamRosters$name)
  #   
  #   # Return team rosters
  #   return(teamRosters)
  #   
  # }
  
)
