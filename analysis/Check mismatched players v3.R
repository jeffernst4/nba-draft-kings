write.csv(unique(data$PlayerBoxScores[data$PlayerBoxScores$season >= 2015, c("slug", "name")]),
          "analysis/data/player names.csv",
          row.names = FALSE)

playerHistory <- data$PlayerHistory

playerSalaries <- data$PlayerSalaries

playerSalaries <- join(playerSalaries, unique(playerHistory[, c("slug", "name", "team")]))

mismatchedSalaries <-
  aggregate(date ~ name + team, playerSalaries[is.na(playerSalaries$slug),], length, na.action = NULL)

playerBoxScores <- data$PlayerBoxScores

# playerBoxScores <- join(playerBoxScores[playerBoxScores$season >= 2015, ], playerSalaries)

salarySlugs <- aggregate(date ~ slug, playerSalaries, length, na.action = NULL)

names(salarySlugs)[2] <- "salary_matches"

boxScoreSlugs <- aggregate(date ~ slug, playerBoxScores, length, na.action = NULL)

names(boxScoreSlugs)[2] <- "all_matches"

test6 <- join(boxScoreSlugs, salarySlugs)

test6$prop <- test6$salary_matches / test6$all_matches

### maybe also look at salary matching % to find players

### Add missing records / dnp for player salaries

test <-
  join(playerSalaries, playerBoxScores[, c("slug", "name", "team", "date", "fantasy_points")])

### Maybe add in age to the formula
