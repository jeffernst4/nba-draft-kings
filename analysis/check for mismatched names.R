
# Find names that aren't matching
playerBoxScores <- data$PlayerBoxScores

# 3481

players <-
  unique(playerBoxScores[, c(
    "date",
    "season",
    "name",
    "team",
    "salary_draftkings",
    "salary_fanduel",
    "salary_yahoo"
  )])

players<-
  players[(is.na(players$salary_draftkings) |
            is.na(players$salary_fanduel) |
            is.na(players$salary_yahoo)) & players$season == 2019, ]

# 3517 entries in players, 2529, 1879

test <- aggregate(date ~ name + team, players, length)

##

dksalaries <- data$PlayerSalaries$DraftKings

write.csv(test, "wrongnames.csv", row.names = FALSE)
