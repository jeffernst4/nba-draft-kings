playerAnalysis <- na.omit(data$PlayerAnalysis[, c(
  "name",
  "date",
  "season",
  "location",
  "fantasy_points",
  "player_fp_1",
  "player_fp_2",
  "player_fp_3",
  "player_fp_4",
  "player_fp_5",
  "player_fp_6",
  "player_fp_7",
  "player_fp_8",
  "player_fp_9",
  "player_fp_10",
  "player_fp_15",
  "player_fp_20"
)])

playerAnalysis$predictions <- predict(model$OLS)
playerAnalysis$residuals <- playerAnalysis$predictions - playerAnalysis$fantasy_points

plotParameters <- list(
  PlayerName = "DeMarcus Cousins",
  Season = 2016:2019,
  Location = "AWAY"
)


plot(playerAnalysis$date[playerAnalysis$name == plotParameters$PlayerName & playerAnalysis$location %in% plotParameters$Location], playerAnalysis$fantasy_points[playerAnalysis$name == plotParameters$PlayerName & playerAnalysis$location %in% plotParameters$Location], ylim = c(0, 80))
lines(playerAnalysis$date[playerAnalysis$name == plotParameters$PlayerName & playerAnalysis$location %in% plotParameters$Location], playerAnalysis$predictions[playerAnalysis$name == plotParameters$PlayerName & playerAnalysis$location %in% plotParameters$Location])

plot(playerAnalysis$date[playerAnalysis$name == plotParameters$PlayerName &
                           playerAnalysis$season %in% plotParameters$Season &
                           playerAnalysis$location %in% plotParameters$Location], playerAnalysis$residuals[playerAnalysis$name == plotParameters$PlayerName &
                                                                                                             playerAnalysis$season %in% plotParameters$Season &
                                                                                                             playerAnalysis$location %in% plotParameters$Location])
