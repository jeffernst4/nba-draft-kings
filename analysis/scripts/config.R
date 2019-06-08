
config <- list(
  
  SalaryTypes = c("DraftKings"),
  
  Stats = c(
    "points",
    "three_pointers_made",
    "rebounds",
    "assists",
    "steals",
    "blocks",
    "turnovers"
  ),
  
  StatsNew = c(
    "two_pointers_made",
    "three_pointers_made",
    "rebounds",
    "assists",
    "steals",
    "blocks",
    "turnovers"
  ),
  
  StatsMultipliers = c(1, 0.5, 1.25, 1.5, 2, 2,-0.5),
  
  FeaturePeriods = list(
    
    MinutesPlayed = c(1, 3, 5, 10, 20),
    
    Salary = c(1, 3, 5, 10, 20),
    
    FantasyPointsPerMin = c(1, 3, 5, 10)
    
  )
  

  
)
