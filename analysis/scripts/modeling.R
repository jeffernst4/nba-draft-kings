
Modeling <- list(
  
  OLS = function(data, outcome, predictors) {
    
    # Create ols model
    model <- lm(formula = as.formula(paste0(
      outcome, " ~ ", paste(predictors, collapse = " + ")
    )), data = data[, c(outcome, predictors)])
    
    # Return model
    return(model)
    
  },
  
  RandomForest = function(data, outcome, predictors, sample, ntree) {
    
    # Create random forest model
    model <- randomForest(as.formula(paste0(
      outcome, " ~ ", paste(predictors, collapse = " + ")
    )),
    data = data[sample(nrow(data), sample), c(outcome, predictors)],
    ntree = ntree,
    importance = TRUE)
    
    # Return model
    return(model)
    
  }
  
)
