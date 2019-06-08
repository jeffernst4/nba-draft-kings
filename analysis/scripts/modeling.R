
Modeling <- list(
  
  OLS = function(data, outcome) {
    
    # Filter data
    data <- na.omit(data)
    
    # Create ols model
    model <- lm(formula = as.formula(paste0(outcome, " ~ .")), data = data)
    
    # Return model
    return(model)
    
  },
  
  RandomForest = function(data, outcome, sampleSize = 5000, ntree = 100, train = TRUE) {
    
    # Filter data
    data <- na.omit(data)
    
    # Create training sample
    trainSample <- sample(nrow(data), sampleSize)
      
    # Create test sample
    testSample <- which(!(1:nrow(data) %in% trainSample))
    
    if (train) {
      
      # Create random forest model
      model <- randomForest(
        data = data[trainSample, ],
        as.formula(paste0(outcome, " ~ .")),
        xtest = data[testSample, !(names(data) %in% outcome), drop = FALSE],
        ytest = data[testSample, outcome],
        ntree = ntree,
        importance = TRUE
      )
      
    } else {
      
      # Create random forest model
      model <- randomForest(
        data = data[trainSample, ],
        as.formula(paste0(outcome, " ~ .")),
        ntree = ntree,
        importance = TRUE
      )
      
    }
    
    # Return model
    return(list(
      Model = model,
      TrainData = data[trainSample,],
      TestData = data[testSample,]
    ))
    
  }
  
)
