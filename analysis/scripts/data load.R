
dataLoad <- list(
  
  GameStats = function(fileType) {
    
    # Create empty data list
    dataList <- list()
    
    # Create file list
    fileList <-
      list.files(path = paste0("data/", fileType, "/"),
                 pattern = "*.csv")
    
    # Create progress bar
    progressBar <- tkProgressBar("Loading Data", "Loading...",
                                 0, 1, 0)
    
    # Load data to a list of data frames
    for(i in 1:length(fileList)) {
      
      if (fileType %in% c("player box scores", "team box scores")) {
        
        # Extract date
        fileDate <-
          as.Date(str_extract(fileList[i], "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
                  format = "%Y-%m-%d")
        
      } else if (fileType %in% c("player season totals", "season schedules")) {
        
        # Extract date
        fileDate <- str_extract(fileList[i], "[0-9]{4}")
        
      }
      
      # Read data
      data <-
        read.csv(
          paste0(
            "data/",
            fileType,
            "/",
            fileList[i]
          ),
          stringsAsFactors = FALSE,
          encoding = "utf-8"
        )
      
      # Check if data has more than one row
      if (nrow(data) > 0) {
        
        # Add date column to data
        data$date <- fileDate
        
        # Add data to list
        dataList <- c(dataList, list(data))
        
      }
      
      # Update progress bar
      setTkProgressBar(progressBar,
                       i / length(fileList),
                       paste0("Loading ", fileType),
                       sprintf("%d%% done", round(i * 100 / length(fileList))))
      
    }
    
    # Close progress bar
    close(progressBar)
    
    # Combine list into a single data frame
    data <- do.call("rbind", dataList)
    
    # Return data
    return(data)
    
  },
  
  PlayerSalaries = function(fileType, gameType) {
    
    # Create empty data list
    dataList <- list()
    
    # Create file list
    fileList <-
      list.files(path = paste0("data/", fileType, "/", gameType, "/"),
                 pattern = "*.csv")
    
    # Create progress bar
    progressBar <- tkProgressBar("Loading Data", "Loading...",
                                 0, 1, 0)
    
    # Load data to a list of data frames
    for(i in 1:length(fileList)) {
      
      # Extract date
      fileDate <-
        as.Date(str_extract(fileList[i], "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
                format = "%Y-%m-%d")
      
      # Read data
      data <-
        read.csv(
          paste0("data/",
                 fileType,
                 "/",
                 gameType,
                 "/",
                 fileList[i]),
          stringsAsFactors = FALSE,
          encoding = "utf-8"
        )
      
      # Check if data has more than one row
      if (nrow(data) > 0) {
        
        # Add date column to data
        data$date <- fileDate
        
        # Add data to list
        dataList <- c(dataList, list(data))
        
      }
      
      # Update progress bar
      setTkProgressBar(progressBar,
                       i / length(fileList),
                       paste0("Loading ", gameType, " ", fileType),
                       sprintf("%d%% done", round(i * 100 / length(fileList))))
      
    }
    
    # Close progress bar
    close(progressBar)
    
    # Combine list into a single data frame
    data <- do.call("rbind", dataList)
    
    # Return data
    return(data)
    
  }
  
)
