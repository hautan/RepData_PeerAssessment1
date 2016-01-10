unZipFile <- function(zipFile = "activity.zip", dataFolder = "data"){
  #create data folder path if it doesn't exist
  if (!file.exists(dataFolder)) {
    dir.create(dataFolder)
  }
  
  writeLines(paste("uziping data to folder... ", dataFolder))
  unzip("activity.zip", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = dataFolder, unzip = "internal", setTimes = FALSE)

}
