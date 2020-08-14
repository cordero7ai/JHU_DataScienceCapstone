

# Download data from the web for the project.
# To use this function in another scripting file use:
# source("downloadFiles.R")
# downloadFinalProjectFile()

downloadFinalProjectFile <- function(){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  destinationPath <- "./data/Coursera-SwiftKey.zip"
  
  if(!file.exists("./data")){
      dir.create("./data")
    }
  if(!file.exists("./data/Coursera-SwiftKey.zip")){
    download.file(fileUrl, destfile = destinationPath, method = "curl")  #for binary files.
  }
  else
  {
    print("Source files has already been downloaded! will place the new one in the backup folder in data.")
    dir.create("./data/backup")
    download.file(fileUrl, destfile = "./data/backup/Coursera-SwiftKey.zip", method = "curl")
  }  
  dateDownloaded <- date()
  unzip( destinationPath, exdir="./data", list =TRUE)                
}
