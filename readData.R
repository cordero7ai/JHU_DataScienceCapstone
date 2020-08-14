## All functions needed to read and clean the data.


# Returns all lines in the file:
readFullFile <- function(inFile)
{
  # con <- file(file.choose(), "r")
  # setwd()
  # list.files()
  
  connection <- file(inFile, "r")
  totalData <- readLines(connection, encoding = "UTF-8", skipNul = TRUE)
  # with readLines(connection, n =1) you can read line by line avoiding to store all data in memory!
  close(connection)
  
  return(totalData)
}

readTotalNumberOfLinesInFile <- function(inFile)
{
  connection <- file(inFile, "rb")
  
  # Straightforward but inneficient way, you loaded all data into memory unnecessarily. 
  # You could check line by line with readlines n =1. or a better way is in chunks.
  # totalLines = 0
  # totalData <- readLines(connection, encoding = "UTF-8", skipNul = TRUE)
  # totalLines <- summary(nchar(totalData))[6]
  
  # Read lines in chunks
  totalLines <- 0L
  while (length(chunk <- readBin(connection, "raw", 65536)) > 0) 
  {
    totalLines <- totalLines + sum(chunk == as.raw(10L))
  }
  
  close(connection)
  
  return(totalLines)  
}


