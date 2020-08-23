## Display Statistics for input Data
displaySummaryOfTextVectorInConsole <- function(textData, sampleSizePercentage, elapsed_time){
  print(paste("*********Summary for Vector:", deparse(substitute(textData)),"**************"))
  print(paste("*********Using:", as.character(sampleSizePercentage * 100), "% of the Original Data"))
  print("")
  print(paste("**vector type:", class(textData)))
  print(paste("**size in MegaBytes:", sprintf("%.5f", object.size(textData)/(1024*1024))))
  print(paste("**Time for Loading and Sampling the Data in Seconds:", elapsed_time))
  print(paste("**Number Of Lines:", length(textData)))
  
  nchars <- lapply(textData, nchar)
  maxchars <- which.max(nchars)
  maxchars2 <- max(unlist(nchars))
  wordCount <- sum(sapply(strsplit(textData, "\\s+"), length))
  
  print(paste("**Line with the Largest Number of Characters:", maxchars))
  print(paste("**Maximum Number of Characters in a Line:", maxchars2))
  print(paste("**Total Number of Words:", wordCount))
  #print(textData)
  print("**************************************************************")
  print("")
}

## TokenizeCleans the vector of characters
tokenizeAndcleanCorpus <- function(textData){
  theTokens<- tokens(textData, what ="word", 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_separators = TRUE, 
                     remove_symbols = TRUE )
  
  theTokens <- tokens_tolower(theTokens)
    #print( ntoken(theTokens) )
  theTokens <- tokens_select(theTokens, stopwords(), selection ="remove") 
    #print( ntoken(theTokens) )
  source("badwords.R")
  theTokens <- tokens_select(theTokens, badwords, selection ="remove") 
    #print( ntoken(theTokens) )
  
  return(theTokens)
}

getElapsedTimeMessage <- function(message, st, et){
  print("**************************************************************")
  print(paste("Elapsed Time in Seconds for:", message, difftime(et, st, unit = "secs") ))
  print("**************************************************************")
  print("*")
}

## Converts numeric named vector into customized n-gram dataframe
convertNNVtoDF <- function(ngramNumVector, n){
  
  df <- stack(ngramNumVector)
  df <- setNames(df[2:1], c('Words','Frequency'))
  df$Words <- as.character(df$Words)
  
  if(n == 2){
    df$Prediction <- gsub("^.*_", "", df$Words)
    df$Predictor <- gsub("_.*", "", df$Words)
  }
  else if(n == 3){
    df$Prediction <- gsub("^.*_", "", df$Words)
    df$Predictor2Gram <- gsub("_([^_]*)$", "", df$Words)
    df$Predictor1Gram <- gsub("^.*_", "", df$Predictor2Gram)
  }  
  else if(n == 4){
    df$Prediction <- gsub("^.*_", "", df$Words)
    df$Predictor3Gram <- gsub("_([^_]*)$", "", df$Words)
    df$Predictor2Gram <- gsub("^.+?(_)", "", df$Predictor3Gram)
    df$Predictor1Gram <- gsub("^.*_", "", df$Predictor2Gram)
  }
  print(head(df))
  
  return(df)
}

## ONLY COUNTS
## Builds a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
  #returns a list of dataframe words from 3-previous_words, 2-previous_words, 1-previous_word.
    #the dataframe contains at most the top 3 most frequent words used after the n-1 n-gram from OFFLINE Data.
      #If the input word(s) do(es) not match any previous n-1 n-gram, then the suggested element word will be @@@
predictNextWordWithObservedNGrams <- function(enteredWords, bigramsDf, trigramsDf, tetragramsDf){
  
  nextWordList <- vector(mode = "list", length = 3)
  
  if(nchar(enteredWords) > 0 ){
    
    ## 1.Format the entered word into homogeneous tokens 
    enteredWords <- gsub("_", "", enteredWords)
    queryTokens <- tokenizeAndcleanCorpus(enteredWords)
    
    ## 2. Trim the entered word(s). 
      #We will predict words by taking at most trigrams. If longer sentences are entered then we start building up n-grams
      #from the end of the sentence and then expanding to previous (n-2) adjacent tokens.
      #We divide the entered words in (n-k)grams from the nth token to the first. 
      #Example: "Apple wins". unigram: "wins", bigram: "apple-wins"
    
    queryLength <- ntoken(queryTokens)[[1]][1]
      
    #Go for initital trigrams:
    if( queryLength >= 3){
      predictorNGram <- paste(tail(queryTokens[[1]], 3), collapse = "_")
      resultSet <- sqldf(sprintf("SELECT Prediction 
                                  FROM tetragramsDf
                                  WHERE Predictor3Gram = '%s' 
                                  ORDER BY Frequency DESC
                                  LIMIT 3"
                                 , predictorNGram)
                        )
      if(nrow(resultSet) == 0){ nextWordList[[1]] <- "@@@"  }
      else{ nextWordList[[1]] <- as.list(resultSet$Prediction) }
      
      #Go for bigrams
      predictorNGram <- paste(tail(queryTokens[[1]], 2), collapse = "_")
      resultSet <- sqldf(sprintf("SELECT Prediction 
                                  FROM trigramsDf
                                  WHERE Predictor2Gram = '%s' 
                                  ORDER BY Frequency DESC
                                  LIMIT 3"
                                 , predictorNGram)
      )
      if(nrow(resultSet) == 0){ nextWordList[[2]] <- "@@@"  }
      else{ nextWordList[[2]] <- as.list(resultSet$Prediction) }
      
      #Go for unigrams
      predictorNGram <- paste(tail(queryTokens[[1]], 1))
      resultSet <- sqldf(sprintf("SELECT Prediction 
                                  FROM bigramsDf
                                  WHERE Predictor = '%s' 
                                  ORDER BY Frequency DESC
                                  LIMIT 3"
                                 , predictorNGram)
      ) 
      if(nrow(resultSet) == 0){ nextWordList[[3]] <- "@@@"  }
      else{ nextWordList[[3]] <- as.list(resultSet$Prediction) }
    }
    #Go for initial bigrams:
    else if( queryLength == 2){
      nextWordList[[1]] <- "@@@"
      predictorNGram <- paste(tail(queryTokens[[1]], 2), collapse = "_")
      resultSet <- sqldf(sprintf("SELECT Prediction 
                                  FROM trigramsDf
                                  WHERE Predictor2Gram = '%s' 
                                  ORDER BY Frequency DESC
                                  LIMIT 3"
                                 , predictorNGram)
      )
      if(nrow(resultSet) == 0){ nextWordList[[2]] <- "@@@"  }
      else{ nextWordList[[2]] <- as.list(resultSet$Prediction) }
      
      #Go for unigrams
      predictorNGram <- paste(tail(queryTokens[[1]], 1))
      resultSet <- sqldf(sprintf("SELECT Prediction 
                                  FROM bigramsDf
                                  WHERE Predictor = '%s' 
                                  ORDER BY Frequency DESC
                                  LIMIT 3"
                                 , predictorNGram)
      )
      if(nrow(resultSet) == 0){ nextWordList[[3]] <- "@@@"  }
      else{ nextWordList[[3]] <- as.list(resultSet$Prediction) }
    }
    #Go for initial unigrams:
    else if( queryLength == 1){
      nextWordList[[1]] <- "@@@"
      nextWordList[[2]] <- "@@@"
      predictorNGram <- tail(queryTokens[[1]], 1)
      resultSet <- sqldf(sprintf("SELECT Prediction 
                                  FROM bigramsDf
                                  WHERE Predictor = '%s' 
                                  ORDER BY Frequency DESC
                                  LIMIT 3"
                                 , predictorNGram)
      ) 
      if(nrow(resultSet) == 0){ nextWordList[[3]] <- "@@@"  }
      else{ nextWordList[[3]] <- as.list(resultSet$Prediction) }
    }
    else{
      nextWordList <- list("@@@","@@@","@@@")
    }
  }
  else{
    nextWordList <- list("@@@","@@@","@@@")
  }
  
  print(queryTokens)
  print(nextWordList)
  
  return(nextWordList)
}

## Main procedure to run a learning experiment
runSimpleNGramModel <- function(){
  
  suppressMessages(library(quanteda))
  suppressMessages(library(sqldf))
  suppressMessages(source("readData.R"))
  options(warn=-1)
  
  ### 1. Load and Sample the Data:
  start_time <- Sys.time()
  
  fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")
  fullDataUSBlogs <- readFullFile("./data/final/en_US/en_US.blogs.txt")
  fullDataUSNews <- readFullFile("./data/final/en_US/en_US.news.txt")
  
  set.seed(13)
  sampleSizePercentage <- .01     
  sampledDataUSTwitter <- sample(fullDataUSTwitter, round(sampleSizePercentage*length(fullDataUSTwitter)), replace = F)
  sampledDataUSBlogs <- sample(fullDataUSBlogs, round(sampleSizePercentage*length(fullDataUSBlogs)), replace = F)
  sampledDataUSNews <- sample(fullDataUSNews, round(sampleSizePercentage*length(fullDataUSNews)), replace = F) 
  sampledDataTwitterBlogsNews <- c(sampledDataUSTwitter, sampledDataUSBlogs, sampledDataUSNews)
  
  textData <- sampledDataTwitterBlogsNews
      #print(textData)
  end_time <- Sys.time()
  elapsed_time <- difftime(end_time, start_time, unit = "secs")
  displaySummaryOfTextVectorInConsole( textData, sampleSizePercentage, elapsed_time )
  
  ### 2. TOKENIZE and Clean the Corpus or Character Vector.
  start_time <- Sys.time()
  
  textTokens <- tokenizeAndcleanCorpus(textData)
  
  end_time <- Sys.time()
  getElapsedTimeMessage("TOKENIZE. Offline", start_time, end_time)
  
  ### 3. Compute n-grams. OFFLINE
  start_time <- Sys.time()
  
    #Obtain tokens data structures.
  unigrams <- tokens_ngrams(textTokens, n = 1)
  bigrams <- tokens_ngrams(textTokens, n = 2)
  trigrams <- tokens_ngrams(textTokens, n = 3)
  tetragrams <- tokens_ngrams(textTokens, n = 4)
  
    #Obtain dfms with frequencies.
  unigrams.dfm <- dfm(unigrams)
  bigrams.dfm <- dfm(bigrams)
  trigrams.dfm <- dfm(trigrams)
  tetragrams.dfm <- dfm(tetragrams)
  
    #Obtain SORTED Frequency vector of n-grams.
  unigrams.numVector <- topfeatures(unigrams.dfm, nfeat(unigrams.dfm))
  bigrams.numVector <- topfeatures(bigrams.dfm, nfeat(bigrams.dfm))
  trigrams.numVector <- topfeatures(trigrams.dfm, nfeat(trigrams.dfm))
  tetragrams.numVector <- topfeatures(tetragrams.dfm, nfeat(tetragrams.dfm))
  
    #Obtain data.frames with frequencies.
  unigrams.df <- convertNNVtoDF(unigrams.numVector, 1)
  bigrams.df <- convertNNVtoDF(bigrams.numVector, 2)
  trigrams.df <- convertNNVtoDF(trigrams.numVector, 3)
  tetragrams.df <- convertNNVtoDF(tetragrams.numVector, 4)
  
  end_time <- Sys.time()
  getElapsedTimeMessage("Computing n-grams. OFFLINE", start_time, end_time)
  
    #SAVE the data frames of unigrams, bigrams, trigrams, tetragrams here!
    #DO NOT RUN 1. 2. 3. again if you already computed the dataframes! USE the STORED dfs
    save(list = c("unigrams.df", "bigrams.df", "trigrams.df", "tetragrams.df"), file = "./data/ngrams.df.Rdata",
         compress = FALSE)
    
  ### 4. A) Make predictions using ONLY observed n-grams. ONLINE. ONLY COUNTS.
  
    # LOAD the ngrams data frames! Faster, do not calculate them again!  
    #load("./data/ngrams.df.RData",  verbose = TRUE)
  start_time <- Sys.time()  
  
  predictNextWordWithObservedNGrams( "right", bigrams.df, trigrams.df, tetragrams.df) 

  end_time <- Sys.time()
  getElapsedTimeMessage("Predicting with Simplest Model Observed ngrams", start_time, end_time)
}