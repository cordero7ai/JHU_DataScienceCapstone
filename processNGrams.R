#We open a Corpus of Documents and produce data frames for unigrams, bigrams, trigrams and tetragrams.
processNGrams <- function(){
  suppressMessages(library(quanteda))
  suppressMessages(source("readData.R"))
  options(warn=-1)
  
  set.seed(13)
  sampleSizePercentage <- c(0.03) 
  
  for(i in 1:length(sampleSizePercentage)){
    ### 1. Load and Sample the Data:
    start_time <- Sys.time()
  
    fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")
    fullDataUSBlogs <- readFullFile("./data/final/en_US/en_US.blogs.txt")
    fullDataUSNews <- readFullFile("./data/final/en_US/en_US.news.txt")
    sampledDataUSTwitter <- sample(fullDataUSTwitter, round(sampleSizePercentage[i]*length(fullDataUSTwitter)), replace = F)
    sampledDataUSBlogs <- sample(fullDataUSBlogs, round(sampleSizePercentage[i]*length(fullDataUSBlogs)), replace = F)
    sampledDataUSNews <- sample(fullDataUSNews, round(sampleSizePercentage[i]*length(fullDataUSNews)), replace = F) 
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
    
    #Obtain data.frames with frequencies.n-grams and probabilities.
    unigrams.df <- convertNNVtoDF(unigrams.numVector, 1)
    bigrams.df <- convertNNVtoDF(bigrams.numVector, 2)
    trigrams.df <- convertNNVtoDF(trigrams.numVector, 3)
    tetragrams.df <- convertNNVtoDF(tetragrams.numVector, 4)
    
    save(list = c("unigrams.df", "bigrams.df", "trigrams.df", "tetragrams.df"), 
         file = paste0("./data/ngrams.df.f.", sampleSizePercentage[i], ".Rdata"),
         compress = FALSE)
    
    #Compute Probabilities for all n-gram dataframes
    unigrams.df <- computeProbabilityAndSaveDf(unigrams.df, 1, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
    bigrams.df <- computeProbabilityAndSaveDf(bigrams.df, 2, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
    trigrams.df <- computeProbabilityAndSaveDf(trigrams.df, 3, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
    tetragrams.df <- computeProbabilityAndSaveDf(tetragrams.df, 4, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
    
    save(list = c("unigrams.df", "bigrams.df", "trigrams.df", "tetragrams.df"), 
         file = paste0("./data/ngrams.df.p.", sampleSizePercentage[i], ".Rdata"),
         compress = FALSE)
    
    end_time <- Sys.time()
    getElapsedTimeMessage("Computing n-grams. OFFLINE", start_time, end_time)
  }
  
  return(sampleSizePercentage)
}

## Converts numeric named vector into customized n-gram dataframe
convertNNVtoDF <- function(ngramNumVector, n){
  
  df <- stack(ngramNumVector)
  df <- setNames(df[2:1], c('Words','Frequency'))
  df$Words <- as.character(df$Words)
  df$Predictor <- gsub("_([^_]*)$", "", df$Words)
  df$Prediction <- gsub("^.*_", "", df$Words)
  df$word1 <- gsub("_(.*)", "", df$Words)
  
  if(n == 2){
    df$word2 <- gsub("^.*_", "", df$Words)
  }
  else if(n == 3){
    df$word2 <- gsub("^.*_", "", df$Predictor)  
    df$word3 <- gsub("^.*_", "", df$Words)
    #df$Last1Gram <- gsub("^.*_", "", df$Predictor)
  }  
  else if(n == 4){
    aux3Gram <- gsub("_([^_]*)$", "", df$Words)
    aux2Gram <- gsub("_([^_]*)$", "", aux3Gram)
    df$word2 <- gsub("^.*_", "", aux2Gram)
    df$word3 <- gsub("^.*_", "", aux3Gram)
    df$word4 <- gsub("^.*_", "", df$Words)
    #df$Last2Gram <- gsub("^.+?(_)", "", df$Predictor3)
    #df$Last1Gram <- gsub("^.*_", "", df$Last2Gram)
  }
  print(paste("Number of Rows = ", nrow(df), "for", n, "-gram" ))
  print(df)
  print("#")
  return(df)
}

computeProbabilityAndSaveDf <- function(df, n, unigrams.df, bigrams.df, trigrams.df, tetragrams.df){
  df$Probability <- 0
  if(n == 1){
    totalWords <- sum(df$Frequency)
    for(i in 1:nrow(df)){
      unigramCounts <- df$Frequency[df["Words"] == df$Words[i]]
      df$Probability[i] <- unigramCounts / totalWords
    }
  }
  else if(n == 2){
    for(i in 1:nrow(df)){
      w1w2 <- df$Words[i]
      w1w2Counts <- df$Frequency[df["Words"] == w1w2]
      w1 <- df$word1[i] 
      w1Counts <- unigrams.df$Frequency[unigrams.df["Words"] == w1]
      if(length(w1w2Counts) == 0 || length(w1Counts) == 0)
      {
        df$Probability[i] <- 0
      }
      else{
        df$Probability[i] <- w1w2Counts / w1Counts
      }
    }
  }
  else if(n == 3){
    for(i in 1:nrow(df)){
      w1w2w3 <- df$Words[i]
      w1w2w3Counts <- df$Frequency[df["Words"] == w1w2w3]
      w1w2 <- paste0(df$word1[i], "_", df$word2[i])
      w1w2Counts <- bigrams.df$Frequency[bigrams.df["Words"] == w1w2]
      if(length(w1w2w3Counts) == 0 || length(w1w2Counts) == 0)
      {
        df$Probability[i] <- 0
      }
      else{
        df$Probability[i] <- w1w2w3Counts / w1w2Counts
      }
    }
  }
  else if(n == 4){
    for(i in 1:nrow(df)){
      w1w2w3w4 <- df$Words[i]
      w1w2w3w4Counts <- df$Frequency[df["Words"] == w1w2w3w4]
      w1w2w3 <- paste0(df$word1[i], "_", df$word2[i], "_", df$word3[i])
      w1w2w3Counts <- trigrams.df$Frequency[trigrams.df["Words"] == w1w2w3]
      if(length(w1w2w3w4Counts) == 0 || length(w1w2w3Counts) == 0)
      {
        df$Probability[i] <- 0
      }
      else{
        df$Probability[i] <- w1w2w3w4Counts / w1w2w3Counts
      }
    }
  }
  
  print(paste("Number of Rows = ", nrow(df), "for", n, "-gram" ))
  print(df)
  print("#")
  return(df)
}

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
                     remove_symbols = TRUE,
                     
                     )
  
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
