## Load the dataset with a different seed than the training Data, then extract n-grams to be used as test cases and store them in a File
loadAndComputeTestCases <- function(){
  suppressMessages(library(quanteda))
  suppressMessages(source("readData.R"))
  options(warn=-1)
  
  set.seed(777)
  sampleSizePercentage <- c(0.0001) 
  
  start_time <- Sys.time()
  
  fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")
  fullDataUSBlogs <- readFullFile("./data/final/en_US/en_US.blogs.txt")
  fullDataUSNews <- readFullFile("./data/final/en_US/en_US.news.txt")
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
  
  #Obtain data.frames with frequencies.n-grams and probabilities.
  test.unigrams.df <- convertNNVtoDF(unigrams.numVector, 1)
  test.bigrams.df <- convertNNVtoDF(bigrams.numVector, 2)
  test.trigrams.df <- convertNNVtoDF(trigrams.numVector, 3)
  test.tetragrams.df <- convertNNVtoDF(tetragrams.numVector, 4)
  
  end_time <- Sys.time()
  getElapsedTimeMessage("Computing n-grams. OFFLINE", start_time, end_time)
  
  save(list = c("test.unigrams.df", "test.bigrams.df", "test.trigrams.df", "test.tetragrams.df"), 
       file = paste0("./data/ngrams.df.testcases.", sampleSizePercentage, ".Rdata"),
       compress = FALSE)
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

loadAndComputeTestCasesV2 <- function(){
  suppressMessages(library(quanteda))
  suppressMessages(source("readData.R"))
  options(warn=-1)
  
  set.seed(777)
  sampleSizePercentage <- c(0.0001) 
  
  start_time <- Sys.time()
  
  fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")
  fullDataUSBlogs <- readFullFile("./data/final/en_US/en_US.blogs.txt")
  fullDataUSNews <- readFullFile("./data/final/en_US/en_US.news.txt")
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
  
  #Obtain data.frames with frequencies.n-grams and probabilities.
  test.unigrams.df <- convertNNVtoDF(unigrams.numVector, 1)
  test.bigrams.df <- convertNNVtoDF(bigrams.numVector, 2)
  test.trigrams.df <- convertNNVtoDF(trigrams.numVector, 3)
  test.tetragrams.df <- convertNNVtoDF(tetragrams.numVector, 4)
  
  #Compute Probabilities for all n-gram dataframes
  test.unigrams.df <- computeProbabilityAndSaveDf(test.unigrams.df, 1, test.unigrams.df, test.bigrams.df, test.trigrams.df, test.tetragrams.df)
  test.bigrams.df <- computeProbabilityAndSaveDf(test.bigrams.df, 2, test.unigrams.df, test.bigrams.df, test.trigrams.df, test.tetragrams.df)
  test.trigrams.df <- computeProbabilityAndSaveDf(test.trigrams.df, 3, test.unigrams.df, test.bigrams.df, test.trigrams.df, test.tetragrams.df)
  test.tetragrams.df <- computeProbabilityAndSaveDf(test.tetragrams.df, 4, test.unigrams.df, test.bigrams.df, test.trigrams.df, test.tetragrams.df)
  
  end_time <- Sys.time()
  getElapsedTimeMessage("Computing n-grams. OFFLINE", start_time, end_time)
  
  save(list = c("test.unigrams.df", "test.bigrams.df", "test.trigrams.df", "test.tetragrams.df"), 
       file = paste0("./data/ngrams.df.testcases.", sampleSizePercentage, ".Rdata"),
       compress = FALSE)
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
  #print(paste("Number of Rows = ", nrow(df), "for", n, "-gram" ))
  #print(df)
  #print("#")
  return(df)
}

experimentAccuracy <- function(sampleSizePercentage, flagA, flagB){
  suppressMessages(source("nGramPredictionV1.R"))
  suppressMessages(source("readData.R"))
  options(warn=-1)
  
  results.bigrams.df <- data.frame(Test.Case = character(), 
                                   size = integer(), 
                                   predictor = character(), 
                                   true.value = character(), 
                                   prediction = character(),
                                   result = numeric())
  
  results.trigrams.df <- data.frame(Test.Case = character(), size = integer() )
  results.tetragrams.df <- data.frame(Test.Case = character(), size = integer() )
  
  #Load test cases
  load("./data/ngrams.df.testcases.1e-04.Rdata",  verbose = TRUE)
  
  #Load trainned Model Parameters
  load(paste0("./data/ngrams.df.p.", sampleSizePercentage, ".Rdata"),  verbose = TRUE)
  
  for(ng in 2:4){
   if(ng == 2){
     for(i in 1:nrow(test.bigrams.df)){
       #Construct test case.
       testCase <- paste(test.bigrams.df$word1[i], test.bigrams.df$word2[i])
       predictorWord <- test.bigrams.df$word1[i]
       trueWord <- test.bigrams.df$word2[i]
       print(paste("TEST CASE: ", i, test.bigrams.df$Words[i]))
       #print(testCase)
       #print(predictorWord)
       #print(trueWord)
       predictedWords <- nGramPredictionV2(predictorWord, flagA, flagB, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
       #print(paste("Predicted Word:", predictedWords, length(predictedWords)))
       predictedStringList <- paste(predictedWords, collapse = ", ")
       
       if( trueWord %in% predictedWords ){ resulted <- 1 } else { resulted <- 0}
       df <- data.frame(Test.Case = testCase, 
                        size = 2, 
                        predictor = predictorWord, 
                        true.value = trueWord, 
                        prediction = predictedStringList,
                        result = resulted)
       
       #Store test case result.  
       results.bigrams.df <- rbind(results.bigrams.df, df) 
     }
   }
   else if(ng == 3){
     for(i in 1:nrow(test.trigrams.df)){
       #Construct test case.
       testCase <- paste(test.trigrams.df$word1[i], test.trigrams.df$word2[i], test.trigrams.df$word3[i])
       predictorWord <- paste(test.trigrams.df$word1[i], test.trigrams.df$word2[i])
       trueWord <- test.trigrams.df$word3[i]
       print(paste("TEST CASE: ", i, test.trigrams.df$Words[i]))
       #print(testCase)
       #print(predictorWord)
       #print(trueWord)
       predictedWords <- nGramPredictionV2(predictorWord, flagA, flagB, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
       #print(paste("Predicted Word:", predictedWords, length(predictedWords)))
       predictedStringList <- paste(predictedWords, collapse = ", ")
       
       if( trueWord %in% predictedWords ){ resulted <- 1 } else { resulted <- 0}
       df <- data.frame(Test.Case = testCase, 
                        size = 3, 
                        predictor = predictorWord, 
                        true.value = trueWord, 
                        prediction = predictedStringList,
                        result = resulted)
       
       #Store test case result.  
       results.trigrams.df <- rbind(results.trigrams.df, df)
     }
   }
   else if(ng == 4){
     for(i in 1:nrow(test.tetragrams.df)){
       #Construct test case.
       testCase <- paste(test.tetragrams.df$word1[i], test.tetragrams.df$word2[i], test.tetragrams.df$word3[i], test.tetragrams.df$word4[i])
       predictorWord <- paste(test.tetragrams.df$word1[i], test.tetragrams.df$word2[i], test.tetragrams.df$word3[i])
       trueWord <- test.tetragrams.df$word4[i]
       print(paste("TEST CASE: ", i, test.tetragrams.df$Words[i]))
       #print(testCase)
       #print(predictorWord)
       #print(trueWord)
       predictedWords <- nGramPredictionV2(predictorWord, flagA, flagB, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
       #print(paste("Predicted Word:", predictedWords, length(predictedWords)))
       predictedStringList <- paste(predictedWords, collapse = ", ")
       
       if( trueWord %in% predictedWords ){ resulted <- 1 } else { resulted <- 0}
       df <- data.frame(Test.Case = testCase, 
                        size = 4, 
                        predictor = predictorWord, 
                        true.value = trueWord, 
                        prediction = predictedStringList,
                        result = resulted)
       
       #Store test case result.  
       results.tetragrams.df <- rbind(results.tetragrams.df, df)
     }
   }
  }
  
  print(paste("Bigram Results for Sample size:", sampleSizePercentage, flagA, flagB))
  print(paste("Bigram test cases:", nrow(results.bigrams.df)))
  print(paste("Bigram test Sum Positives:", sum(results.bigrams.df$result)))
  print(paste("Bigram test Accuracy:", sum(results.bigrams.df$result)/nrow(results.bigrams.df) ))
  
  print(paste("Trigram Results for Sample size:", sampleSizePercentage, flagA, flagB))
  print(paste("Trigram test cases:", nrow(results.trigrams.df)))
  print(paste("Trigram test Sum Positives:", sum(results.trigrams.df$result)))
  print(paste("Trigram test Accuracy:", sum(results.trigrams.df$result)/nrow(results.trigrams.df) ))
  
  print(paste("Tetragram Results for Sample size:", sampleSizePercentage, flagA, flagB))
  print(paste("Tetragram test cases:", nrow(results.tetragrams.df)))
  print(paste("Tetragram test Sum Positives:", sum(results.tetragrams.df$result)))
  print(paste("Tetragram test Accuracy:", sum(results.tetragrams.df$result)/nrow(results.tetragrams.df) ))
}

experimentPerplexity <- function( sampleSizePercentage ){
  suppressMessages(source("nGramPredictionV1.R"))
  suppressMessages(source("readData.R"))
  options(warn=-1)
  
  perplexity.bigrams.df <- data.frame(w1w2 = character(), 
                                   w1 = character(), 
                                   p.w2gw1 = numeric(),
                                   p.w1 = numeric(),
                                   perplexity = numeric())
  
  perplexity.trigrams.df <- data.frame(w1w2w3 = character(), 
                                       w1w2 = character(),
                                       w1 = character(),
                                       p.w3gw1w2 = numeric(),
                                       p.w2gw1 = numeric(),
                                       p.w1 = numeric(),
                                       perplexity = numeric()
                                       )
  perplexity.tetragrams.df <- data.frame(w1w2w3w4 = character(), 
                                         w1w2w3 = character(),
                                         w1w2 = character(),
                                         w1 = character(),
                                         p.w4gw1w2w3 = numeric(),
                                         p.w3gw1w2 = numeric(),
                                         p.w2gw1 = numeric(),
                                         p.w1 = numeric(),
                                         perplexity = numeric()
                                         )
  
  #Load test cases
  load("./data/ngrams.df.testcases.1e-04.Rdata",  verbose = TRUE)
  
  #Load trainned Model Parameters
  load(paste0("./data/ngrams.df.p.", sampleSizePercentage, ".Rdata"),  verbose = TRUE)
  
  for(ng in 2:4){
    if(ng == 2){
      for(i in 1:nrow(test.bigrams.df)){
        
        Probw2givenw1 <- bigrams.df[bigrams.df["Words"] == test.bigrams.df$Words[i], "Probability"]
        if( length(Probw2givenw1) == 0 ||  Probw2givenw1 == 0) { Probw2givenw1 <- 0.0000001 }
        
        Probw1 <- unigrams.df[unigrams.df["Words"] == test.bigrams.df$word1[i], "Probability"]
        if( length(Probw1) == 0 ||  Probw1 == 0) { Probw1 <- 0.0000001 }
        
        perplexityBigram <- computePerplexityBigram(Probw2givenw1, Probw1)
        
        df <- data.frame(w1w2 = test.bigrams.df$Words[i], 
                         w1 = test.bigrams.df$word1[i], 
                         p.w2gw1 = Probw2givenw1,
                         p.w1 = Probw1,
                         perplexity = perplexityBigram
                         )
        
        print(paste("TEST CASE: ", i))
        print(df)
        
        #Store test case result.  
        perplexity.bigrams.df <- rbind(perplexity.bigrams.df, df)
      }
    }
    else if(ng == 3){
      for(i in 1:nrow(test.trigrams.df)){
        word1.word2.word3 <- test.trigrams.df$Words[i]
        word1.word2 <- paste(test.trigrams.df$word1[i], test.trigrams.df$word2[i], sep = "_")
        word1 <- test.trigrams.df$word1[i]
        
        Probw3givenw1w2 <- trigrams.df[trigrams.df["Words"] == word1.word2.word3, "Probability"]
        if( length(Probw3givenw1w2) == 0 ||  Probw3givenw1w2 == 0) { Probw3givenw1w2 <- 0.0000001 }
        
        Probw2givenw1 <- bigrams.df[bigrams.df["Words"] == word1.word2, "Probability"]
        if( length(Probw2givenw1) == 0 ||  Probw2givenw1 == 0) { Probw2givenw1 <- 0.0000001 }
        
        Probw1 <- unigrams.df[unigrams.df["Words"] ==  word1, "Probability"]
        if( length(Probw1) == 0 ||  Probw1 == 0) { Probw1 <- 0.0000001 }
        
        perplexityTrigram <- computePerplexityTrigram(Probw3givenw1w2, Probw2givenw1, Probw1)
        
        df <- data.frame(w1w2w3 = word1.word2.word3, 
                         w1w2 = word1.word2,
                         w1 = word1,
                         p.w3gw1w2 = Probw3givenw1w2,
                         p.w2gw1 = Probw2givenw1,
                         p.w1 = Probw1,
                         perplexity = perplexityTrigram
                        )
        
        print(paste("TEST CASE: ", i))
        print(df)
        
        #Store test case result.  
        perplexity.trigrams.df <- rbind(perplexity.trigrams.df, df)
      }
    }
    else if(ng == 4){
      for(i in 1:nrow(test.tetragrams.df)){
        word1.word2.word3.word4 <- test.tetragrams.df$Words[i]
        word1.word2.word3 <- paste(test.tetragrams.df$word1[i], test.tetragrams.df$word2[i], test.tetragrams.df$word3[i], sep = "_")
        word1.word2 <- paste(test.tetragrams.df$word1[i], test.tetragrams.df$word2[i], sep = "_")
        word1 <- test.tetragrams.df$word1[i]
        
        Probw4givenw1w2w3 <- tetragrams.df[tetragrams.df["Words"] == word1.word2.word3.word4, "Probability"]
        if( length(Probw4givenw1w2w3) == 0 ||  Probw4givenw1w2w3 == 0) { Probw4givenw1w2w3 <- 0.0000001 }
        
        Probw3givenw1w2 <- trigrams.df[trigrams.df["Words"] == word1.word2.word3, "Probability"]
        if( length(Probw3givenw1w2) == 0 ||  Probw3givenw1w2 == 0) { Probw3givenw1w2 <- 0.0000001 }
        
        Probw2givenw1 <- bigrams.df[bigrams.df["Words"] == word1.word2, "Probability"]
        if( length(Probw2givenw1) == 0 ||  Probw2givenw1 == 0) { Probw2givenw1 <- 0.0000001 }
        
        Probw1 <- unigrams.df[unigrams.df["Words"] ==  word1, "Probability"]
        if( length(Probw1) == 0 ||  Probw1 == 0) { Probw1 <- 0.0000001 }
        
        perplexityTetragram <- computePerplexityTetragram(Probw4givenw1w2w3, Probw3givenw1w2, Probw2givenw1, Probw1)
        
        df <- data.frame(w1w2w3w4 = word1.word2.word3.word4, 
                         w1w2w3 = word1.word2.word3,
                         w1w2 = word1.word2,
                         w1 = word1,
                         p.w4gw1w2w3 = Probw4givenw1w2w3,
                         p.w3gw1w2 = Probw3givenw1w2,
                         p.w2gw1 = Probw2givenw1,
                         p.w1 = Probw1,
                         perplexity = perplexityTetragram
                         )
        print(paste("TEST CASE: ", i))
        print(df)
        
        #Store test case result.  
        perplexity.tetragrams.df <- rbind(perplexity.tetragrams.df, df)
      }
    }
  }
  
  print(paste("Bigram Perplexity for Sample size:", sampleSizePercentage))
  print(paste("Bigram test cases:", nrow(perplexity.bigrams.df)))
  print(paste("Bigram test Perplexity:", mean(perplexity.bigrams.df$perplexity)))
  
  print(paste("Trigram Perplexity for Sample size:", sampleSizePercentage))
  print(paste("Trigram test cases:", nrow(perplexity.trigrams.df)))
  print(paste("Trigram test Perplexity:", mean(perplexity.trigrams.df$perplexity)))
  
  print(paste("Tetragram Perplexity for Sample size:", sampleSizePercentage))
  print(paste("Tetragram test cases:", nrow(perplexity.tetragrams.df)))
  print(paste("Tetragram test Perplexity:", mean(perplexity.tetragrams.df$perplexity)))
}

computePerplexityBigram <- function(pw2gw1, pw1){
  jpd.w1.w2 <- pw2gw1*pw1  #Joint probability distribution computed using the chain rule.
  
  perplexityBi <- ( (1/jpd.w1.w2) )^(1/2)
  
  return(perplexityBi)
}

computePerplexityTrigram <- function(pw3gw1w2, pw2gw1, pw1){
  jpd.w1.w2.w3 <- pw3gw1w2*pw2gw1*pw1  #Joint probability distribution computed using the chain rule.
  
  perplexityTri <- ( (1/jpd.w1.w2.w3) )^(1/3)
  
  return(perplexityTri)
}

computePerplexityTetragram <- function(pw4gw1w2w3, pw3gw1w2, pw2gw1, pw1){
  jpd.w1.w2.w3.w4 <- pw4gw1w2w3*pw3gw1w2*pw2gw1*pw1  #Joint probability distribution computed using the chain rule.
  
  perplexityTetra <- ( (1/jpd.w1.w2.w3.w4) )^(1/4)
  
  return(perplexityTetra)
}
