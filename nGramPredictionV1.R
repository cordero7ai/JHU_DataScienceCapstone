#This function predicts a vector of three suggestions based on the inputed n-gram and the set of observed n-grams that were
#computed from the Corpus in the processNGrams.R script. But also predicts based in the HT algotihm for unobserved lexems in n-grams.

nGramPredictionV1 <- function(inputNGram, sampledFile){
  suppressMessages(library(quanteda))
  options(warn=-1)
  set.seed(13)
  load(sampledFile,  verbose = TRUE)
  start_time <- Sys.time()
  
  #@@@ inside prediction Vector indicates an empty prediction slot.
  predictionVector <- c("@@@","@@@","@@@")
  
  inputNGram.df <- cleanInputNGram(inputNGram)
  print("Input n-gram:")
  print(inputNGram.df)
  print("")
  
  #A. Predict words based upon the observed n-grams in the data.
  predictionVector <- predictAssumingObservedNGram(inputNGram.df, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
  
  #B. Predict words that were not observed in the n-grams in the data.
  #We use the Historical Track (HT) algorithm described in section Task 4 in the Journal Readme.md, Readme.html in the repository.
  predictionVector <- c(predictionVector, predictAssumingUnobservedNGram(inputNGram.df, unigrams.df, bigrams.df, trigrams.df, tetragrams.df))
  
  #Remove blanks and set the final vector to be proposed for prediction.
  #The three first slots corresponds to predictions from observed n-grams in Corpus, the remaining 3 proceed from the HT algorithm
  #for unobserved n-grams and or lexems.
  print("***************************")
  print("Initial Prediction Vector:")
  print(predictionVector)
  print("*** ***")
  finalPredictionVector <- c()
  for(i in 1:length(predictionVector)){
    if(predictionVector[i] != "@@@"){
      finalPredictionVector <- c(finalPredictionVector, predictionVector[i])      
      if(length(finalPredictionVector) == 3){
        break
      }
    }
  }
  print("Proposed Prediction Vector:")
  print(finalPredictionVector)
  print(paste(" for input n-gram: \"", inputNGram, "\" using the sampled Data:", sampledFile))
  print("***")
  
  end_time <- Sys.time()
  getElapsedTimeMessage("PREDICTION. ONLINE", start_time, end_time)
  
  return(finalPredictionVector)
}

#A. Predicts from observed full n-grams. Or partially matching n-grams from the previous (n-k)-grams. 
predictAssumingObservedNGram <- function(inputNGram.df, unigrams.df, bigrams.df, trigrams.df, tetragrams.df){
  set.seed(13)
  predictionVector <- c("@@@","@@@","@@@")
  
  #1. Look for EXACT n-gram matches given the input. Suggest the top three words with highest probabilities.
  #2. In case that we found less than 3 suggestions. Use backoff models: n-1, n-2, grams. And for each case try to get the top three
  #   with highest probabilities.
  
  if( inputNGram.df$size == 1 ){
    resultSet.df <- bigrams.df[ bigrams.df["Predictor"] == inputNGram.df$Words[1], ]
    resultSet.df <- head(resultSet.df[ order(-resultSet.df$Probability),], 3)
    predictionVector <- addPredictionsFromDFToVector(resultSet.df, predictionVector, inputNGram.df$Unigram[1])
  }
  else if( inputNGram.df$size == 2 ){
    ##Match exact bigrams
    resultSet.df <- trigrams.df[ trigrams.df["Predictor"] == inputNGram.df$Words[1], ]
    resultSet.df <- head(resultSet.df[ order(-resultSet.df$Probability),], 3)
    predictionVector <- addPredictionsFromDFToVector(resultSet.df, predictionVector, inputNGram.df$Unigram[1])
    
    ##If needed match exact unigrams
    resultSet.df <- bigrams.df[ bigrams.df["Predictor"] == inputNGram.df$Unigram[1], ]
    resultSet.df <- head(resultSet.df[ order(-resultSet.df$Probability),], 3)
    predictionVector <- addPredictionsFromDFToVector(resultSet.df, predictionVector, inputNGram.df$Unigram[1])
  }
  else if( inputNGram.df$size == 3 ){
    ##Match exact trigrams
    resultSet.df <- tetragrams.df[ tetragrams.df["Predictor"] == inputNGram.df$Words[1], ]
    resultSet.df <- head(resultSet.df[ order(-resultSet.df$Probability),], 3)
    predictionVector <- addPredictionsFromDFToVector(resultSet.df, predictionVector, inputNGram.df$Unigram[1])
    
    ##If needed match exact bigrams
    resultSet.df <- trigrams.df[ trigrams.df["Predictor"] == inputNGram.df$Bigram[1], ]
    resultSet.df <- head(resultSet.df[ order(-resultSet.df$Probability),], 3)
    predictionVector <- addPredictionsFromDFToVector(resultSet.df, predictionVector, inputNGram.df$Unigram[1])
    
    ##If needed match exact unigrams
    resultSet.df <- bigrams.df[ bigrams.df["Predictor"] == inputNGram.df$Unigram[1], ]
    resultSet.df <- head(resultSet.df[ order(-resultSet.df$Probability),], 3)
    predictionVector <- addPredictionsFromDFToVector(resultSet.df, predictionVector, inputNGram.df$Unigram[1])
  }
  return(predictionVector)
}

#B. Predicts from unobserved n-grams. Using the Historical Track algorithm.
predictAssumingUnobservedNGram <- function(inputNGram.df, unigrams.df, bigrams.df, trigrams.df, tetragrams.df){
  set.seed(13)
  predictionVector <- c("@@@","@@@","@@@")
  
  ##We received a sequence w_{1}^{n-1} that did not match an observed n-gram.
  ##Choose the word to be predicted wn with the highest joint probability distribution P(w1,w2,...,wn)
  ##Approximate the jpd by using the chain rule and the bigram model P(w1,w2,...,wn) = P(w1)P(w2|w1)P(w3|w2)...P(wn|wn-1)
  ## thus, we need to decompose the n-gram into bigrams. e.g. predict w4 based on-> w1, (w2,w1), (w3,w2), (w4, w3).
  ## we will predict an observed word w4.
  ## In case we find unobserved words in the sequence, we use the most likely words to replace them.
  ## if (wi, wi+1) are observed, keep them in the sequence as they are.
  ## if there is one unobserved wi in the pair wi,wj. use the other word to find the other most likely word wj (highest frequency wi wj in the bigrams) 
  ## propagate predicted words until we reach the end.
  ## If we introduce an n-gram that has not been observed before in any subset way then substitute with the most common observed n-gram and look for predictions.
  ## In this way we substitute the unknown word with a known one by using our historial track in the Corpus.
  ## once we auto completed the word, we use the original prediction algorithm in A) to look for the final prediction w4.
  
  if( inputNGram.df$size == 1 ){   # Unknown unigram received. just predict the three most frequent words. Assume conditional independence. P(w1,w2) = P(w1)P(w2|w1)
    resultSet.df <- head(unigrams.df[ order(-unigrams.df$Probability),], 1)   # w1 is not known so we use the most likely frequent word 
    autoCompletedNGram.df <- data.frame(Words = resultSet.df$Words[1], size = 1)
    autoCompletedNGram.df$word1 <- as.character(resultSet.df$Words[1])
    autoCompletedNGram.df$Unigram <- as.character(resultSet.df$Words[1])
  }
  else if( inputNGram.df$size == 2 ){ #Decompose into P(w1,w2,w3) = P(w1)P(w2|w1)P(w3|w2) to predict w3. w1, w2 can be unknown.
    word1.df <- unigrams.df[ unigrams.df["Words"] == inputNGram.df$word1[1], ]
    word2.df <- unigrams.df[ unigrams.df["Words"] == inputNGram.df$word2[1], ]
    
    if( (nrow(word1.df) == 0 && nrow(word2.df) == 0) || (nrow(word1.df) == 1 && nrow(word2.df) == 1)){  # w1, w2 not observed
      resultSet.df <- head(bigrams.df[ order(-bigrams.df$Frequency),], 1)
      autoCompletedNGram.df <- data.frame(Words = resultSet.df$Words[1], size = 2)
      autoCompletedNGram.df$word1 <- as.character(resultSet.df$word1[1])
      autoCompletedNGram.df$word2 <- as.character(resultSet.df$word2[1])
      autoCompletedNGram.df$Unigram <- as.character(resultSet.df$word2[1])
    }
    else if( nrow(word1.df) == 0 && nrow(word2.df) == 1 ){ #w2 has been observed.
      resultSet.df <- bigrams.df[ bigrams.df["word2"] == inputNGram.df$word2[1], ]
      resultSet.df <- head(resultSet.df[ order(-resultSet.df$Frequency),], 1)
      autoCompletedNGram.df <- data.frame(Words = resultSet.df$Words[1], size = 2)
      autoCompletedNGram.df$word1 <- as.character(resultSet.df$word1[1])
      autoCompletedNGram.df$word2 <- as.character(resultSet.df$word2[1])
      autoCompletedNGram.df$Unigram <- as.character(resultSet.df$word2[1])
    }
    else if( nrow(word1.df) == 1 && nrow(word2.df) == 0 ){ #w1 has been observed.
      resultSet.df <- bigrams.df[ bigrams.df["word1"] == inputNGram.df$word1[1], ]
      resultSet.df <- head(resultSet.df[ order(-resultSet.df$Frequency),], 1)
      autoCompletedNGram.df <- data.frame(Words = resultSet.df$Words[1], size = 2)
      autoCompletedNGram.df$word1 <- as.character(resultSet.df$word1[1])
      autoCompletedNGram.df$word2 <- as.character(resultSet.df$word2[1])
      autoCompletedNGram.df$Unigram <- as.character(resultSet.df$word2[1])
    }
  }
  else if( inputNGram.df$size == 3 ){   #Find P(w1,w2,w3, w4) = P(w1)P(w2|w1)P(w3|w2)P(w4|w3) with autocompletion according to maximum likelihood substitutions
    word1.df <- unigrams.df[ unigrams.df["Words"] == inputNGram.df$word1[1], ]
    word2.df <- unigrams.df[ unigrams.df["Words"] == inputNGram.df$word2[1], ]
    word3.df <- unigrams.df[ unigrams.df["Words"] == inputNGram.df$word3[1], ]
    mostLikelyNGram <- c()
  
    ## complete w1 and w2 as necessary
    if( (nrow(word1.df) == 0 && nrow(word2.df) == 0) || (nrow(word1.df) == 1 && nrow(word2.df) == 1) ){  # w1, w2 not observed
      resultSet.df <- head(bigrams.df[ order(-bigrams.df$Frequency),], 1)
      mostLikelyNGram <- c(mostLikelyNGram, resultSet.df$word1, resultSet.df$word2)
    }
    else if( nrow(word1.df) == 0 && nrow(word2.df) == 1 ){ #w2 has been observed.
      resultSet.df <- bigrams.df[ bigrams.df["word2"] == inputNGram.df$word2[1], ]
      resultSet.df <- head(resultSet.df[ order(-resultSet.df$Frequency),], 1)
      mostLikelyNGram <- c(mostLikelyNGram, resultSet.df$word1, resultSet.df$word2)
    }
    else if( nrow(word1.df) == 1 && nrow(word2.df) == 0 ){ #w1 has been observed.
      resultSet.df <- bigrams.df[ bigrams.df["word1"] == inputNGram.df$word1[1], ]
      resultSet.df <- head(resultSet.df[ order(-resultSet.df$Frequency),], 1)
      mostLikelyNGram <- c(mostLikelyNGram, resultSet.df$word1, resultSet.df$word2)
    }
    
    ## complete w3 conditioned on previous w2 and w1
    if( nrow(word3.df) == 0 ){
      resultSet.df <- bigrams.df[ bigrams.df["word1"] == mostLikelyNGram[2], ]
      resultSet.df <- head(resultSet.df[ order(-resultSet.df$Frequency),], 1)
      mostLikelyNGram <- c(mostLikelyNGram, resultSet.df$word2[1])
    }
    else{
      mostLikelyNGram <- c(mostLikelyNGram, inputNGram.df$word3[1])
    }
    autoCompletedNGram.df <- data.frame(Words = paste(mostLikelyNGram , collapse = "_"), size = 3)
    autoCompletedNGram.df$word1 <- as.character(mostLikelyNGram[1])
    autoCompletedNGram.df$word2 <- as.character(mostLikelyNGram[2])
    autoCompletedNGram.df$word3 <- as.character(mostLikelyNGram[3])
    autoCompletedNGram.df$Unigram <- as.character(mostLikelyNGram[3])
    autoCompletedNGram.df$Bigram <- as.character(paste(as.character(mostLikelyNGram[2]), as.character(mostLikelyNGram[3]), sep = "_"))
  }
  
  if(!is.na(autoCompletedNGram.df) && !is.na(autoCompletedNGram.df$Unigram)){
  predictionVector <- predictAssumingObservedNGram(autoCompletedNGram.df, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
  }
  return(predictionVector)
}

getElapsedTimeMessage <- function(message, st, et){
  print("**************************************************************")
  print(paste("Elapsed Time in Seconds for:", message, difftime(et, st, unit = "secs") ))
  print("**************************************************************")
  print("*")
}

#If there are less than 3 predictions in the vector, we return TRUE
checkForRemainingPredictions <- function(predictionVector){
  if(predictionVector[1] == "@@@" || predictionVector[2] == "@@@" || predictionVector[3] == "@@@"){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#Receives a vector of characters and outputs a dataframe with its processed tokens.
cleanInputNGram <- function(inputNGram){
  
  inputNGram <- gsub("_", "", inputNGram)
  queryTokens <- tokenizeAndcleanCorpus(inputNGram)
  
  if(ntoken(queryTokens) <= 2){
    n <- as.numeric(ntoken(queryTokens))
  }
  else{
    n <- 3
  }
  predictorTokens <- tail(queryTokens[[1]], 3)
  predictorNGram <- paste(tail(queryTokens[[1]], 3), collapse = "_")
  df <- data.frame(Words = predictorNGram, size = n)
  df$word1 <- as.character(predictorTokens[1])
  if(n == 1){ df$Unigram <- as.character(predictorTokens[1]) }
  if(n == 2){
    df$word2 <- as.character(predictorTokens[2])
    df$Unigram <- as.character(predictorTokens[2])
  }
  else if(n >= 3){
    df$word2 <- as.character(predictorTokens[2])
    df$word3 <- as.character(predictorTokens[3])
    df$Unigram <- as.character(predictorTokens[3])
    df$Bigram <- as.character(paste(as.character(predictorTokens[2]), as.character(predictorTokens[3]), sep = "_"))
  }
  
  return( df )
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

#Receives a dataframe with predictions n <= 3 and adds them to empty slots of the prediction vector.
addPredictionsFromDFToVector <- function(df, predictionVector, predictorLastUnigram){
  if(nrow(df) >0){
    for(i in 1:nrow(df)){
      if(checkForRemainingPredictions(predictionVector))
      {  
        for(j in 1:length(predictionVector)){
          if(predictionVector[j] == "@@@"){
            if(!(predictorLastUnigram == df[i,]$Prediction))
            {  
              predictionVector[j] <- df[i,]$Prediction
              break
            }
          }
        }
      }
      
    }  
  }
  return(predictionVector)
}

## Same but including flags
nGramPredictionV2 <- function(inputNGram, flagA, flagB, unigrams.df, bigrams.df, trigrams.df, tetragrams.df){
  suppressMessages(library(quanteda))
  options(warn=-1)
  set.seed(13)
  
  #@@@ inside prediction Vector indicates an empty prediction slot.
  predictionVector <- c("@@@","@@@","@@@")
  
  inputNGram.df <- cleanInputNGram(inputNGram)
  print("Input n-gram:")
  print(inputNGram.df)
  print("")
  
  #A. Predict words based upon the observed n-grams in the data.
  if(flagA){
    predictionVector <- predictAssumingObservedNGram(inputNGram.df, unigrams.df, bigrams.df, trigrams.df, tetragrams.df)
  }
  #B. Predict words that were not observed in the n-grams in the data.
  #We use the Historical Track (HT) algorithm described in section Task 4 in the Journal Readme.md, Readme.html in the repository.
  if(flagB){
    predictionVector <- c(predictionVector, predictAssumingUnobservedNGram(inputNGram.df, unigrams.df, bigrams.df, trigrams.df, tetragrams.df))
  }
  #Remove blanks and set the final vector to be proposed for prediction.
  #The three first slots corresponds to predictions from observed n-grams in Corpus, the remaining 3 proceed from the HT algorithm
  #for unobserved n-grams and or lexems.
  finalPredictionVector <- c()
  for(i in 1:length(predictionVector)){
    if(predictionVector[i] != "@@@"){
      finalPredictionVector <- c(finalPredictionVector, predictionVector[i])      
      if(length(finalPredictionVector) == 3){
        break
      }
    }
    
  }

  return(finalPredictionVector)
}
