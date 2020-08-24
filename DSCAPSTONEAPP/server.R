#
# Final Project for the Data Science Capstone Course 
# Johns Hopkins University
# Author: Jorge Cordero
# August the 23rd, 2020
#
suppressMessages(library(quanteda))
options(warn=-1)
load("./data/ngrams.df.p.0.03.Rdata",  verbose = TRUE)
set.seed(13)

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

#If there are less than 3 predictions in the vector, we return TRUE
checkForRemainingPredictions <- function(predictionVector){
    if(predictionVector[1] == "@@@" || predictionVector[2] == "@@@" || predictionVector[3] == "@@@"){
        return(TRUE)
    }
    else{
        return(FALSE)
    }
}

nGramPredictionV2 <- function(inputNGram, flagA, flagB, unigrams.df, bigrams.df, trigrams.df, tetragrams.df){
    
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

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$outWords <- renderText({  input$wordsInput  })
    
    output$outAlgc <- renderText({
        message <- "The predictions were calculated based on the"
        message <- paste(message, input$algorithmC)
        
        if(input$algorithmC == "EMS" || input$algorithmC == "HT")
        {
            message <- paste(message, "algorithm.")
        }
        else
        {
            message <- paste(message, "algorithms.")
        }
        
    })
    
    output$outCleannedWord <- renderText({
        if(input$wordsInput != "" )
        {
            inputNGram.df <- cleanInputNGram(input$wordsInput )
            gsub("_", " ", inputNGram.df$Words)
        }
    })
    
    output$outPredictions <- renderText({
        if(input$wordsInput != "" )
        {
            if(input$algorithmC == "EMS") { 
                flagA <- TRUE 
                flagB <- FALSE
            }
            else if(input$algorithmC == "HT") { 
                flagA <- FALSE
                flagB <- TRUE
            }
            else if(input$algorithmC == "EMS + HT") { 
                flagA <- TRUE
                flagB <- TRUE
            }
            
            print(flagA)
            print(flagB)
            
            predictedWords <- nGramPredictionV2(input$wordsInput, 
                                                flagA, 
                                                flagB, 
                                                unigrams.df, 
                                                bigrams.df, 
                                                trigrams.df, 
                                                tetragrams.df)
            
            paste(predictedWords, collapse=",  ")
        }
        else
        {
            "Enter TEXT!!"
        }
    })
    
    output$plotUnigrams<-renderPlot({
        unigramsDf <- head(unigrams.df, 25)
        
        p <- ggplot(data=unigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                         fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
        p + xlab("Word") +labs(title = "Top 25 UNIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
    })
    
    output$plotBigrams<-renderPlot({
        bigramsDf <- head(bigrams.df, 25)
        
        p <- ggplot(data=bigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                         fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
        p + xlab("Word") +labs(title = "Top 25 BIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
    })
    
    output$plotTrigrams<-renderPlot({
        trigramsDf <- head(trigrams.df, 25)
        
        p <- ggplot(data=trigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                         fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
        p + xlab("Word") +labs(title = "Top 25 TRIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
    })
    
    output$plotTetragrams<-renderPlot({
        tetragramsDf <- head(tetragrams.df, 25)
    
        p <- ggplot(data=tetragramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                         fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
        p + xlab("Word") +labs(title = "Top 25 TETRAGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
    })
    
    
    output$tableAccuracy = DT::renderDataTable({
        accuracyTable <- data.frame( Sample.Size = c(".1%",".1%",".1%",".5%",".5%",".5%","1%","1%","1%","3%","3%","3%"),
                                     n.gram = c("bigrams", "trigrams", "tetragrams","bigrams", "trigrams", "tetragrams","bigrams", "trigrams", "tetragrams","bigrams", "trigrams", "tetragrams"),
                                     Accuracy.Percentage.EMS = c(25.64, 26.16, 24.72, 44.84, 44.28, 41.60, 55.87, 55.01, 54.93, 60.28, 61.99, 	60.28),
                                     Accuracy.Percentage.HT = c(13.32, 24.36, 24.32, 13.31, 16.32, 38.52, 14.88, 7.22, 4.92, 9.36, 3.57, 3.82),
                                     Accuracy.Percentage.EMS.HT = c(28.44, 26.52, 25.08, 47.16, 46.08, 42.61, 	60.12, 72.60, 71.64, 72.12, 81.36, 80.04)
                                   )
        
        DT::datatable(accuracyTable, options = list(lengthMenu = c(5, 10, 150), pageLength = 15))
    })
    
    output$indicatorEquation <- renderUI({
        withMathJax(helpText("$$acc(w_n, t_n) = 1$$ iff $$w_n = t_n$$ where$$w_n$$ is the predicted word and$$t_n$$ is the true word that was held out in the experiment. 0 otherwise."))
    })
    
    output$perplexityEquation <- renderUI({
        withMathJax(helpText("$$Perplexity = (\\frac{1}{P(w_1, w_2, ..., w_n)})^{1/n}$$ where n is the n-gram level and $$P(w_1, w_2, ..., w_n)=P(w_1)P(w_2|w_1)P(w_3|w_2, w_1), ...,P(w_n|w_{1}^{n-1})$$ is the joint probability distribution of observing word $$w_n$$ after words $$ w_1^{n-1} = w_1, w_2, ..., w_{n-1}$$"))
    })
    
    output$tablePerplexity = DT::renderDataTable({
        perplexityTable <- data.frame( Sample.Size = c(".1%",".5%","1%","3%",".1%",".5%","1%","3%",".1%",".5%","1%","3%"),
                                       n.gram = c("bigrams","bigrams","bigrams","bigrams","trigrams","trigrams","trigrams","trigrams","tetragrams","tetragrams","tetragrams","tetragrams"),   
                                       Accuracy = c(	28.44, 47.16, 	60.12, 72.12, 26.52, 46.08, 72.60, 81.36, 25.08, 42.61, 71.64, 80.04),
                                       mean.Perplexity = c( "19,407.643", "10,926.306", "8,829.629", "7,053.359", "22,929.987", "15,065.242", "	13,094.427", "11,244.204", "27,110.892", "19,839.251", "17,637.174", "	15,753.426"))
    
        DT::datatable(perplexityTable, options = list(lengthMenu = c(5, 10, 150), pageLength = 15))
    })
})
