# All functions for Task 2 and more.

displayNumberOfLinesBarPlot <- function(){
  suppressMessages(library(ggplot2))
  
  dataPath <- "./data/final/en_US/"
  source("readData.R")
  numberLinesBlogs <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.blogs.txt"))
  numberLinesNews <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.news.txt"))
  numberLinesTwitter <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.twitter.txt"))
  
  df <- data.frame( c(numberLinesBlogs, numberLinesNews, numberLinesTwitter))
  df$File <- c("US_Blogs.txt", "US_News.txt", "US_Twitter.txt")
  colnames(df)[1] <- "Counts"
  colors1 <- c("red", "blue", "green")
  
  # print(df)
  
  ggplot(df, aes(x=File, y=Counts)) + geom_bar(stat='identity', fill = colors1) + ylab("Number of Lines") + xlab("File") + ggtitle("Number of Lines for all US Files") 
}


printLengthsOfFilesTable <- function(){
  dataPath <- "./data/final/en_US/"
  source("readData.R")
  numberLinesBlogs <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.blogs.txt"))
  numberLinesNews <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.news.txt"))
  numberLinesTwitter <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.twitter.txt"))
  
  df <- data.frame( c(numberLinesBlogs, numberLinesNews, numberLinesTwitter))
  df$File <- c("US_Blogs.txt", "US_News.txt", "US_Twitter.txt")
  colnames(df)[1] <- "Counts"
  
  # print(df)
  knitr::kable(df, format = "html")
}

displaySizeOfFilesBarPlot <- function(){
  dataPath <- "./data/final/en_US/"
  
  sizeBlogs <- (file.info(paste0(dataPath, "en_US.blogs.txt"))$size)
  sizeNews <- (file.info(paste0(dataPath, "en_US.news.txt"))$size)
  sizeTwitter <- (file.info(paste0(dataPath, "en_US.twitter.txt"))$size)
  
  df <- data.frame( c(sizeBlogs/1024/1024, sizeNews/1024/1024, sizeTwitter/1024/1024))
  df$File <- c("US_Blogs.txt", "US_News.txt", "US_Twitter.txt")
  colnames(df)[1] <- "Size"
  colors1 <- c("red", "blue", "green")
  # print(df)
  
  ggplot(df, aes(x=File, y=Size)) + geom_bar(stat='identity', fill = colors1) + ylab("Size of File in MBs") + xlab("File") + ggtitle("File Sizes for US Files") 
}

displaySizeOfFilesTable <- function(){
  dataPath <- "./data/final/en_US/"
  
  sizeBlogs <- (file.info(paste0(dataPath, "en_US.blogs.txt"))$size)
  sizeNews <- (file.info(paste0(dataPath, "en_US.news.txt"))$size)
  sizeTwitter <- (file.info(paste0(dataPath, "en_US.twitter.txt"))$size)
  
  df <- data.frame( c(sizeBlogs/1024/1024, sizeNews/1024/1024, sizeTwitter/1024/1024))
  df$File <- c("US_Blogs.txt", "US_News.txt", "US_Twitter.txt")
  colnames(df)[1] <- "Size in MBs"
  
  # print(df)
  knitr::kable(df, format = "html")
}  

displayLineStatisticsForFiles <- function(){
  fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")
  fullDataUSBlogs <- readFullFile("./data/final/en_US/en_US.blogs.txt")
  fullDataUSNews <- readFullFile("./data/final/en_US/en_US.news.txt")
  
  df <- data.frame( File = character(), Maximum.Number.Of.Characters = numeric(), Total.Number.Of.Words = numeric()) 
  
  nchars <- lapply(fullDataUSTwitter, nchar)
  maxchars <- which.max(nchars)
  wordCount <- sum(sapply(strsplit(fullDataUSTwitter, "\\s+"), length))
  
  df <- rbind(df, c("en_US.twitter.txt", maxchars, wordCount) )
  
  nchars <- lapply(fullDataUSBlogs, nchar)
  maxchars <- which.max(nchars)
  wordCount <- sum(sapply(strsplit(fullDataUSBlogs, "\\s+"), length))
  
  df <- rbind(df, c("en_US.blogs.txt", maxchars, wordCount) )
  
  nchars <- lapply(fullDataUSNews, nchar)
  maxchars <- which.max(nchars)
  wordCount <- sum(sapply(strsplit(fullDataUSNews, "\\s+"), length))
  
  df <- rbind(df, c("en_US.news.txt", maxchars, wordCount) )
  
  colnames(df) <- c("File", "Maximum.Number.Of.Words.In.A.Line", "Total.Number.Of.Words")
  
  print(df)
  knitr::kable(df, format = "html")
}

## These functions are meant to be used for any dataset

createCorpus <- function(textData_in_Memory){
  corpora <- paste(textData_in_Memory, collapse=" ")
  corpora <- VectorSource(corpora)
  corpora <- Corpus(corpora)
  
  return(corpora)
}

cleanCorpus <- function(rawCorpus) {
  
  rawCorpus <- tm_map(rawCorpus, content_transformer(tolower))
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  rawCorpus <- tm_map(rawCorpus, toSpace, "/|@|//|$|:|:)|*|&|!|?|_|-|#|")
  
  rawCorpus <- tm_map(rawCorpus, removeNumbers)
  rawCorpus <- tm_map(rawCorpus, removeWords, stopwords("english"))
  rawCorpus <- tm_map(rawCorpus, removePunctuation)
  rawCorpus <- tm_map(rawCorpus, stripWhitespace)
  
  source("badwords.R")
  rawCorpus <- tm_map(rawCorpus, removeWords, VectorSource(badwords))
  
  #rawCorpus <- tm_map(rawCorpus, stemDocument)
  
  return (rawCorpus)
}

## Obtain all frequencies for all words in the Corpus

calculateFrequencyOfWords <- function (theCorpus) {
  csparse <- DocumentTermMatrix(theCorpus)
  cmatrix <- as.matrix(csparse)   
  
  wordFrequencies <- colSums(cmatrix)
  wordFrequencies <- as.data.frame(sort(wordFrequencies, decreasing=TRUE))
  wordFrequencies$word <- rownames(wordFrequencies)
  colnames(wordFrequencies) <- c("Frequency", "Word")
  
  return (wordFrequencies)
}

### Calculate the unigrams, bigrams and trigrams using this generalized function version for an n-gram
getN_grams <- function(textData_in_Memory, numberOfGrams){
  theTokens<- tokens(textData_in_Memory,what ="word", remove_numbers = TRUE, remove_punct = TRUE, 
                     remove_separators = TRUE, remove_symbols = TRUE )
  theTokens <- tokens_tolower(theTokens)
  theTokens <- tokens_select(theTokens, stopwords(), selection ="remove") 
  
  unigram <- tokens_ngrams(theTokens, n = numberOfGrams)  
  unigram.dfm <- dfm(unigram, tolower =TRUE, remove = stopwords("english"), remove_punct = TRUE)   
  
  return( unigram.dfm )
}

### Calculate Coverage. You receive a corpus and in turn the function obtains how many words are needed to cover a given percentage of the full corpus.
getCoverage <- function(theCorpus, percentage){
  
  
  allWordFrequencies <- calculateFrequencyOfWords(theCorpus)
  totalNumberOfWords <- sum(allWordFrequencies$Frequency)
  currentPercentage <- 0
  counter <- 1
  numberOfWords <- 0
  
  while(currentPercentage < percentage){
    numberOfWords <- numberOfWords + allWordFrequencies$Frequency[counter]
    currentPercentage <- numberOfWords / totalNumberOfWords
    
    counter <- counter + 1
  }
  return(counter)
}


runExploratoryAnalisis <- function(){
  library(tm)
  library(ggplot2)
  library(wordcloud)
  library(quanteda)
  source("readData.R")
  options(warn=-1)
  
  ### Read all files at once
  fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")
  fullDataUSBlogs <- readFullFile("./data/final/en_US/en_US.blogs.txt")
  fullDataUSNews <- readFullFile("./data/final/en_US/en_US.news.txt")
  
  ### Sample the Data
  set.seed(13)
  sampleSizePercentage <- .0000125    # For each file we sample the given percentage of lines (from 0 to 1, e.g. .6 is sixty percent). 
  
  sampledDataUSTwitter <- sample(fullDataUSTwitter, round(sampleSizePercentage*length(fullDataUSTwitter)), replace = F)
  sampledDataUSBlogs <- sample(fullDataUSBlogs, round(sampleSizePercentage*length(fullDataUSBlogs)), replace = F)
  sampledDataUSNews <- sample(fullDataUSNews, round(sampleSizePercentage*length(fullDataUSNews)), replace = F) 
  
  ### Optional, we may as well use any sampled subset (sampledDataUSTwitter, sampledDataUSBlogs, sampledDataUSNews)
  ### This decision has to take into consideration other factors to be investigated outside the raw Data. e.g. Expert opinion.
  sampledDataTwitterBlogsNews <- c(sampledDataUSTwitter, sampledDataUSBlogs, sampledDataUSNews)
  
  #print(length(sampledDataUSTwitter))
  #print(length(sampledDataUSBlogs))
  #print(length(sampledDataUSNews))
  #print(length(sampledDataTwitterBlogsNews))
  #print(head(sampledDataTwitterBlogsNews))
  #print(tail(sampledDataTwitterBlogsNews))
  
  ### Replace here other sources of data. e.g. (sampledDataUSTwitter, sampledDataUSBlogs, sampledDataUSNews)
  textData <- sampledDataTwitterBlogsNews
  
  USCorpus <- createCorpus(textData)
  #print("**********Corpus")
  #print(strwrap(USCorpus))
  
  USCorpus <- cleanCorpus(USCorpus)
  #print("***************Corpus")
  #print(strwrap(USCorpus))
  
  allWordFrequencies <- calculateFrequencyOfWords(USCorpus)
  top25Words <- allWordFrequencies[1:25,]
  #print(top25Words)
  
  ### Plot results of word frequencies
  p <- ggplot(data=top25Words, aes(x=reorder(Word,Frequency), y=Frequency,
               fill=factor(reorder(Word,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 Words: US Data") +theme(legend.title=element_blank()) + coord_flip()
  
  ### Show a Word Cloud 
  wordcloud(allWordFrequencies$Word[1:100], allWordFrequencies$Frequency[1:100], colors=brewer.pal(8, "Dark2"))
  
  
  numberOfWords = 25
  #Obtain the top N Unigrams
  unigrams <- getN_grams( textData, 1) 
  unigramsDf <- stack(topfeatures(unigrams, numberOfWords))
  unigramsDf <- setNames(stack(topfeatures(unigrams, numberOfWords))[2:1], c('Words','Frequency'))
    
  p <- ggplot(data=unigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                   fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 UNIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
  
  #Obtain the Bigrams
  bigrams <- getN_grams( textData, 2) 
  bigramsDf <- stack(topfeatures(bigrams, numberOfWords))
  bigramsDf <- setNames(stack(topfeatures(bigrams, numberOfWords))[2:1], c('Words','Frequency'))
  
  p <- ggplot(data=bigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                   fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 BIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
  
  #Obtain the Trigrams
  trigrams <- getN_grams( textData, 3) 
  trigramsDf <- stack(topfeatures(trigrams, numberOfWords))
  trigramsDf <- setNames(stack(topfeatures(trigrams, numberOfWords))[2:1], c('Words','Frequency'))
  
  p <- ggplot(data=trigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                  fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 TRIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
  
  #Obtain coverage for any given percentage
  percents <- seq(from = .05, to =.95, by = .05)
  wordCovers <- c()
  
  for (i in 1:length(percents)){
    wordCovers <- c(wordCovers, getCoverage(USCorpus, percents[i]))
  }
  
  q <- qplot(percents,wordCovers, geom=c("line","point")) + geom_text(aes(label=wordCovers), hjust=1.35, vjust=-0.1)+ xlab("Percentage") + ylab("Times that Words Appear") + labs(title = paste( "Coverage for ", sum(allWordFrequencies$Frequency), "Words")) 
  q +scale_x_continuous(breaks=percents)
}



