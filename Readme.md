# NLP Application: SwiftKey Data

## Data Science Capstone
## Johns Hopkins University

This repository contains all code and products that are necessary to build a predictive application for natural language processing. We will demonstrate the application of several data science tasks to a dataset from the SwiftKey company. We include all scripts that were used to obtain, pre-process, analize and make predictions from the Data. 

Please refer to all R scripts in the webpage [https://github.com/corderoai/JHU_DataScienceCapstone](https://github.com/corderoai/JHU_DataScienceCapstone).

The Milestone Report is available at:
[https://rpubs.com/jcoml/648976](https://rpubs.com/jcoml/648976)


# Journal of Tasks

### Code to load r scripts

`source()`

## Task 0 - Understanding the problem

The first step in analyzing any new data set is figuring out: 
(a) what data you have and (b) what are the standard tools and models used for that type of data. Make sure you have downloaded the data from Coursera before heading for the exercises. This exercise uses the files named LOCALE.blogs.txt where LOCALE is the each of the four locales en_US, de_DE, ru_RU and fi_FI. The data is from a corpus called HC Corpora. See the About the Corpora reading for more details. The files have been language filtered but may still contain some foreign text.

### Packages for NLP

[tokenizers](https://cran.r-project.org/web/packages/tokenizers/vignettes/introduction-to-tokenizers.html)

[readr](https://readr.tidyverse.org/)

[tm](https://cran.r-project.org/web/packages/tm/tm.pdf)

[]()

[]()

```
# We obtained the Data from the web.

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
```
Please refer to `the downloadFiles.R` script.

## Task 1 - Getting and cleaning the data
Large databases comprising of text in a target language are commonly used when generating language models for various purposes. In this exercise, you will use the English database but may consider three other databases in German, Russian and Finnish.

The goal of this task is to get familiar with the databases and do the necessary cleaning. After this exercise, you should understand what real data looks like and how much effort you need to put into cleaning the data. When you commence on developing a new language, the first thing is to understand the language and its peculiarities with respect to your target. You can learn to read, speak and write the language. Alternatively, you can study data and learn from existing information about the language through literature and the internet. At the very least, you need to understand how the language is written: writing script, existing input methods, some phonetic knowledge, etc.

Note that the data contain words of offensive and profane meaning. They are left there intentionally to highlight the fact that the developer has to work on them.

Tasks to accomplish

Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a file as input and returns a tokenized version of it.

Profanity filtering - removing profanity and other words you do not want to predict.

### Some checks

- Length of file en_US.blogs.txt:

`theSize <- (file.info("./data/final/en_US/en_US.blogs.txt")$size)`
`print(theSize/1024/1024)`

- Number of lines in en_US.twitter.txt:

We read the file in chuncks to have a faster task...

`connection <- file("./data/final/en_US/en_US.twitter.txt", "rb")`
```
numberOfLines <- 0L

while (length(chunk <- readBin(connection, "raw", 65536)) > 0) 
{
    numberOfLines <- numberOfLines + sum(chunk == as.raw(10L))
}
close(connection)
numberOfLines
```
- Longest length of the line seen in any of three en_US files:

```
readTotalNumberOfLinesInFile <- function(inFile)
{
  totalLines = 0
  
  connection <- file(inFile, "r")
  totalData <- readLines(connection, encoding = "UTF-8", skipNul = TRUE)
  close(connection)
  
  totalLines <- summary(nchar(totalData))[6]
  
  return(totalLines)  
}

readTotalNumberOfLinesInFile("./data/final/en_US/en_US.blogs.txt")
readTotalNumberOfLinesInFile("./data/final/en_US/en_US.news.txt")
readTotalNumberOfLinesInFile("./data/final/en_US/en_US.twitter.txt")
```

- In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?

```
fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")

# class(fullDataUSTwitter)
# str(fullDataUSTwitter)
# Notice how fullDataUSTwitter is just a vector of character types(each line is an element of the vector being a string of characters including all words in a line)

rowsOfLove <- grep("love", fullDataUSTwitter)
rowsOfHate <- grep("hate", fullDataUSTwitter) 
loveHateRatio <- length(rowsOfLove)/length(rowsOfHate)
loveHateRatio 
```

- The one tweet in the en_US twitter data set that matches the word "biostats" says what?

```
lineNumber <- grep("biostats", fullDataUSTwitter)
fullDataUSTwitter[lineNumber]

# Or just:

lineOfCharacters <- grep("biostats", fullDataUSTwitter, value = T)
lineOfCharacters
```

- How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)

```
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", fullDataUSTwitter))
```
We could have done all the previous operations by reading the Data line by line. The advantage of such approach would be low main memory consumption but at the gruesome cost of higher elapsed time.

Please refer to the `readData.R` script.

## Task 2 - Exploratory Data Analysis

The first step in building a predictive model for text is understanding the distribution and relationship between the words, tokens, and phrases in the text. The goal of this task is to understand the basic relationships you observe in the data and prepare to build your first linguistic models.

Tasks to accomplish

Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.
Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.

### Initial Review

Initially, we need to consider the dimmensions between all dataset files we have:

```
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
  
  print(df)
  
  ggplot(df, aes(x=File, y=Counts)) + geom_bar(stat='identity', fill = colors1) + ylab("Number of Lines") + xlab("File") + ggtitle("Number of Lines for all US Files") 
}

displayNumberOfLinesBarPlot()
```

We observe that the twitter file is the largest:

```
printLengthsOfFilesTable <- function(){
  dataPath <- "./data/final/en_US/"
  source("readData.R")
  numberLinesBlogs <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.blogs.txt"))
  numberLinesNews <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.news.txt"))
  numberLinesTwitter <- readTotalNumberOfLinesInFile(paste0(dataPath, "en_US.twitter.txt"))
  
  df <- data.frame( c(numberLinesBlogs, numberLinesNews, numberLinesTwitter))
  df$File <- c("US_Blogs.txt", "US_News.txt", "US_Twitter.txt")
  
  # print(df)
  knitr::kable(df, col.names = gsub("[.]", " ", names(df)))
}

printLengthsOfFilesTable ()
```

It is necessary to review the size of each file in Mbs as well to have an impression of the density of lines in each file:

```
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

displaySizeOfFilesBarPlot()

displaySizeOfFilesTable <- function(){
  dataPath <- "./data/final/en_US/"
  
  sizeBlogs <- (file.info(paste0(dataPath, "en_US.blogs.txt"))$size)
  sizeNews <- (file.info(paste0(dataPath, "en_US.news.txt"))$size)
  sizeTwitter <- (file.info(paste0(dataPath, "en_US.twitter.txt"))$size)
  
  df <- data.frame( c(sizeBlogs/1024/1024, sizeNews/1024/1024, sizeTwitter/1024/1024))
  df$File <- c("US_Blogs.txt", "US_News.txt", "US_Twitter.txt")
  colnames(df)[1] <- "Size in MBs"
  
  # print(df)
   knitr::kable(df, col.names = gsub("[.]", " ", names(df)))
} 

displaySizeOfFilesTable()
```
Interestingly enough we can see that the blogs and news files are larger in Byte size than the twitter ones. This is logical since twitter has a fixed length of characters per tweet whereas in the other cases the length of the posts is open for the writer. 

In order to check the later assumption we will find some relevant statistics regarding the lines in the files:

```
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
  
  #print(df)
  knitr::kable(df, col.names = gsub("[.]", " ", names(df)))
}

displayLineStatisticsForFiles()
```

Notice how the maximum number of words in a line in the twitter file was only 26. Which sounds completely logic given the maximum character restriction.


### Cleaning Up the Data

We form the Corpus and remove all unnecessary features from Data. We also sample the Data in order to have a faster analisis, however we may as well include all contents with all lines for the analysis. We also apply a censorship function in order to remove all bad words, punctuations or other features we decided irrelvant for this project. The list of obscene words was taken from the repository [https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en](https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en).

The following function make it possible:

```
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
```

### Questions to consider

1. Some words are more frequent than others - what are the distributions of word frequencies?

We consider the top 25 words that we found over a sampled subset of the original Data from merging the three datasets. We set a variable to extract only a percentage of lines from the original file (trying to practice a sampling task, however we may as well consider all data for exploratory analisis since no performance issues are critical in this case). We then show a plot containing the frequencies of most used words.

```
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

options(warn=-1)
library(tm)
library(wordcloud)
library(ggplot2)
source("readData.R")

### Read all files at once
  fullDataUSTwitter <- readFullFile("./data/final/en_US/en_US.twitter.txt")
  fullDataUSBlogs <- readFullFile("./data/final/en_US/en_US.blogs.txt")
  fullDataUSNews <- readFullFile("./data/final/en_US/en_US.news.txt")
  
  ### Sample the Data
  set.seed(13)
  sampleSizePercentage <- .00000625    # For each file we sample the given percentage of lines (from 0 to 1, e.g. .6 is sixty percent). 
  
  sampledDataUSTwitter <- sample(fullDataUSTwitter, round(sampleSizePercentage*length(fullDataUSTwitter)), replace = F)
  sampledDataUSBlogs <- sample(fullDataUSBlogs, round(sampleSizePercentage*length(fullDataUSBlogs)), replace = F)
  sampledDataUSNews <- sample(fullDataUSNews, round(sampleSizePercentage*length(fullDataUSNews)), replace = F) 
  
  ### Optional, we may as well use any sampled subset (sampledDataUSTwitter, sampledDataUSBlogs, sampledDataUSNews)
  ### This decision has to take into consideration other factors to be investigated outside the raw Data. e.g. Expert opinion.
  sampledDataTwitterBlogsNews <- c(sampledDataUSTwitter, sampledDataUSBlogs, sampledDataUSNews)
  
  textData <- sampledDataTwitterBlogsNews
  
  USCorpus <- createCorpus(textData)
  USCorpus <- cleanCorpus(USCorpus)
  
  allWordFrequencies <- calculateFrequencyOfWords(USCorpus)
  top25Words <- allWordFrequencies[1:25,]
  
  #Plot results of word frequencies
  p <- ggplot(data=top25Words, aes(x=reorder(Word,Frequency), y=Frequency,
               fill=factor(reorder(Word,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 Words: US Data") +theme(legend.title=element_blank()) + coord_flip()
```

We present the results in a graphic manner as a word cloud with the top 100 most used words:

```
# Show a word Cloud for the 100 most used words

wordcloud(allWordFrequencies$Word[1:100], allWordFrequencies$Frequency[1:100], colors=brewer.pal(8, "Dark2"))
```


2. What are the frequencies of 2-grams and 3-grams in the dataset?

According to [https://en.wikipedia.org/wiki/N-gram](https://en.wikipedia.org/wiki/N-gram) an n-gram is a contiguous sequence of n items from a given sample of text or speech. The items can be phonemes, syllables, letters, words or base pairs according to the application. The n-grams typically are collected from a text or speech corpus. When the items are words, n-grams may also be called shingles. Using Latin numerical prefixes, an n-gram of size 1 is referred to as a "unigram"; size 2 is a "bigram" (or, less commonly, a "digram"); size 3 is a "trigram".

For the effects of this project we calculate the frequencies of the n-grams the following way (the unigrams are the same as just word frequencies, we choose the top 25):

```
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

library(quanteda)

numberOfWords = 25
  #Obtain the top N Unigrams
  unigrams <- getN_grams( textData, 1) 
  unigramsDf <- stack(topfeatures(unigrams, numberOfWords))
  unigramsDf <- setNames(stack(topfeatures(unigrams, numberOfWords))[2:1], c('Words','Frequency'))
    
  p <- ggplot(data=unigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                   fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 UNIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
```

Now, we count the top 25 bigrams:

```
#Obtain the Bigrams
  bigrams <- getN_grams( textData, 2) 
  bigramsDf <- stack(topfeatures(bigrams, numberOfWords))
  bigramsDf <- setNames(stack(topfeatures(bigrams, numberOfWords))[2:1], c('Words','Frequency'))
  
  p <- ggplot(data=bigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                   fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 BIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
```

Finally we obtain the frequencies for the trigrams:

```
#Obtain the Trigrams
  trigrams <- getN_grams( textData, 3) 
  trigramsDf <- stack(topfeatures(trigrams, numberOfWords))
  trigramsDf <- setNames(stack(topfeatures(trigrams, numberOfWords))[2:1], c('Words','Frequency'))
  
  p <- ggplot(data=trigramsDf, aes(x=reorder(Words,Frequency), y=Frequency,
                                  fill=factor(reorder(Words,-Frequency))))+ geom_bar(stat="identity") 
  p + xlab("Word") +labs(title = "Top 25 TRIGRAMS: US Data") +theme(legend.title=element_blank()) + coord_flip()
```


3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?

In the following plot we can obtain the coverage for 50% and 90% by checking the x axis (for completeness we show all other percentages):

```
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
    
    return(counter)
  }
}  

#Obtain coverage for any given percentage
  percents <- seq(from = .05, to =.95, by = .05)
  wordCovers <- c()
  
  for (i in 1:length(percents)){
    wordCovers <- c(wordCovers, getCoverage(USCorpus, percents[i]))
  }
  
  q <- qplot(percents,wordCovers, geom=c("line","point")) + geom_text(aes(label=wordCovers), hjust=1.35, vjust=-0.1)+ xlab("Percentage") + ylab("Times that Words Appear") + labs(title = paste( "Coverage for ", sum(allWordFrequencies$Frequency), "Words")) 
  q +scale_x_continuous(breaks=percents)
```

4. How do you evaluate how many of the words come from foreign languages?

As a first approach in order to evaluate words that come from foreign languages, we may access a dictionary or idiom list for that language, then we could either remove them from the corpus or identify them as foreign words.

5. Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

We may use another machine learning method to learn the vocabulary and habits of the user in order to offer a more personalized prediction, and thus, increasing coverage. The coverage of other spatio temporal features such as location: We may also use location services in order to recommend nearby places or other features that might be of interest. Finally, we may learn the relationship between stopwords and other nouns and verbs that might be correlated.


Please refer to the `exploratoryAnalisis.R` script in this repo.

## Task 3 - Modeling

The goal here is to build your first simple model for the relationship between words. This is the first step in building a predictive text mining application. You will explore simple models and discover more complicated modeling techniques.

### Tasks to accomplish

**A. Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.**

There are three major steps in building the n-gram model.

1. Loading and sampling the Data.

2. Extracting the n-grams from the sampled Data and the entered word(s).

3. Making predictions of the words in the last level of a given n-gram by taking the `n-1` n-gram as the pivot.

We assume that the entered sentence has at most 3 words. However, for cases where we encounter longer sentences we decided to take into consideration only the last 3 words. We believe those ending words correspond to better predictors of the fourth word, although further linguistic argumentation is needed.

We assume that the best prediction is given by the most restrictive combination of entered words. The largest n-gram of entered words narrows down the number of possibilities for predicted words. Thus, we should aim to predict the following token by taking advantage of this feature.

The algorithm accepts the word(s), then tokenizes the sentence into lexems. We aim to predict the next word by firstly taking into consideration the largest n-gram, then the (n-1)-gram down to the (n-2)-gram. For each case we match the entered n-gram with those ones that were extracted from the Corpus. We select and output the top 3 words with the most frequencies. At the end, we return a list of at most 3 top word suggestions for each n-gram case. Therefore, an entered n-gram will obtain a n sized list of lists of top frequency suggestions.

Lower ranked n-grams (2-grams, unigrams) also obtain their suggestions in the same fashion. In the case that no n-gram was matched against the mined tokens a string "@@@" is returned. That delimiter lexem can help us to signal other procedures to make suggestions based in unobserved n-grams in the Corpus. As a simple example: The trigram "I will live" will be tokenized and matched against the extracted Corpus n-grams and a possible list of suggestion would look like "((x3_1, x3_2, x3_3), (y2_1, y2_2, y2_3), (z1_1, z1_2, z1_3))", in this case x3_1 is the suggested "first" top word (with highest frequency from the trigram in the corpus) from the trigram "I will live"". x3_3 is the third top word suggestion by using the trigram as the predictor. y2_2 is the second top word suggestion from the bigram "I will". y2_1 is the top word suggestion for the same bigram. z1_3 is the third top suggestion (third highest frequency in corpus) from the unigram "I".

In the case that lower level n-grams are entered the same procedure applies.

An entered n-gram that does not match some or all of its sub (n-k)grams against the Corpus receives a prediction of a special token "@@@" for that specific unobserved (n-k)gram. For example the entered words "Her name" matched the unigram "name" at least three times in the Corpus but did not match the "her-name" bigram. In this case the algorithm will suggest the following list of lists: (("@@@"), (x1_1, x1_2, x1_3)).


```
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
    
  ### 4. A) Make predictions using ONLY observed n-grams. ONLINE
  
    # LOAD the ngrams data frames! Faster, do not calculate them again!  
    #load("./data/ngrams.df.RData",  verbose = TRUE)
  start_time <- Sys.time()  
  
  predictNextWordWithObservedNGrams( "right", bigrams.df, trigrams.df, tetragrams.df) 

  end_time <- Sys.time()
  getElapsedTimeMessage("Predicting with Simplest Model Observed ngrams", start_time, end_time)


```

**B. Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.**

The previous algorithm is based only in counts. We may need to normalize it to convert the frequencies into probabilities. A good reference for constructing n-gram predictor models can be found at [https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf](https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf).

Before we proceed to elaborate a more professional algorithm. We need to decide how much Data are we going to cover. The longer the sample size the higher the frequencies will get for more n-grams. Furthermore, a large amount of unigrams might be covered. It would be interesting to concentrate on setting different sample sizes and choose the smallest number of words that covers the highest amount of the Corpus. However, we will concentrate on 3 sample sizes and we will evaluate all algorithms in these datasets.

We define the parameter `Omega` to be the actual sample size in which the experiments will be carried out. We will construct three datasets with `Omega = 0.1%, 0.5%, 1%,` and `3%` of all Data in the Corpus.

In order to select different dataset sample instances we use the following code (in the processNGrams.R script):

```
processNGrams <- function(){
  suppressMessages(source("readData.R"))
  options(warn=-1)
  
  set.seed(13)
  sampleSizePercentage <- c(0.001, 0.005, 0.01) 
  
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
```

As stated in page 3 of [https://web.stanford.edu/~jurafsky/slp3/3.pdf](https://web.stanford.edu/~jurafsky/slp3/3.pdf): "we can not just estimate by counting the number of times every word occurs following every long string, because language is creative and any particular context might have never occurred before!" This Assumption is the core to devise a better way for predicting words given a particular set of n-grams. The frequency counts do not help us to estimate the probabilities of unseen words without making more assumptions.

In order to predict words that were not previously observed in the Corpus we follow the next algorithm:

**Historical Track Algorithm (HT)**

-Assume a given Word w_{j+1} is more likely to appear if and only if its likelihood is maximum when accompained with words w_{1}^{j-1} that preceeded it (have been previously observed).

a. Receive vector of predictions, if vector is empty or it has less than three suggestions:

  a.1. Loop for the number of remaining predictions and predict the word w_{j+1} given the sequence w_{1}^{j}:
  
    a.1.1. Using the chain rule we reduce the joint probability distribution P(w_{n},w_{1}^{n-1}) = P(w_{1})P(w_{2}|w_{1})P(w_{3}|w_{1}^{2})...P(w_{n}|w_{1}^{n-1}). Furthermore, we apply the bigram model to compact even more the joint chained probability distribution into bigram sequences: P(w_{n},w_{1}^{n-1}) = P(w_{1})P(w_{2}|w_{1})...P(w_{n}|w_{n-1}).  
    
    a.1.2. Look for the bigrams that partially match w_{1}^{j}.
    
    a.1.3. Obtain the probabilities for those matched sequences according to the chain rule.
    
    a.1.4. For the factors in the chain rule that involve the unobserved words in the prediction (variable on the left part of the conditional distribution): Substitute that probability with an uniform distribution (acccording to the total number of bigrams). If the unobserved word matches the predictor (conditional right part) then naively set the probability P(w_{j+1}|w_{j}) = P(w_{j+1}).
    
  a.2. Obtain the w_{j+1} to be predicted by choosing the bigrams with highest Probability from the previously computed set.
  
  a.3. In case that no word was found in the unigrams then choose a random word with uniform distribution from the top 25 most frequen words. 
  
  a.4. Add that word to the prediction vector.
  
b. Return the prediction vector.

### Relevant Performance Debuggers:

`object.size()`: this function reports the number of bytes that an R object occupies in memory

`Rprof()`: this function runs the profiler in R that can be used to determine where bottlenecks in your function may exist. The profr package (available on CRAN) provides some additional tools for visualizing and summarizing profiling data.

`gc()`: this function runs the garbage collector to retrieve unused RAM for R. In the process it tells you how much memory is currently being used by R.

Questions considered:

**1. How can you efficiently store an n-gram model (think Markov Chains)?**

Markov Chains follow the basic principle: The future is independent of the past given the present. Specifically according to [1](https://en.wikipedia.org/wiki/Markov_chain) it is a sequence of possible events in which the probability of each event depends only on the state attained in the previous event.

In the case of this application, we may use a markov chains as the modeling tool to encapsulate the syntax of sentences. Markov chains can be thought as a way to define a state transition model (a graph whose nodes are words or states and edges or transitions are the connections to other states or words).

It is extremely easy to visualize how we can translate n-grams to a markov chain. Each initial word A in an n-gram  represents a given state in the chain and then, given the possible following words B we can calculate a probability of transitioning from A to B. In other words, A can transition to N possible B words. We just need to count the frequencies of transitioning from A to a given Bx word and dividing it by the total number of transition frequencies in Data.

In order to predict a word we need at least a 2-gram (since an input word works as a pivot or root node).

An ngram is efficiently represented as a tree structure, where an initial word is the root and the subsequent words represent transitions to words in branches and leafs. Levels in the three are the levels in the n-grams. In terms of memory this may be expensive but in time complexity very fast (lineal time because n words can only transition to another set of m words independently of previous ones). We will initially consider those transitions that have been observed from Data sequences in sentences.  

**First Outline:**

We can start by calculating unigrams, bigrams, etc. From a sample size as big as possible (in an offline manner).  A prediction algorithm will be trainned with these Data.

Then, once the application is loaded, we wait until a given word(s) is entered `W`. The entered word or words are matched against the previously calculated n-grams with a level as long as the number of words in the text. 

Finally the prediction algorithm will recommend the top `beta` words that have the highest frequencies in the final level in the n-gram.


**2. How can you use the knowledge about word frequencies to make your model smaller and more efficient?**

The algorithm is composed of two phases an offline and online modules. We can take advantage of previously computed n-grams to reduce the search space at the time of suggesting a new word in an online manner.Thus, we will be performing searches in a data structure of n-grams.


**3. How many parameters do you need (i.e. how big is n in your n-gram model)?**

Whenever we detect a new word `W` we proceed to search for its n-grams in the data structure.

We need a parameter `ng` to define the number of n-grams to be retieved. We may need another paramenter `lng` to define the size of the top n-grams to be searched online that were not observed in the Data.

**4. Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?**

By reducing the sample size of the top `ng` n-grams we may assign a uniform distribution to those most `lng` n-grams that were not observed.

**5. How do you evaluate whether your model is any good?**

By dividing the Data in training, validation and test sets. We compute residual errors over predicted words of the final level in the n-grams.

We expect to gain more accuracy by increasing the sample size.

**6. How can you use backoff models to estimate the probability of unobserved n-grams?**

The approach mentioned above assigns an uniform distribution to those top `lng` n-grams that have not been fully observed. However we may explore those words that end up in the final `n-1` n-gram level with the highest frequencies. If this is not possible we go back to `n-2` n-grams and repeat the same steps. Finally, we may end up with the unigrams with the highest frequencies in the Data.

A smart way would be to also perform online-learning over the history of sentences entered by the user and use that as another Data source to calculate n-grams.

For the case of unigrams any speculative algorithm or other sources of Data can be used. However, this search is likely to be heuristic.

Please refer to the `learning.R` script for more details.

**Note About Processing Input words:**

Before we elaborate more on the algorithms we process the entered text and tokenize it with the following function:

```
#Receives a vector of characters and outputs a dataframe with its processed tokens.
cleanInputNGram <- function(inputNGram){
  
  inputNGram <- gsub("_", "", inputNGram)
  queryTokens <- tokenizeAndcleanCorpus(enteredWords)
  n <- as.numeric(ntoken(queryTokens[[1]][1]))
  predictorTokens <- tail(queryTokens[[1]], 3)
  predictorNGram <- paste(tail(queryTokens[[1]], 3), collapse = "_")
  df <- data.frame(Words = predictorNGram, size = n)
  df$word1 <- as.character(predictorTokens[3])
  
  if(n == 2){
    df$word2 <- as.character(predictorTokens[2])
  }
  else if(n >= 3){
    df$word2 <- as.character(predictorTokens[2])
    df$word3 <- as.character(predictorTokens[1])
  }
  
  return( df )
}
```

##Task 4 - Prediction Model

The goal of this exercise is to build and evaluate your first predictive model. You will use the n-gram and backoff models you built in previous tasks to build and evaluate your predictive model. The goal is to make the model efficient and accurate.

###Tasks to accomplish

Build a predictive model based on the previous data modeling steps - you may combine the models in any way you think is appropriate.

**Note: Please refer to the `nGramPredictionV1.R` script for details of the functions to be mentioned below.**

1. Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of your model.

a. We load the full data into memory (which is about 556 MBs), clean the Data by using the `quanteda` package (removing punctuation, special characters, bad words, etc.) and tokenize all lexems. Then, we find the unigrams, bigrams, trigrams and tetragrams. For clarity, portability and speed of execution we store those n-grams into a `.RData` file as dataframes along with its respective frequencies and probabilities (e.g. Probability(word_2 | word_1) = Counts(word_1_word_2)/Counts(word_1)).

For simplicity, the algorithms use all these n-grams in the datagrams. The portability of this approach is very important, we only access the full data once per sample size experiment. The idea is to load this data in the server side of a shiny app, this way we do not need to sample or access the full Data whenever the user enters the app. 

b. **Space Complexity:** The datagrams for a given sample size are stored in a single file. This file has information related to the frequencies and probabilities. In specific terms the space complexity is linear `O(k*S*M)` where `k` is the constant number of columns for the dataframes, `M` is the full size of the Corpus and `0<s<1` is the sample size. The following table presents a summary of the different obtained files:

| Sample Size |  Size in MegaBytes |# of unigrams| # of bigrams | # of trigrams | # of tetragrams|
|-----|--------|--------|--------|--------|--------|
| .1% | 11 MB | 11,333| 31, 895| 30,208| 27,320|
| .5% | 52 MB | 30,327| 149,946| 150,685| 136,596|
| 1%  | 104 MB| 45,557| 288,100| 303,569| 276,178|
| 3%  | 297 MB | 85,275| 776,006| 894,046| 818,433|

c **Time Complexity:** The idea of using a reduced sample size and a dataframe set structure simplifies computations significatively. The time complexity is in the worst case scenario is in the order of `O(r*c)` where `r` is the maximum number of rows in a dataframe and `c` is the maximum length of a given n-gram.

- In order to measure elapsed times we use the following functions:

```
start_time <- Sys.time()

### CODE TO BE TESTED

end_time <- Sys.time()
  getElapsedTimeMessage("PREDICTION. ONLINE", start_time, end_time)
  
getElapsedTimeMessage <- function(message, st, et){
  print("**************************************************************")
  print(paste("Elapsed Time in Seconds for:", message, difftime(et, st, unit = "secs") ))
  print("**************************************************************")
  print("*")
}  
```

- The amount of time required to load the Data accross different table sizes and query time (predicting a word given an initial entered sentence) is shown in the following table:

| Sample Size |  Time to Load Full Data into Memory | Time per Single Query |
|-----|--------|--------|
| .1% | 4.5 seconds | 0.041 seconds|
| .5% | 4.7 seconds | 0.049 seconds|
| 1% | 5.01 seconds | 0.053 seconds|
| 3% | 5.07 seconds | 0.085 seconds|

**Note that "Time to Load Full Data into Memory" is run only once when the user enters the shiny app. The contents of the n-grams are stored into the server's memory.**

2. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.

According to [https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf](https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf) there are two major ways we can test a n-gram prediction algorithm: By using extrinsic or intrinsic evaluations. Extrinsic evaluation requires to test for specific accuracies over inputed sentences. Intrinsic evaluation uses specific polinomial formulae such as perplexity.

a. **Perplexity:** The intuition behind this metric is that "the best language model is the one that best predicts an unseen test set". A reference to this metric is found at [https://towardsdatascience.com/perplexity-intuition-and-derivation-105dd481c8f3](https://towardsdatascience.com/perplexity-intuition-and-derivation-105dd481c8f3).

Perplexity is the inverse probability of the test set, normalized by the number of words:

```
PP(W) = P(w1, w2, ..., wN)^(-1/N)
```

where N is the n-gram level n and the joint probability distribution is decomposed by the chain rule:

```
P(w1, w2, ..., wN) = P(w1)P(W2|w1), ..., P(wn|w_{1}^{n-1})

P(W2|w1) = COUNTS(w1_w2) / COUNTS(w1)
P(wn|w_{1}^{n-1}) = COUNTS(w1_w2,...,_wn-1_wn)/COUNTS((w1_w2,...,_wn-1)
```
for cases when the probability is zero (unobserved) we set an extremely low probability 1*10-7 to avoid divisions by zero.

**We can see that minimizing perplexity is equivalent to maximizing probability [https://web.stanford.edu/class/cs124/](https://web.stanford.edu/class/cs124/).**

For performing this computation we need to modify the original nGramPredictionV1 function to incluse in a list of lists the predicted words but also their decomposed probabilities `P(w1)P(w2|w1)...P(wn|w1w2...wn-1)`.

b. **Accuracy of Predictions:** We prepare a test set in which we will hide the last lexem of the sentence, this last word would be our "true" prediction `Y`. We will use the (n-1) test n-grams and stored the predicted words by the model `h`. We just average the amount of accurate answers `Y == h` for any n-gram by the total amount of test instances `t`.

```
Accuracy_N = (1/t)*(Number_Of_Ys==hs)
```

###Questions to consider

1. How does the model perform for different choices of the parameters and size of the model?

**For our case the model do not have internal parameters. We consider to run the experiments by choosing the exact match algorithm, the back off completion algorithm and the history track algorithm by separate and combined for all sample sizes.**

2. How much does the model slow down for the performance you gain?

**The best performance is achieved when the sample size is larger, thus we need to take into consideration the amount of time it takes to predict words for a single query.**


3. Does perplexity correlate with the other measures of accuracy?

**Indeed, they are the inverse.**

4. Can you reduce the size of the model (number of parameters) without reducing performance?

**The best try would be to use the smallest sample size to train the model.**

### Natural language processing I Check

1. For each of the sentence fragments below use your natural language processing algorithm to predict the next word in the sentence.

The guy in front of me just bought a pound of bacon, a bouquet, and a case of

**beer**

2. You're the reason why I smile everyday. Can you follow me please? It would mean the

**world**

3. Hey sunshine, can you follow me and make me the

**happiest**

4. Very early observations on the Bills game: Offense still struggling but the

**defence**

5. Go on a romantic date at the

**beach**

6. Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my

**way**

7. Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some

**time**

8. After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little

**fingers**

9. Be grateful for the good times and keep the faith during the

**bad**

10. If this isn't the cutest thing you've ever seen, then you must be

**insane**

## Task 5 - Creative Exploration

So far you have used basic models to understand and predict words. In this next task, your goal is to use all the resources you have available to you (from the Data Science Specialization, resources on the web, or your own creativity) to improve the predictive accuracy while reducing computational runtime and model complexity (if you can). Be sure to hold out a test set to evaluate the new, more creative models you are building.

Tasks to accomplish

### 1. Explore new models and data to improve your predictive model.

We tested different sample sizes and different seeds to construct the training set.

### 2. Evaluate your new predictions on both accuracy and efficiency.

We will present the final results in next section.

Questions to consider

**1. What are some alternative data sets you could consider using?**

Collecting a dataset of tweets available at different repositories for NLP.

**2. What are ways in which the n-gram model may be inefficient?**

Whenever the test cases contain words that were skipped in the cleaning phase.

**3. What are the most commonly missed n-grams? Can you think of a reason why they would be missed and fix that?**

All connecting words. They are missing because of the highly restrictive cleaning process.

**4. What are some other things that other people have tried to improve their model?**

Try to reduce perplexity by training the model with a larger sample size or a bootstrapped partition.

**5. Can you estimate how uncertain you are about the words you are predicting?**

We can try to measure entropy over the joint probability distribution of the predicted n-gram.


## Experimental Results.

We conducted a series of experiments to test the accuracy of predicting words given any sentence. Specifically, we tried to include sentences including unigrams, bigrams and trigrams (or any other 3>gram). We take a given n-gram and remove the last word, this last word is thus compared with the prediction given by the algorithm by using the (n-1)gram.

The amount of test cases is presented in the following table:

| n-gram |  # of test cases |
|-----|--------|
| bigrams| 3708|
| trigrams| 3435|
| tetragrams| 3148|

Then, we selected the trainned models corresponding to different amounts of sample sizes (.1%, .5%, 1% and 3%). We understand by trainned models the posterior probabilities that were computed after estimating the counts of observed n-grams.

Our general algorithm do not contain continuos parameters but discrete sub algorithms. We ran experiments to test the exact matches algorithm (EMS) that matches input words with observed training n-grams and the historical track algorithm (HT).

In the case of the EMS, we concluded that given the extremely small sample size used to train the algorithms it is very likely that we will not observe the majority of the input test n-grams. Therefore, we decided to include in the EMS the subalgorithm which uses the back-off models (predicting from previos (n-k) starting by the end of the sentence), e.g. the sequente "happy new" (input bigram) will be matched with the two words but also we will look for recommendations for the "new" (input unigram).

For each input sentence we tokenize it and convert it to an n-gram W. We compute the accuracy with an indicator function 1a. We input 1 to the result of the experimental test case if the predicted word is identical to the held out word in the input. We add a 0 if the predicted and initial n-grams differ. We then report the average result by summing up the number of accurate results (1's) and dividing by the total number of test cases for a given n-gram level.


The results for each n-gram tupple is presented in the following table:

### Accuracy

| Sample Size |  n-gram | Accuracy using only EMS(%)| Accuracy using only HT(%)| Accuracy using EMS and HT(%)|
|-------------|---------|------------------------|-----------------------|--------------------------|
| .1%         |  bigrams | 25.64| 13.32| 28.44|
| .1%         |  trigrams| 26.16| 24.36| 26.52|
| .1%         |tetragrams| 24.72| 24.32| 25.08|
| .5%         |  bigrams | 44.84| 13.31| 47.16|
| .5%         |  trigrams| 44.28| 16.32| 46.08|
| .5%         |tetragrams| 41.60| 38.52| 42.61|
|  1%         | bigrams  | 55.87| 14.88| 60.12|
|  1%         | trigrams | 55.01|  7.22| 72.60|
|  1%         |tetragrams| 54.93|  4.92| 71.64|
|  3%         | bigrams  | 60.28|  9.36| 72.12|
|  3%         | trigrams | 61.99|  3.57| 81.36|
|  3%         |tetragrams| 60.28|  3.82| 80.04|


### Perplexity Results

We measure the perplexity for the posterior probabilities computed by the training Data that match the unseen text. We decomposed the Data into the same four sample sizes and computed the mean perplexity for all test cases. The following table shows the results:

| Sample Size |  n-gram | Accuracy | mean(Perplexity) |
|-------------|---------|----------|------------|
| .1%         |  bigrams | 28.44    | 19,407.643|
| .5%         |  bigrams | 47.16    | 10,926.306|
|  1%         |  bigrams | 60.12    | 8,829.629|
|  **3%**         |  **bigrams** | **72.12**|  **7,053.359**|
| .1%         |  trigrams| 26.52    | 22,929.987|
| .5%         |  trigrams| 46.08    | 15,065.242|
|  1%         | trigrams | 72.60    | 13,094.427|
|  **3%**         | **trigrams** | **81.36**| **11,244.204**|
| .1%         |tetragrams| 25.08    | 27,110.892|
| .5%         |tetragrams| 42.61    | 19,839.251|
|  1%         |tetragrams| 71.64    | 17,637.174|
|  **3%**         |**tetragrams**| **80.04**| **15,753.426**|

**The results clearly indicate a trend, whenever we observe a decrease in perplexity we naturally observe an increase in accuracy.** The general approach used to compute the perplexity is given by the following code:

```
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
```

Details regarding the experiments are contained in the script `experiments.R` in the repository. This is a screenshot of the general approach:

```
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
}

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
```

## Natural language processing II Check

1. When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd

**die**

2. Guy at my table’s wife got up to go to the bathroom and I asked about dessert and he started telling me about his

**marital**

3. I’d give anything to see arctic monkeys this

**weekend**

4. Talking to your mom has the same effect as a hug and helps reduce your

**stress**

5. When you were in Holland you were like 1 inch away from me but you hadn’t time to take a

**picture**

6. I’d just like all of these questions answered, a presentation of evidence, and a jury to settle the

**matter**

7. I can’t deal with unsymetrical things. I can’t even hold an uneven number of bags of groceries in each

**hand**

8. Every inch of you is perfect from the bottom to the

**top**

9. I’m thankful my childhood was filled with imagination and bruises from playing

**outside**

10. I like how the same people are in almost all of Adam Sandler’s

**just**

## Task 6 - Data Product

The goal of this exercise is to create a product to highlight the prediction algorithm that you have built and to provide an interface that can be accessed by others via a Shiny app..

Tasks to accomplish

Create a data product to show off your prediction algorithm You should create a Shiny app that accepts an n-gram and predicts the next word.

Questions to consider

What are the most interesting ways you could show off your algorithm?

Are there any data visualizations you think might be helpful (look at the Swiftkey data dashboard if you have it loaded on your phone)?

How should you document the use of your data product (separately from how you created it) so that others can rapidly deploy your algorithm?

Tips, tricks, and hints

Consider the size of the predictive model you have developed. You may have to sacrifice some accuracy to have a fast enough/small enough model to load into Shiny.

**Please refer to the shiny App folder in the repository for more details.**


## Task 7 - Slide Deck

The goal of this exercise is to "pitch" your data product to your boss or an investor. The slide deck is constrained to be 5 slides or less and should: (1) explain how your model works, (2) describe its predictive performance quantitatively and (3) show off the app and how it works.

Tasks to accomplish

Create a slide deck promoting your product. Write 5 slides using RStudio Presenter explaining your product and why it is awesome!

Questions to consider

How can you briefly explain how your predictive model works?

How can you succinctly quantitatively summarize the performance of your prediction algorithm?

How can you show the user how the product works?

Tips, tricks, and hints

The Rstudio presentation information is available here (https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations).

**Please refer to the index.Rmd and index.html files for the presentation slides in the repository.**