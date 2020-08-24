#
# Final Project for the Data Science Capstone Course 
# Johns Hopkins University
# Author: Jorge Cordero
# August the 23rd, 2020
#
#   Found Online at:  https://cordero.shinyapps.io/DSCAPSTONEAPP/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("JHU - Data Science - CAPSTONE SWIFTKEY Data"),

        sidebarPanel(
            div(img(src="wordcloud-1.png", height = "100%", width ="100%"), style="text-align: center;"),
            br(),
            
            textInput(inputId="wordsInput", label = "Please enter any word or sentence in the field: ",
                      value = "", width = NULL, placeholder = "Enter any Sentence!"),
            br(),
            
            selectInput(inputId="algorithmC", label="Algorithm(s) to Apply:", choices= c("EMS", "HT", "EMS + HT"),
                        multiple = FALSE, selected = "EMS + HT"),
            br(), br(),
            
            h4("The Suggested Words will appear in the right panel, automatically in less than 1 second..."),
            br(),
            code("IF YOU JUST OPENNED THE WEBPAGE PLEASE WAIT FOR 10 SECONDS! LOADING MODELS!...")
        ),
        mainPanel(
            tabsetPanel(
                            tabPanel('Word Predictions', align='center',
                                     code("IF YOU JUST OPENNED THE WEBPAGE PLEASE WAIT FOR 10 SECONDS! LOADING MODELS!..."),
                                     h3("Entered Word:"),
                                     textOutput("outWords"),
                                     tags$head(tags$style("#outWords{color: red; font-size: 21px; font-style: bold;}")), 
                                     br(),
                                     
                                     h3("Trimmed Word (cleanned):"),
                                     textOutput("outCleannedWord"),
                                     br(),
                                    
                                     h3("Suggested Words (predictions):"),
                                     textOutput("outPredictions"),
                                     tags$head(tags$style("#outPredictions{color: green; font-size: 23px; font-style: bold;}")),
                                     br(), br(),
                                     
                                     textOutput("outAlgc"),
                                     tags$head(tags$style("#outAlgc{color: blue; font-size: 20px; font-style: italic;}")),   
                            ),
                            tabPanel('Data Summary', align='center',
                                     code("IF YOU JUST OPENNED THE WEBPAGE PLEASE WAIT FOR 10 SECONDS! LOADING MODELS!..."),
                                     h2("Models trained with a random sample of 3% of the initial SwiftKey Data"),
                                     p("The initial corpus consisted of three files, a news dataset, a blog dataset and a twitter message dataset. The size of those files was 556 MBs but we sampled over 3%. Obtaining 129,000 lines of text for training the model algorithms."),
                                     p("An n-gram is a contiguous sequence of n items from a given sample of text or speech. The items can be phonemes, syllables, letters, words or base pairs according to the application. The n-grams typically are collected from a text or speech corpus. When the items are words, n-grams may also be called shingles. Using Latin numerical prefixes, an n-gram of size 1 is referred to as a unigram; size 2 is a bigram (or, less commonly, a digram); size 3 is a trigram, etc."),
                                     h3("Most Common Unigrams"),
                                     plotOutput("plotUnigrams"),
                                     h3("Most Common Bigrams"),
                                     plotOutput("plotBigrams"),
                                     h3("Most Common Trigrams"),
                                     plotOutput("plotTrigrams"),
                                     h3("Most Common Tetragrams"),
                                     plotOutput("plotTetragrams")
                                    
                            ),
                            tabPanel('Algorithms', align='center',
                                     h2("Algorithms"),
                                     p("For all algorithms we estimated unigrams, bigrams, trigrams and tetragrams from the 3% sampled Data. We also computed their frequencies as well as their posterior probability."),
                                     h2("EM Algorithm"),
                                     p("The exact match algorithm is a simplistic approach that matches the entered words with previously computed n-grams in order to predict the next word:"),
                                     br(),
                                     p("1. Clean the output by removing, punctuation, badwords, spaces and other elements from the word(s)."),
                                     p("2. Tokenize and construct an n-gram with the cleanned input. If there are more than 3 input words, just take the last 3 words."),
                                     p("3. The tokenized word W of size n will be matched against the (n+1)grams that were computed offline. In case that we match an (n+1)gram we predict the most likely word n+1 (maximum probability)"),
                                     withMathJax( p("Probabilities are computed by $$P(w_n|w_1^{n-1})=\\frac{counts(w_1,w_2,...,w_n)}{counts(w_1,w_2,...,w_{n-1})}$$") ),
                                     p("4. We obtain the top three word predictions (highest probabilities)"),
                                     p("5. Backoff Modeling: If less than three suggestions have been found, then we reduce the input words to lower level (n-k)grams and repeat the same steps from step 3."),
                                     h2("HT Algorithm"),
                                     p("Historical track algorithm. Heuristic approach that looks to autocomplete missing words in an n-gram by estimating maximum likelihood given the words that we know."),
                                     br(),
                                     p("1. We clean and tokenize the input and obtain an input n-gram predictor W."),
                                     p("2. Divide W according to the chain rule applied to the bigram model:"),
                                     withMathJax( p("Probabilities in bigram model: $$PB(w_n|w_1^{n-1}) = P(w_1)P(w_2|w1)P(w_3|w_2),...,P(w_n|w_{n-1})$$") ),
                                     p("Thus we need to find the pairs of words that compose the input W in the observed bigrams."),
                                     p("3. For each pair of words contained in the sequence W:"),
                                     withMathJax( p("for $$(w_i,w_{i+1}) \\in W$$ if both $$(w_i, w_{i+1})$$ have been previously observed, then keep them and obtain their probability $$P(w_i+1|w_{i+1})$$") ),
                                     withMathJax( p("for $$(w_i,w_{i+1}) \\in W$$ if only $$w_{i+1}$$ has been previously observed, then find the observed $$w_{j}$$  with maximum  probability $$P(w_i+1|w_j)$$, replace $$w_{i}$$ with $$w_j$$ and store the pair $$(w_j, w_{i+1})$$") ),
                                     p("If $$w_{i}$$ has only been observed, then discard the other word and the probability of the unseen word becomes the marginal probability of what we have observed $$P(w_i,w_{i+1})=P(w_i)$$ "),
                                     p("4. Having the autocompleted sequence, then suggest the top 3 words that maximizes PB."),
                                     h2("EM +HT "),
                                     p("Combine EM and HT: If EM did not find the 3 suggestions then use HT to find them."),
                                     h3("We suggest the top 3 words with the highest posterior conditional probability.")
                            ),
                            tabPanel('Experimental Results', align='center',
                                     withMathJax(),
                                     code("IF YOU JUST OPENNED THE WEBPAGE PLEASE WAIT FOR 10 SECONDS! LOADING MODELS!..."),
                                     h2("Accuracy Results"),
                                     p("For each input sentence we tokenize it and convert it to an n-gram W. We compute the accuracy with an indicator function. We input 1 to the result of the experimental test case if the predicted word is identical to the held out word in the input. We add a 0 if the predicted and initial n-grams differ."),
                                     uiOutput("indicatorEquation"),
                                     p("We then report the average result by summing up the number of accurate results (1’s) and dividing by the total number of test cases for a given n-gram level. The results for each n-gram tupple is presented in the following table:"),
                                     DT::dataTableOutput("tableAccuracy"),
                                     h2("Perplexity Results"),
                                     p("We measured the perplexity for the posterior joint probabilities computed by the training Data that match the unseen text. We decomposed the Data into the same four sample sizes and computed the mean perplexity for all test cases. Perplexity can be computed as:"),
                                     uiOutput("perplexityEquation"),
                                     p("The following table shows the results:"),
                                     DT::dataTableOutput("tablePerplexity")
                            )
                       )
            
        )
    
))

# In order to display the ui.R, server.R and README.html into the same page do the following:

# Create a text file with no extension. Name this file: DESCRIPTION. Place the following content and edit as necessary. The display mode is set for Showcase.
#Title: Hello Shiny!
#    Author: RStudio, Inc.
#AuthorUrl: http://www.rstudio.com/
#    License: GPL-3
#DisplayMode: Showcase
#Tags: getting-started
#Type: Shiny
#Save this file in the same directory as your server.R and ui.R files. Deploy as usual. Here is the reference: http://shiny.rstudio.com/articles/display-modes.html. Just be sure that you don't have a file type or extension associated with your DESCRIPTION file.

