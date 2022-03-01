#install.packages('tm')
#install.packages("wordcloud")
#install.packages("wordcloud2")
#install.packages("syuzhet")
#install.packages("stringr")

library(stringr)
library(syuzhet)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)

######################### Sentiment Analysis on Child book - Black Beauty  #############################

#Reading CSV file (child dataset)
child_book <- read.csv("C:/Users/Priyanka Bhagwat/Desktop/MA331_datasets/271_Black Beauty.csv", na.strings = c("","NA"), stringsAsFactors = F)

#Examining the internal structure child dataset
str(child_book)

#omiting the NA values.
child_book <- child_book %>% na.omit()
dim(child_book)

#iconv converts a character vector from one string encoding to another.
child_corpus <- iconv(child_book$text)

#The main structure for managing documents in tm is a so-called Corpus, representing a collection of text documents.
child_corpus <- Corpus(VectorSource(child_corpus))

#The inspect function opens an interactive window that allows for the manipulation of a number of arguments 
#It offers several views to analyze the series graphically.
inspect(child_corpus[1:10])

#Cleaning the text 
child_corpus <- tm_map(child_corpus, tolower)  #converting text into lower case.
child_corpus <- tm_map(child_corpus, removeNumbers) # Removing number between the text
child_corpus <- tm_map(child_corpus, removePunctuation) # Removing Punctuation
child_corpus <- tm_map(child_corpus, removeWords, stopwords('english')) #Removing stop words in English
child_corpus <- tm_map(child_corpus, stripWhitespace) #Removing white spaces between the text
#inspect(child_corpus)

#A term document matrix is a way of representing the words in the text as a table (or matrix) of numbers
tdm <- TermDocumentMatrix(child_corpus)
#tdm 

tdm_matrix <- as.matrix(tdm)
tdm_matrix[1:10, 1:20]

child_words_plot <- rowSums(tdm_matrix) #an inbuilt R function used to calculate the sum of rows of a matrix or an array
#Subsetting is a useful indexing feature for accessing object elements
child_frequent_words <- subset(child_words_plot, child_words_plot >= 50) #to get the words which are repeated more that 250 times
child_frequent_words

#Barplot to represent frequently used words
barplot(child_frequent_words, las = 2, col = rainbow(50))

#Removing Insignificant words that are frequently employed in texts but offer no sentiment or emotion
child_corpus <- tm_map(child_corpus, removeWords, c('made','side','said','will','shall','now','thing','sir','three'))

#replacing horses with horse
child_corpus <- tm_map(child_corpus, gsub, pattern ='horses', replacement ='horse')

tdm <- TermDocumentMatrix(child_corpus)
tdm_matrix <- as.matrix(tdm)
tdm_matrix[1:10, 1:20]

child_words_plot <- rowSums(tdm_matrix)
child_frequent_words <- subset(child_words_plot, child_words_plot >= 50)
child_frequent_words

#Barplot to represent frequently used words
barplot(child_frequent_words, las = 2, col = rainbow(50))

#Barplot to represent top 10 used words in child book
sort <- sort(rowSums(tdm_matrix), decreasing = T)
frequent_words <- data.frame(word=names(sort), freq=sort)
barplot(frequent_words[1:10,]$freq,
        names.arg = frequent_words[1:10,]$word, 
        col="Red", main = "Top 10 frequent words used in Black Beauty book")

set.seed(222) # forreproducibility.

child_word_cloud <- sort(rowSums(tdm_matrix), decreasing = T)
#wordcloud helps in graphical representations of word frequency that give greater prominence to words that appear more frequently in a source text.
wordcloud(words = names(child_word_cloud),
          freq = child_frequent_words,
          max.words = 300,
          random.order = F,
          min.freq = 1,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.1), 
          rot.per = 0.5)

#representation using wordcloud2 with shape as Triangle
child_word_cloud2 <- data.frame(names(child_frequent_words), child_frequent_words)
colnames(child_word_cloud2) <- c('word', 'freq')
wordcloud2(child_word_cloud2, 
           size = 0.5, 
           gridSize =  0, 
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

#sentiment analysis on child book
child_Emotions <- iconv(child_book$text)

#Understanding the emotions and sentiments of entire data using get_nrc_sentiments function.
#The columns include one for each emotion type was well as the positive or negative sentiment valence.
child_sentiment_score <- get_nrc_sentiment(child_Emotions)
head(child_sentiment_score)
barplot(colSums(child_sentiment_score),las=2,col=rainbow(10)
        ,main = "Sentiment classification of Black Beauty(Child  book)")

################ Sentiment Analysis on Emma Adult book #################

#Reading CSV file (Adult dataset)
adult_book <- read.csv("C:/Users/Priyanka Bhagwat/Desktop/MA331_datasets/1260_Jane Eyre-An Autobiography.csv", na.strings = c("","NA"), stringsAsFactors = F)

#Examining the internal structure child dataset
str(adult_book)

#omiting the NA values.
adult_book <- adult_book %>% na.omit()
dim(adult_book)

#iconv converts a character vector from one string encoding to another.
adult_corpus <- iconv(adult_book$text)

#The main structure for managing documents in tm is a so-called Corpus, representing a collection of text documents.
adult_corpus <- Corpus(VectorSource(adult_corpus))

#Cleaning the text

adult_corpus <- tm_map(adult_corpus, tolower) #converting text into lower case.
adult_corpus <- tm_map(adult_corpus, removeNumbers) # Removing numbers between the text
adult_corpus <- tm_map(adult_corpus, removePunctuation) #Removing Punctuation  
adult_corpus <- tm_map(adult_corpus, removeWords, stopwords('english')) #Removing stop words in English
adult_corpus <- tm_map(adult_corpus, stripWhitespace) #Removing white spaces between the text

#A term document matrix is a way of representing the words in the text as a table (or matrix) of numbers
tdm <- TermDocumentMatrix(adult_corpus)

tdm_matrix <- as.matrix(tdm)
tdm_matrix[1:10, 1:20]

adult_words_plot <- rowSums(tdm_matrix) #an inbuilt R function used to calculate the sum of rows of a matrix or an array
#Subsetting is a useful indexing feature for accessing object elements
adult_frequent_words <- subset(adult_words_plot, adult_words_plot >= 100) #to get the words which are repeated more that 100 times
adult_frequent_words

#Barplot to represent frequently used words
barplot(adult_frequent_words, las = 2, col = rainbow(50))

#Removing Insignificant words that are frequently employed in texts but offer no sentiment or emotion
adult_corpus <- tm_map(adult_corpus, removeWords, c('now','mrs','thing','said','say','will','can'))

#using term document matrix we can represent the text or words in a matrix form
tdm <- TermDocumentMatrix(adult_corpus)
tdm_matrix <- as.matrix(tdm)
tdm_matrix[1:10, 1:20]


adult_words_plot <- rowSums(tdm_matrix)
adult_frequent_words <- subset(adult_words_plot, adult_words_plot >= 50) #to get the words which are repeated more that 50 times
adult_frequent_words

#Barplot to represent frequently used words
barplot(adult_frequent_words, las = 2, col = rainbow(50))

#Barplot to represent top 10 used words in adult book.
sort <- sort(rowSums(tdm_matrix), decreasing = T)
frequent_words<- data.frame(word=names(sort), freq=sort)
barplot(frequent_words[1:10,]$freq,
        names.arg = frequent_words[1:10,]$word, 
        col="#00ffff", 
        main = "Top 10 frequent words used in Jane Eyre- An Autobiography Adult book")

#Representation using Wordcloud
#wordcloud helps in graphical representations of word frequency that give greater prominence to words that appear more frequently in a source text.
adult_word_cloud <- sort(rowSums(tdm_matrix), decreasing = T)
wordcloud(words = names(adult_word_cloud),
          freq = adult_frequent_words,
          max.words = 1500,
          random.order = F,
          min.freq = 1,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(3, 0.3), 
          rot.per = 0.7)

#Representation using Wordcloud2 with shape as Triangle
adult_word_cloud2 <- data.frame(names(adult_frequent_words), adult_frequent_words)
colnames(adult_word_cloud2) <- c('word', 'freq')
wordcloud2(adult_word_cloud2, 
           size = 0.5, 
           gridSize =  0, 
           shape = 'triangle',
           rotateRatio = 0.5, 
           minSize = 1)

#Understanding the emotions and sentiments of entire data using get_nrc_sentiments function.
#The columns include one for each emotion type was well as the positive or negative sentiment valence.
adult_Emotions <- iconv(adult_book$text)

adult_sentiment_score <- get_nrc_sentiment(adult_Emotions)
head(adult_sentiment_score)
barplot(colSums(adult_sentiment_score),las=2,col=rainbow(10),
        main = "Sentiment classification of Jane Eyre-An Autobiography(Adult Book)")

