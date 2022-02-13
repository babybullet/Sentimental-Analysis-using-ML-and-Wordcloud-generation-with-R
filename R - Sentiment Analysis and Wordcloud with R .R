#Reading a csv file
apple <- read.csv('Apple.csv')
str(apple) # str gives us the structure of the data 

library(tm) #The main structure for managing documents in tm is called a Corpus,
#which represents a collection of text documents. 
#tm provides a set of predefined sources, e.g., DirSource, VectorSource, or DataframeSource,
corpus <- iconv(apple$text, to = "utf-8") #iconv is used to specify encoding feature to a selected column 
corpus <- Corpus(VectorSource(corpus))# Corpus is a collection of document, we use each tweet as a document here
inspect(corpus[1:5]) #VectorSource(x) : Takes a grouping of 
#texts and makes each element of the resulting vector a document within your R Workspace.
 
# Clean text
corpus <- tm_map(corpus, tolower)   #Tm stands for Transformation the tm_map() function is used to remove unnecessary white space,
#to convert the text to lower case, to remove common stopwords like 'the', "we". 
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)# removePunctuation removes comma, fullstop, @$ etc.
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))# Removing common words like, we, would, on , in ...
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)# gsub returns all the similar links
cleanset <- tm_map(cleanset, content_transformer(removeURL)) #content transformers, i.e., functions which modify the content of an R object.

inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document matrix A term document matrix is a way of representing
#the words in the text as a table (or matrix) of numbers.
tdm <- TermDocumentMatrix(cleanset)
tdm # Sparcity in matrix shows numbers of 0 
tdm <- as.matrix(tdm)# representation of the tdm in form of matrix 
tdm[1:15, 1:20] # 1st 10 words and 1st 20 column

# Bar plot
w <- rowSums(tdm) #Rowsums- rowSums() function in R Language is 
#used to compute the sum of rows of a matrix or an array. dims, we get frequency of words by doing this 
w <- subset(w, w>=30) # w>30 means frequency of words greater then 25 
barplot(w,
        las = 2, #The las argument allows to change the orientation of the axis labels, las=2 means vertically
        col = rainbow(50))
#In barplot, earnings frequency is much higher because this tweets
#are all made before the earnings started

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)#rowSums computes sums of rows
set.seed(222) #ensures that we get the same result if we start with that 
#same seed each time we run the same process. set.seed starts a sequence of random numbers.

wordcloud(words = names(w),
          freq = w,
          max.words = 300,
          random.order = F,
          min.freq = 10,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3), # scale used to change size of wordcloud
          rot.per = 0.7) # if you want rotation in wordcloud



# Sentiment analysis
install.packages('syuzhet')
library(syuzhet) # Calls get_nrc_sentiment for analysis dictionary in last part
#The package comes with four sentiment dictionaries and provides
#a method for accessing the robust, but computationally expensive, sentiment
#extraction tool developed in the NLP group
library(lubridate) #Lubridate is an R package that makes it easier to work with dates and times. 
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr) #dplyr is a new package which provides a set of tools for 
#efficiently manipulating datasets in R

# Reading apple file
apple <- read.csv('Apple.csv')
tweets <- iconv(apple$text, to = 'utf-8')

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('delay')

# Bar plot generation
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')
