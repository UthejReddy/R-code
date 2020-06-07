#install packages
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews on oneplus 7T #############################
#enter url of product
oneplus <- "https://www.amazon.in/Test-Exclusive-749/dp/B07DJ8K2KT/ref=sr_1_1?dchild=1&keywords=oneplus+8&qid=1591417680&sr=8-1"
amazon_reviews <- NULL
#use condition for loop
for (i in 1:10){
  murl <- read_html(as.character(paste(oneplus,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)#shows the count of reviews
write.table(amazon_reviews,"oneplus7T.txt",row.names = F)#creates a txt file containing the reviews


# Importing oneplus reviews data
review <- readLines(file.choose())
review
length(review)

stpwrd <- scan(file.choose(), what="character", comment.char=";")


# Corpus
install.packages("tm")
library(tm)
oneplus_corpus <- Corpus(VectorSource(review))
inspect(oneplus_corpus[1])


# Data Cleansing
oneplus_corpus2 <- tm_map(oneplus_corpus, tolower)
inspect(oneplus_corpus2[1])
oneplus_corpus2 <- tm_map(oneplus_corpus, removePunctuation)
inspect(oneplus_corpus2[1])
oneplus_corpus2 <- tm_map(oneplus_corpus2, removeNumbers)
oneplus_corpus2 <- tm_map(oneplus_corpus2, removeWords, stopwords('english'))
inspect(oneplus_corpus2[1])

#striping white spaces
oneplus_corpus <- tm_map(oneplus_corpus2, stripWhitespace)
inspect(oneplus_corpus2[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(oneplus_corpus2)
dtm <- t(tdm)
tdm <- as.matrix(tdm)

# Bar plot
words <- rowSums(tdm)
words
words_sub <- subset(words, words >= 10)
words_sub
windows()
barplot(words_sub, las=3, col = rainbow(20))



# Word cloud
install.packages("wordcloud")
library(wordcloud)
windows()
wordcloud(words = names(words_sub), freq = words_sub) # wordcloud with only subset of words
words_sub2 <- sort(rowSums(tdm), decreasing = TRUE)
wordcloud(words = names(words_sub2), freq = words_sub2) # all words are considered

windows()
wordcloud(words = names(words_sub2), freq = words_sub2, random.order = F, colors = rainbow(20), scale=c(3,1), rot.per = 0.3)

# lOADING positive AND negative words for sentimental analysis.
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including our own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(words_sub2), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- words_sub2[pos.matches]
pos_names <- names(freq_pos)
windows()
wordcloud(pos_names,freq_pos,scale=c(4,1),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(words_sub2), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- words_sub2[neg.matches]
neg_names <- names(freq_neg)
windows()
wordcloud(neg_names,freq_neg,scale=c(5,1),colors = brewer.pal(8,"Dark2"))

####emotion mining####3
install.packages("syuzhet")
library("syuzhet")
library(lubridate,ggplot2)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

E_review <- iconv(review, "UTF-8") #Unicode Transformation Format. The '8' means it uses 8-bit blocks to represent a character

x <- get_nrc_sentiment(E_review)
head(x,n=5)

E_review[4]
get_nrc_sentiment('happy')
get_nrc_sentiment('boring')

get_sentiment('boring',method="afinn")
get_sentiment('happy',method="afinn")

#each sentences by eight 
example<-get_sentences(E_review)
nrc_data<-get_nrc_sentiment(example)


# Bar plot for emotion mining
windows()
barplot(colSums(nrc_data), las = 1, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')



sentiment_vector<-get_sentiment(example,method="bing")
sentiment_afinn<-get_sentiment(example,method="afinn")
sentiment_nrc<-get_sentiment(example,method="nrc")

sum(sentiment_afinn)
mean(sentiment_afinn)
summary(sentiment_afinn)
windows()
plot(sentiment_vector,type='l',main='Plot trajectory',xlab='Narative time',ylab='Emotional valence')
abline(h=0,color='red')

plot(
  sentiment_vector, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

##Shape smoothing and normalization using a Fourier based transformation and low pass filtering is achieved using the get_transformed_values function as shown below.
ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values, 
  type ="l", 
  main ="onplus7T reviews using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

#Most Negative and Positive reviews
negative<-example[which.min(sentiment_vector)]
positive<-example[which.max(sentiment_vector)]
my_example_text <- "I begin this story with a neutral statement.  
  Basically this is a very silly test.  
You are testing the Syuzhet package using short, inane sentences.  
I am actually very happy today. 
I have finally finished writing this package.  
Tomorrow I will be very sad. 
I won't have anything left to do. 
I might get angry and decide to do something horrible.  
I might destroy the entire package and start from scratch.  
Then again, I might find it satisfying to have completed my first R package. 
Honestly this use of the Fourier transformation is really quite elegant.  
You might even say it's beautiful!"
s_v <- get_sentences(my_example_text)
s_v_sentiment <- get_sentiment(s_v)
plot(
  s_v_sentiment, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
  
