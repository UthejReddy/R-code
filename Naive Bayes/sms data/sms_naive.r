

sms_raw <- read.csv(file.choose(), stringsAsFactors = FALSE)

str(sms_raw)

# convert to factor.
sms_raw$type <- factor(sms_raw$type)

str(sms_raw$type)
table(sms_raw$type)

# build a corpus 
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
# examine 
print(sms_corpus)
inspect(sms_corpus[1])

# data cleaning
sms_clean <- tm_map(sms_corpus, tolower)
sms_clean <- tm_map(sms_clean, removeNumbers)
sms_clean <- tm_map(sms_clean, removeWords, stopwords())
sms_clean <- tm_map(sms_clean, removePunctuation)
sms_clean <- tm_map(sms_clean, stripWhitespace)

# examine the clean corpus
inspect(sms_clean[1])

# create a document term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_clean)
inspect(sms_dtm)

# create train and test datasets
sms_train1 <- sms_raw[1:4169, ]
sms_test1  <- sms_raw[4170:5559, ]

dtm_train <- sms_dtm[1:4169, ]
dtm_test  <- sms_dtm[4170:5559, ]

corpus_train <- sms_clean[1:4169]
corpus_test  <- sms_clean[4170:5559]

# check the proportion of spam
prop.table(table(sms_train1$type))
prop.table(table(sms_test1$type))

# word cloud
library(wordcloud)
wordcloud(corpus_train, min.freq = 30, random.order = FALSE)

# combine the training data into spam and ham groups
spam <- subset(sms_train1, type == "spam")
ham  <- subset(sms_train1, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5),colors = 'blue')
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

sms_dict<-findFreqTerms(dtm_train, 3)

sms_train <- DocumentTermMatrix(corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(corpus_test, list(dictionary = sms_dict))

# convert to  factor
counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply counts to columns of train and test data
sms_train<- apply(sms_train, MARGIN = 2, counts)
sms_test  <- apply(sms_test, MARGIN = 2, counts)

#Training a model on the data 
library(e1071)
sms_classifier <- naiveBayes(sms_train1, sms_train1$type)


# Evaluating model performance 
sms_pred <- predict(sms_classifier, sms_test1)

install.packages(gmodel)
library(gmodels)
CrossTable(sms_pred, sms_test1$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Improving model performance 
sms_classifier2 <- naiveBayes(sms_train1, sms_train1$type, laplace = 1)
sms_pred2 <- predict(sms_classifier2, sms_test1)
CrossTable(sms_pred2, sms_test1$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
# Here ham and spam data are perfectly classified