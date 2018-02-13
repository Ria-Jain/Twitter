install.packages("tm")
install.packages("SnowballC")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("readxl")

library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tidyverse)
library(readxl)

tweets <- read_excel("C:/Users/RiaJain/Desktop/Twitter/tweets.xlsx", sheet=1)
tweets$polarity <- as.factor(tweets$polarity)

table(tweets$polarity)
#negative  neutral positive 
#  43       19      138 

corpus <- Corpus(VectorSource(tweets$text))
corpus[[1]]$content
View(corpus)
# Convert to lower-case.
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]$content

corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

corpus <- tm_map(corpus, removeWords, c('apple', stopwords('english')))
corpus[[1]]$content

corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content
corpus <- Corpus(VectorSource(tweets$text))

frequencies <- DocumentTermMatrix(corpus)



findFreqTerms(frequencies, lowfreq = 20)

# Remove these words that are not used very often. Keep terms that appear in 0.5% or more of tweets.

sparse <- removeSparseTerms(frequencies, 0.995)

tweetsSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$polarity <- tweets$polarity

# Build a training and testing set.
set.seed(123)
split <- sample.split(tweetsSparse$polarity, SplitRatio=0.7)
trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)

# Build a CART classification regression tree model on the training set.
tweetCART <- rpart(polarity ~ ., data=trainSparse, method='class')
prp(tweetCART)

predictCART <- predict(tweetCART, newdata=testSparse, type='class')
table(testSparse$polarity, predictCART)

predictCART
            #negative neutral positive
#negative        4       0        9
#neutral         2       4        0
#positive        3       0       38

(4+4+38) / nrow(testSparse)
#Accuracy: 0.7666667
# Baseline assume always not-negative.
table(testSparse$polarity)
#negative  neutral positive 
#  13        6       41 

# Try a random forest model.
set.seed(123)
tweetRF <- randomForest(polarity ~ ., data=trainSparse)
tweetRF

#Call:
#randomForest(formula = polarity ~ ., data = trainSparse) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 18

#OOB estimate of  error rate: 20.71%
#Confusion matrix:
  #        negative neutral positive class.error
#negative       14       0       16  0.53333333
#neutral         0       7        6  0.46153846
#positive        6       1       90  0.07216495

#Accuracy: 0.7928571
(14+7+90)/nrow(trainSparse)

predictRF <- predict(tweetRF, newdata=testSparse)
table(testSparse$polarity, predictRF)

#predictRF
#          negative neutral positive
#negative        4       0        9
#neutral         0       5        1
#positive        0       0       41
# Accuracy:  0.8333333
(4 + 5+41) / nrow(testSparse)


