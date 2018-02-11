#Install Packages
install.packages("twitteR")
install.packages("ROAuth")
install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("httr")
install.packages("wordcloud")
install.packages("tm")
install.packages("Rstem")
install.packages("psych")
install.packages("C:/Users/RiaJain/Desktop/Twitter/sentiment_0.2.tar.gz") #This package is present in the folder, change the path accordingly
#If unable to install the package, install the package manually from the bottom right window and choose the package from the folder

#Load the Libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(tm)
library(Rstem)
library(psych)
library(sentiment)


# setup twitter to allow R program to use twitter account
# visit Apps.Twitter.com
# create new app
# provide uniquename, some description, provide url in format http://www.anywebsite.com
# create access tockens
# copy all the 4 api/tockens --- into the code as below
# DO NOT HAVE ANY SPACE in the code

setwd("C:/Users/RiaJain/Desktop/Twitter")

oauth_endpoint(authorize = "https://api.twitter.com/oauth",
               access = "https://api.twitter.com/oauth/access_token")
#connect to API
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

consumerKey="jO0acHoCirfP4eVCzyEAxbv0O"
consumerSecret="iiyeOXJcuj9H0cBvH4cEPAGb7030BYWZjwHrAkUqPaTohmAHNP"
accesstoken="959475001261948931-kvqsWuwBUjxGYLFAgC6NqHbjKXsbHB7"
accesssecret="ZrulbbSw6EAkJ4lB4T43EYeigLNPDH1Ej8GgsHpjYPea4"

Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console

##### Authorization PIN -DYNAMIC

save(Cred, file='twitter authentication.Rdata')

load('twitter authentication.Rdata')

setup_twitter_oauth(consumer_key=consumerKey,
                    consumer_secret=consumerSecret,
                    access_token=accesstoken,
                    access_secret=accesssecret)

##****************Step 3: Perform tweets extraction and data cleaning****************

# Harvest the tweets
all_tweets = searchTwitter("election", n=100, lang="en")

tweets_df <- twListToDF(all_tweets)
write.csv(tweets_df,"AllTweets.csv")

# get the text
tweet_txt=sapply(all_tweets,function(x) x$getText())

# remove retweet entities
tweet_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)",
                 "", tweet_txt)
# remove at people
tweet_txt = gsub("@\\w+", "", tweet_txt)
# remove punctuation
tweet_txt = gsub("[[:punct:]]", "", tweet_txt)
# remove numbers
tweet_txt = gsub("[[:digit:]]", "", tweet_txt)
# remove html links
tweet_txt = gsub("http\\w+", "", tweet_txt)
# remove unnecessary spaces
tweet_txt = gsub("[ \t]{2,}", "", tweet_txt)
tweet_txt = gsub("^\\s+|\\s+$", "", tweet_txt)
 
# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x),
                       error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
# lower case using try.error with sapply
tweet_txt = sapply(tweet_txt, try.error)

# remove NAs in tweet_txt
tweet_txt = tweet_txt[!is.na(tweet_txt)]
names(tweet_txt) = NULL

write.csv(tweet_txt, "CleanedTweets.csv")

tweet_txt = removeWords(tweet_txt, stopwords(kind = "en"))

# classify emotion
class_emo = classify_emotion(tweet_txt, algorithm="bayes", prior=1.0)

# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(tweet_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

## *********Create data frame with the results and obtain some general statistics******
# data frame with results
sent_df = data.frame(text=tweet_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

sent_df1 = within(sent_df,
                  polarity <- factor(polarity, levels=names(sort(table(polarity), decreasing=TRUE))))

ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", title="classification based on emotion") 


## plot distribution of Polarity
ggplot(sent_df1, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="Dark2")+labs(x="polarity categories", y="number of tweets",title="classification based on polarity")