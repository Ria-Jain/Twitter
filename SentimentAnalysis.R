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