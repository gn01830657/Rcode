library(streamR)
library(lubridate)
library(stringr)
library(plyr)
library(ROAuth)
library(RCurl)

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.pem")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- '3aSVlydMlsD92X6XAWIQgKdAD'
consumerSecret <- 'VvYJiKJtNtqnfaYKc9lJkj9zbEmRm4eZGYxoXiheJ8htrmCi8z'
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake(cainfo="cacert.pem")
save(list="twitCred", file="twitteR_credentials")


keyword <- c(":-)",":)")
filterStream("twitter1.json", track = keyword,
             timeout = 10800, oauth = twitCred)

keyword <- c(":D",":-D","XD")
filterStream("twitter2.json", track = keyword,
             timeout = 10800, oauth = twitCred)

keyword <- c(":-(",":(")
filterStream("twitter3.json", track = keyword,
             timeout = 10800, oauth = twitCred)

keyword <- c(":O","o_O","o_0")
filterStream("twitter4.json", track = keyword,
             timeout = 10800, oauth = twitCred)

tweets1 <- parseTweets("twitter1.json", simplify = TRUE)
tweets2 <- parseTweets("twitter2.json", simplify = TRUE)
tweets3 <- parseTweets("twitter3.json", simplify = TRUE)
tweets4 <- parseTweets("twitter4.json", simplify = TRUE)

#save to hadoop

#load from hadoop

#mission1
len1 <- length(tweets1$text)
len2 <- length(tweets2$text)
len3 <- length(tweets3$text)
len4 <- length(tweets4$text)


slices <- c(len1, len2,len3, len4)
lbls <- c(":-)", ":D", ":-(", ":0")
pie(slices, labels = lbls, main="Pie Chart of Countries")

#mission2
#wordcount

#mission3&4
dat1 <- data.frame(tweets1)
dat1 <- dat1[!is.na(dat1$time_zone),]
count1 <- count(dat1$time_zone)[order(count(dat1$time_zone)[,2],decreasing = TRUE),]
pie(count1$freq, labels = count1$x, main="Pie Chart of Countries")
sum1 <- sum(dat1$retweet_count)

dat2 <- data.frame(tweets2)
dat2 <- dat2[!is.na(dat2$time_zone),]
count2 <- count(dat2$time_zone)[order(count(dat2$time_zone)[,2],decreasing = TRUE),]
pie(count2$freq, labels = count2$x, main="Pie Chart of Countries")
sum2 <- sum(dat2$retweet_count)

dat3 <- data.frame(tweets3)
dat3 <- dat3[!is.na(dat3$time_zone),]
count3 <- count(dat3$time_zone)[order(count(dat3$time_zone)[,2],decreasing = TRUE),]
pie(count3$freq, labels = count3$x, main="Pie Chart of Countries")
sum3 <- sum(dat3$retweet_count)

dat4 <- data.frame(tweets4)
dat4 <- dat4[!is.na(dat4$time_zone),]
count4 <- count(dat4$time_zone)[order(count(dat4$time_zone)[,2],decreasing = TRUE),]
pie(count4$freq, labels = count4$x, main="Pie Chart of Countries")
sum4 <- sum(dat4$retweet_count)

slices <- c(sum1,sum2,sum3,sum4)
lbls <- c(":-)", ":D", ":-(", ":0")
pie(slices, labels = lbls, main="Pie Chart of Countries")
