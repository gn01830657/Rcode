# load packages
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

# Filter the Twitter stream
filterStream("NBAplayoffs.json", track = c("NBA","Playoffs"),
             timeout = 10800, oauth = twitCred)
#filterStream("tweets_oscars.json", track = c("Oscars", "Oscars2014"),
#timeout = 10800, oauth = twitCred)

# Parse tweets
tweets <- parseTweets("NBAplayoffs.json", simplify = TRUE)
dat <- tweets
#tweets <- parseTweets("tweets_oscars.json", simplify = TRUE)

# Set locale of machine
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Get info on first tweet
dat$created_at[1]

# Parse values
dat$time <- as.POSIXct(dat$created_at, tz = "UTC", format = "%a %b %d %H
                       :%M:%S %z %Y")

# Round to nearest hour
dat$round_hour <- round_date(dat$time, unit = "hour")

# Plot the data
plot_time <- as.data.frame(table(dat$round_hour))
plot_time <- plot_time[-nrow(plot_time),]
plot_time
plot(plot_time[,2], type = "l", xaxt = "n", xlab = "Hour", ylab = "Frequency")
axis(1, at = c(1, 2, 3), labels = plot_time[c(1, 2, 3), 1])

# Inspect one entry
unlist(dat[1234,])

# Search terms
teams <- c(
  "Cleveland Cavaliers",
  "Toronto Raptors",
  "Miami Heat",
  "Atlanta Hawks",
  "Boston Celtics",
  "Charlotte Hornets",
  "Indiana Pacers",
  "Detroit Pistons",
  "Golden State Warriors",
  "San Antonio Spurs",
  "Oklahoma City Thunder",
  "Los Angeles Clippers",
  "Portland Trail Blazers",
  "Dallas Mavericks",
  "Memphis Grizzlies",
  "Houston Rockets"
)
# actor <- c(
# "matthew mcconaughey",
# "christian bale",
# "bruce dern",
# "leonardo dicaprio",
# "chiwetel ejiofor"
# )

# actress <- c(
# "cate blanchett",
# "amy adams",
# "sandra bullock",
# "judi dench",
# "meryl streep"
# )

# film <- c(
# "(12|twelve) years a slave",
# "american hustle",
# "captain phillips",
# "dallas buyers club",
# "gravity",
# "nebraska",
# "philomena",
# "(the )?wolf of wall street"
# )

# Detecting search terms
tmp_team <- lapply(dat$text, str_detect, teams)
dat_team <- ldply(tmp_team)
colnames(dat_team) <- teams
# tmp_actor <- lapply(dat$lotext, str_detect, actor)
# dat_actor <- ldply(tmp_actor)
# colnames(dat_actor) <- c("mcconaughey", "bale", "dern", "dicaprio", "ejiofor")
# tmp_actress <- lapply(dat$lotext, str_detect, actress)
# dat_actress <- ldply(tmp_actress)
# colnames(dat_actress) <- c("blanchett", "adams", "bullock", "dench", "streep")
# tmp_film <- lapply(dat$lotext, str_detect, film)
# dat_film <- ldply(tmp_film)
# colnames(dat_film) <- c("twelve_years", "american_hustle", "capt_phillips", "dallas_buyers", "gravity", "nebraska", "philomena", "wolf_wallstreet")

# Get summary statistics
apply(dat_team, 2, sum)
barplot(apply(dat_team, 2, sum),las = 3)
# apply(dat_actor, 2, sum)
# apply(dat_actress, 2, sum)
# apply(dat_film, 2, sum)

# Approximate matching
tmp_team2 <- lapply(teams, agrep, dat$text)
length_team <- unlist(lapply(tmp_team2, length))
names(length_team) <- teams
length_team
barplot
# tmp_actor2 <- lapply(actor, agrep, dat$lotext)
# length_actor <- unlist(lapply(tmp_actor2, length))
# names(length_actor) <- c("mcconaughey", "bale", "dern", "dicaprio", "ejiofor")
# tmp_actress2 <- lapply(actress, agrep, dat$lotext)
# length_actress <- unlist(lapply(tmp_actress2, length))
# names(length_actress) <- c("blanchett", "adams", "bullock", "dench", "streep")
# length_actor
# length_actress


######
library("tm")
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}
getSentiment <- function (text, key){
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  ##########################################
  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  # get mood probability
  sentiment = js$output$result
  
  ###################################
  return(list(sentiment=sentiment))
}
#tweets = read.csv(file.choose())
r_stats_text <- sapply(tweets, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(tweets))
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
r_stats_text_corpus = tm_map(r_stats_text_corpus, removeNumbers)
myDtm <- TermDocumentMatrix(r_stats_text_corpus, control = list(minWordLength = 1))
inspect(myDtm[100:130,10:30])
