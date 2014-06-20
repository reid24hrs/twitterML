install.packages("streamR")  # from CRAN
# install_github("streamR", "pablobarbera", subdir = "streamR")  # from GitHub
install.packages("devtools")
install.packages("ROAuth")
install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
install.packages("plyr")
install.packages("stringr")

library("devtools")
library("ROAuth")
library("streamR")
library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")
library("ggplot2")
library("plyr")
library("stringr")

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "yiS6tLca7A9Oa7eS4HzHBmVqQ"
consumerSecret <- "oUVrb6K6fS64c21CXUKei8o3vlkb7sHjlmtCPjszH3j4meu5cF"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")


load("my_oauth.Rdata")

# Load df with tweets (either from text file or from Twitter)
#filterStream("/Users/tommy/twitterR/tweets.json", track = c("data"), timeout = (14400), 
#             oauth = my_oauth)
#df <- parseTweets("/Users/tommy/twitterR/tweets.json", simplify = TRUE)
#write.table(df, "/Users/tommy/twitterR/df2.txt", sep="\t")
df <- read.table("/Users/tommy/twitterR/df2.txt", sep="\t", header=TRUE, fill = TRUE, row.names=NULL)

# Create a feature - number of words in a tweet
df$text <- as.character(df$text)
df$words <- strsplit(df$text," ")
df$wordCount <- sapply(df$words,length)
df$wordCount

df$text=sapply(df$text,function(row) iconv(row,to='UTF-8'))

#A helper function to remove @ symbols from user names...
trim <- function (x) sub('@','',x)

# We extracted 8 features (8F) which consist of 

# one nominal (author) 

# and seven binary features 

# presence of shortening of words and slangs
# time-event phrases
# opinioned words
# emphasis on words
# currency and percentage signs

# “@username” at the beginning of the tweet
#df$to = who message is to, df$tonum is numeric 1 or 0
df$to=sapply(df$text,function(tweet) str_extract(tweet,"^(@[[:alnum:]_]*)"))
df$to=sapply(df$to,function(name) trim(name))
df$tonum <- 0
df$tonum[is.na(df$to)] <- 1
df$tonum

# RT at the beginning of a tweet
#df$rt = who's been RT'd, df$rtnum is numeric 1 or 0
df$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
df$rtnum <- 0
df$rtnum[is.na(df$rt)] <- 1
df$rtnum

# “@username” within the tweet but not at the start
df$userwithin=sapply(df$text,function(tweet) str_match(tweet,"[^@]+(@[[:alnum:]_]*)")[2])
df$userwithinnum <- 0
df$userwithinnum[is.na(df$userwithin)] <- 1
df$userwithinnum


# hash lang

# hash location just based on if longitude is present
df$locnum <- 1
df$locnum[is.na(df$longitude)] <- 0
df$locnum

# count number of chars

# count number of capital letters

# count number of punctuation marks

# count number of spaces 

# count number of repeated words

# count number of repeated characters

df <- read.table("/Users/tommy/twitterR/df2.txt", sep="\t", header=TRUE, fill = TRUE, row.names=NULL)

# replace NA values with 0
df[is.na(df)] <- 0

# convert non-numeric fields to numeric
attach(df)
retweet_count <- as.numeric(retweet_count)
friends_count <- as.numeric(friends_count)
favourites_count <- as.numeric(favourites_count)


# run logistic regression model to predict if locnum is 1 or 0
mylogit <- glm(locnum ~ rtnum + userwithinnum + tonum + retweet_count + favourites_count + followers_count + statuses_count + user_id_str + id_str, data = df, family = "binomial")
