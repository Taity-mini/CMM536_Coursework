#Load Necessary libraries
require(tm)
require(wordcloud)
library(wordcloud,quietly=TRUE)
library(RColorBrewer,quietly=TRUE)

leaveRemainTweets <- read.csv("data/leaveRemainTweets_CW.csv", header=TRUE)

head(leaveRemainTweets)

names(leaveRemainTweets)

ncol(leaveRemainTweets)
nrow(leaveRemainTweets)


#split dataset into leave and remain dataframes
tweets = split(leaveRemainTweets, leaveRemainTweets$label)
remainTweets = tweets$Remain
leaveTweets = tweets$Leave

nrow(remainTweets)
nrow(leaveTweets)


#Custom Corpus Function for preprocessing  
 
# create a corpus for text

buildCorpus <- function (tweets){
  corp <- Corpus(VectorSource(tweets$text))
  
  
  corp <- tm_map(corp,
                 content_transformer(function(x) iconv(x, to='ASCII',
                                                       sub='byte')))
  # remove stop words and other preprocessing
 
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeNumbers)
  
  ##Remove obvious words /stopwords
  corp <- tm_map(corp, function(x)removeWords(x,c(stopwords("english"),"amp", "will", "‰Û_", "https", "htdb")))
  
  ##tweet cleaning
  
  # corp = gsub("&amp", "",  corp)
  # clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
  # clean_tweet = gsub("@\\w+", "", clean_tweet)
  # # clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
  # # clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
  # # clean_tweet = gsub("http\\w+", "", clean_tweet)
  # # clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  # # clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
  
  
  ##Remove URLS from tweets
  
  toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
  
  #credit to https://stackoverflow.com/a/31352005/8816204
  corp <-tm_map(corp, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
  corp <-tm_map(corp, toSpace, "@\\w+")
  corp <-tm_map(corp, toSpace, "&amp")
  corp <-tm_map(corp, toSpace, "httpsw+")
  corp <-tm_map(corp, toSpace, "http:\\w+")
  corp <-tm_map(corp, toSpace, "https:\\w+")

  corp <-tm_map(corp, toSpace, "[ \t]{2,}")
  corp <-tm_map(corp, toSpace, "^\\s+|\\s+$")
  
  tdm <- TermDocumentMatrix(corp) ##
  #remove punctuation last so urls are removed correctly
  corp <- tm_map(corp, removePunctuation)
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  d$word <- gsub("˜", " ", d$word) ## Edit 2
  
  tweets <- d$word
}


leaveWordCloud <- buildCorpus(leaveTweets)

remainWordCloud <- buildCorpus(remainTweets)


wordcloud(words = leaveWordCloud, freq = d$freq, min.freq = 3,
          max.words=2000, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))


wordcloud(words = remainWordCloud, freq = d$freq, min.freq = 3,
          max.words=2000, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))


d$word
# uncomment to Save the image (words cloud in a file)
# png("MachineLearningCloud.png")
# Run this alone will produce the image in the RSTudio plots viewer


#Text Analyis


