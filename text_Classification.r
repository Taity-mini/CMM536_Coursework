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

buildCorpus <- function (tweets, wholeDataSet, wordAssocation){
  corp <- Corpus(VectorSource(tweets$text))
  
  
  corp <- tm_map(corp,
                 content_transformer(function(x) iconv(x, to='ASCII',
                                                       sub='byte')))
  # remove stop words and other preprocessing
 
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeNumbers)

  toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
  
  
  ##Tweet cleaning
  
  #credit to https://stackoverflow.com/a/31352005/8816204
  corp <-tm_map(corp, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
  corp <-tm_map(corp, toSpace, "@\\w+")
  corp <-tm_map(corp, toSpace, "&amp")
  
  ##Remove URLS from tweets
  corp <-tm_map(corp, toSpace, "httpsw+")
  corp <-tm_map(corp, toSpace, "http:\\w+")
  corp <-tm_map(corp, toSpace, "https:\\w+")

  corp <-tm_map(corp, toSpace, "[ \t]{2,}")
  corp <-tm_map(corp, toSpace, "^\\s+|\\s+$")
  

  # ##Remove obvious words /stopwords
  if(missing(wholeDataSet)){
    corp <- tm_map(corp, function(x)removeWords(x,c(stopwords("english"),"amp", "will", "‰Û_", "https", "http", "httpsdb")))
  } else if(wholeDataSet == TRUE){
    corp <- tm_map(corp, function(x)removeWords(x,c(stopwords("english"),"amp", "will", "‰Û_", "https", "http", "httpsdb", "eu", "brexit", "rt", "leave", "remain", "vote")))
  }

  #remove punctuation last so urls are removed correctly
  corp <- tm_map(corp, removePunctuation)
  
  if(missing(wordAssocation)){
    tdm <- TermDocumentMatrix(corp)
    
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    d$word <- gsub("˜", " ", d$word) ## Edit 2
    
    tweets <- d$word
  }else if(wordAssocation == TRUE){
    dtm <- DocumentTermMatrix(corp)
  }
}


leaveWordCloud <- buildCorpus(leaveTweets)

remainWordCloud <- buildCorpus(remainTweets)


wordcloud(words = leaveWordCloud, freq = d$freq, min.freq = 3,
          max.words=2000, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = remainWordCloud, freq = d$freq, min.freq = 3,
          max.words=2000, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))


#Text Analyis


# 1) Find the most frequent word in the collection of tweets

tweetWordCloud <-buildCorpus(leaveRemainTweets, TRUE)

head(tweetWordCloud,1)

# 2) Word Assoication 

wordAssoication <- buildCorpus(leaveRemainTweets,TRUE,TRUE)


(freq.terms <- findFreqTerms(wordAssoication, lowfreq = 15))

findAssocs(wordAssoication, freq.terms, 0.2)


# cor_1 <- findAssocs(wordAssoication, colnames(wordAssoication)[1:2], 0))
(cor_1 <- findAssocs(wordAssoication, head(tweetWordCloud,10), 0))


# 3) Top Most Frequent Words in Both Leave and Remain tweets

#Leave
leaveWords <- buildCorpus(leaveTweets, TRUE)
head(leaveWords, 10)


#Remain
remainWords <- buildCorpus(remainTweets, TRUE)
head(remainWords, 10)








