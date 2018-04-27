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

buildCorpus <- function (tweets, wordCloud, wholeDataSet, wordAssocation, dtm){
  
  #Check the optional parameters are set
  wordA <-hasArg(wordAssocation)
  wds <- hasArg(wholeDataSet)
  wordc <-  hasArg(wordCloud)
  dtmcheck <- hasArg(dtm)
  
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
  if(wds){
    if(wholeDataSet){
      corp <- tm_map(corp, function(x)removeWords(x,c(stopwords("english"),"amp", "will", "‰Û_", "https", "http", "httpsdb", "eu", "brexit", "rt", "leave", "remain", "vote")))
    }
  } else{
    corp <- tm_map(corp, function(x)removeWords(x,c(stopwords("english"),"amp", "will", "‰Û_", "https", "http", "httpsdb")))  
  }
  
  
  
  #remove punctuation last so urls are removed correctly
  corp <- tm_map(corp, removePunctuation)
  
  #If wordAssocation is true then 
  

  
  #If wordAssocation is true then create term document  matrix
  if(wordA){
    if(wordAssocation){
      tdm <- TermDocumentMatrix(corp)
    }
    else{
      if(dtm){
        dtm <- DocumentTermMatrix(corp)
      }
    }
    
  }
 
  else if(!wordA){ # If no  wordAssocation then create TermDocument Matrix
    tdm <- TermDocumentMatrix(corp)
    
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    d$word <- gsub("˜", " ", d$word) ## Edit 2
    
    
    if(wordc){
      if(wds){
        tweets <- d$word
      } else{
        tweets <- d$word
        if(wordCloud){
          wordcloud(words =tweets, freq = d$freq, min.freq = 3,
                    max.words=2000, random.order=FALSE, rot.per=0.2,
                    colors=brewer.pal(8, "Dark2"))
        }
      }
      
    }
    
  }
 
}

leaveWordCloud <- buildCorpus(leaveTweets,TRUE)

remainWordCloud <- buildCorpus(remainTweets,TRUE)


wordcloud(words = leaveWordCloud, freq = d$freq, min.freq = 3,
          max.words=2000, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = remainWordCloud, freq = d$freq, min.freq = 3,
          max.words=2000, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))


#Text Analyis


# 1) Find the most frequent word in the collection of tweets

tweetWordCloud <-buildCorpus(leaveRemainTweets, FALSE, TRUE)
head(tweetWordCloud,1)

tweetWordCloud <-buildCorpus(leaveRemainTweets, TRUE)

head(tweetWordCloud,1)

# 2) Word Assoication 

wordAssoication <- buildCorpus(leaveRemainTweets,TRUE,TRUE)


freq.terms <- findFreqTerms(wordAssoication, lowfreq = 15)

findAssocs(wordAssoication, freq.terms, 0.2)



wordAssoication <- buildCorpus(leaveRemainTweets,FALSE,TRUE,TRUE)
freq.terms <- findFreqTerms(wordAssoication, lowfreq =50)

library("Rgraphviz")

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

plot(wordAssoication, term = freq.terms, corThreshold = 0.12, weighting = T)




# 3) Top Most Frequent Words in Both Leave and Remain tweets

#Leave
leaveWords <- buildCorpus(leaveTweets, TRUE)
head(leaveWords, 10)


#Remain
remainWords <- buildCorpus(remainTweets, TRUE)
head(remainWords, 10)


#Text classification

#Get term document matrix
dtm <-  buildCorpus(leaveRemainTweets,FALSE,TRUE,FALSE,TRUE)

dtm <- as.matrix(dtm)
tweets <- data.frame(dtm)

tweets$label <- leaveRemainTweets$label

#Split data into training and testing subsets
#Divide the dataset into 70% training and 30% testing, to prevent overfitting from occurring
inTrainTweets <- createDataPartition(y=tweets$label, p=0.7, list=FALSE)

#Assign indexes to split the Tweets DTM into training and testing
training <- tweets[inTrainTweets,]
testing <- tweets[-inTrainTweets,]


cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)


train_control<- trainControl(method="cv", number=5,verboseIter=FALSE)



svmModel <- train(label~ ., data = training,
                  trControl = train_control,
                  tuneLength =10,
                  method = "svmRadial"
                
)

svmModel

#Evaluate

predictkNN <- predict(svmModel,testing)
confusionMatrix(predictkNN, testing$label)


#Improved

train_control<- trainControl(method="cv", number=10,verboseIter=FALSE)



rfModel <- train(label~ ., data = training,
                  trControl = train_control,
                  tuneLength =10,
                  method = "rf"
                  
)

RFModel<- train(label~., data=training,
                trControl=train_control,
                method="rf",
                tuneLength =10,
                metric = 'Accuracy'
)



predictSVM <- predict(svmModel2,testing)
confusionMatrix(predictSVM, testing$label)
