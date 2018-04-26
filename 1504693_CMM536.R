
# Function to Install and Load R Packages
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];

  if(length(Remaining_Packages))
  {
    install.packages(Remaining_Packages);
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}


#Import librarys
library(caret)
library(partykit)
library(mlbench)
library(RWeka)
library(C50)
library(datasets)
library(rpart)
library(ggplot2)
library(data.table)
library(stream)
library(mlbench)
library(doParallel)
library(streamMOA)
library(e1071)
require(RMOA)
require(ROCR)



# Specify the list of required packages to be installed and load
Required_Packages=c("ggplot2", "jsonlite", "data.table", "RMOA", "ROCR", "stream", "caret", "partykit",
                    "RWeka", "C50", "datasets", "rpart", "mlbench", "doParallel", "streamMOA", "RMOA", "ROCR", "e1071");
# 
# # Call the Function
Install_And_Load(Required_Packages)
#Clean RStudio Environment
# rm(list = ls())




#Set working directory
setwd("~/CMM536 Advanced Data Science/Coursework/CMM536_Coursework")

#Feature names
adultNames <- c("age", "workclass", "fblwgt",
                "education", "education-num", 
                "martial-status",
                "occupation",
                "relationship",
                "race",
                "sex",
                "captial-gain",
                "captain-loss",
                "hours-per-week",
                "native-country",
                "class")



#Import datasets
adult <- read.table("data/adult.data" ,header = FALSE, sep = ",",
                         strip.white = TRUE, col.names = adultNames,
                         na.strings = "?", stringsAsFactors = TRUE)


#inspect dataset
str(adult)

head(adult, 10)

summary(adult)

#basic exploration
nrow(adult)
ncol(adult)

#class distrubtion
barplot(table(adult$class))



##Check unique classes
length(unique(adult$class))

#Check for missing values ('?')
table(complete.cases (adult))

#Remove rows with null values in the workclass & occupation column
cleanadult = adult[!is.na(adult$workclass)& !is.na(adult$occupation) & !is.na(adult$class),]

#Remove flwgt feature
cleanadult$fblwgt = NULL

str(cleanadult)



#Copy dataset
noClass <-cleanadult
#Remove class as it is not being transformed to binary
noClass$class <- NULL

binaryVars <- caret::dummyVars(~ ., data = noClass)
newAdult <- predict(binaryVars, newdata = noClass)

#add class to binarised dataset
binAdult <-cbind(newAdult, cleanadult[14])
View(binAdult)

# #transform class to numeric
# str(binAdult$class)
# binAdult$class <- as.numeric(binAdult$class)

#remove any rows with NA values
row.has.na <- apply(binAdult, 1, function(x){any(is.na(x))})
sum(row.has.na)
binAdult <- binAdult[!row.has.na,]



#classifer


# train_index <- sample(1:nrow(binAdult), 0.8 * nrow(binAdult))
# test_index <- setdiff(1:nrow(binAdult), train_index)


#Setup Parallel processing to speed up classification modelling
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)


#split 80% training and 20% testing datasets

inTrain <- createDataPartition(y=binAdult$class, p=0.8, list=FALSE)


#Assign indexes to split the binAdult dataset into training and testing
training <- binAdult[inTrain,]
testing <- binAdult[inTrain,]



ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     number = 10,
                     verboseIter=TRUE)

# ensure reproducibility of results by setting the seed to a known value
set.seed(1)
#use knn
knnModel<- train(class~., data=training,method="knn", tuneGrid=expand.grid(.k=3), trControl=ctrl)

print(knnModel)
summary(knnModel$finalModel)
names(mod21.knn)


#Evaluation
predictkNN <- predict(knnModel,testing)
confusionMatrix(predictkNN, testing$class)


#Data streaming classifer


#Pre-processing

#copy the binary adult dataset
df <- binAdult

#Convert the class to numeric values
# df$class = lapply(df$class, as.numeric)
# df$class <- unlist(df$class)

str(df$class)

#Set 
ctrl <- MOAoptions(model = "OCBoost", randomSeed = 123456789, ensembleSize = 25,
                   smoothingParameter = 0.5)
mymodel <- OCBoost(control = ctrl)
mymodel



dfStream <-datastream_dataframe(data=as.data.table(df))
chunk <- 50
turns <- (nrow(dfStream$data)/chunk)-1
turns <- floor(turns)
position <- chunk

#first sample (train)
sample <- dfStream$get_points(dfStream, n =chunk,
                             outofpoints = c("stop", "warn", "ignore"))
head(sample,3)
head(df,3)

str(binAdult$class)


##Train the first chunk

myboostedclasifier <- trainMOA(model=mymodel,
                      formula = class~.,
                      data = datastream_dataframe(sample))

#Now iterate ove the whole stream
for (i in 1:turns){
  #next sample 
  
  sample <- dfStream$get_points(dfStream, n =chunk,
                                outofpoints = c("stop", "warn", "ignore"))
  
  #update the trained model with the new chunks
  myboostedclasifier <- trainMOA(model = myboostedclasifier$model,
                        formula = class~., 
                        data = datastream_dataframe(sample),
                        reset = FALSE, trace=FALSE)
  
  cat("chunk: ",i, "\n")
}

##Do some prediction to test the model

predictions <- predict(myboostedclasifier, sample)
table(sprintf("Reality: %s", sample$class),
      sprintf("Predicted: %s", predictions))


predictions <- as.factor(predictions)
confusion.mstream <- confusionMatrix(predictions, sample$class)

cat("Accuracy is: ", confusion.mstream$overall["Accuracy"])



#Hold results in a vector
accuracies <- c()
dfStream$reset()

for(i in 1:turns){
  #next sample
  
  sample <- dfStream$get_points(dfStream, n=chunk,
                                outofpoints = c("Stop", "warn", "ignore"))
  predictions <- predict(myboostedclasifier, sample)
  predictions <- as.factor(predictions)
  #caculate accuracy
  
  confusion.mstream <- confusionMatrix(predictions, sample$class)
  accuracies[i] <- confusion.mstream$overall["Accuracy"]
  
  cat(accuracies[i],"%","\n")
  
}

head(accuracies)


plot(accuracies,type='l',col='red',
     xlab="Chunk Number",ylab="Accuracy",frame=FALSE)
