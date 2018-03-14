#Import library
rm(list = ls())


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

# Specify the list of required packages to be installed and load    
Required_Packages=c("ggplot2", "jsonlite", "data.table", "RMOA", "ROCR", "stream", "caret");

# Call the Function
Install_And_Load(Required_Packages)


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
adultTrain <- read.table("data/adult.data" ,header = FALSE, sep = ",",
                         strip.white = TRUE, col.names = adultNames,
                         na.strings = "?", stringsAsFactors = TRUE)


#inspect dataset
str(adultTrain)

head(adultTrain, 10)

summary(adultTrain)

#basic exploration
nrow(adultTrain)
ncol(adultTrain)



##Check unique classes
length(unique(adultTrain$class))

#Check for missing values ('?')
table(complete.cases (adultTrain))

#Remove rows with null values in the workclass & occupation column
cleanAdultTrain = adultTrain[!is.na(adultTrain$workclass)& !is.na(adultTrain$occupation) & !is.na(adultTrain$class),]

#Remove flwgt feature
cleanAdultTrain$fblwgt = NULL

str(cleanAdultTrain)



#Copy dataset
noClass <-cleanAdultTrain
#Remove class as it is not being transformed to binary
noClass$class <- NULL

binaryVars <- caret::dummyVars(~ ., data = noClass)
newAdult <- predict(binaryVars, newdata = noClass)

#add class to binarised dataset
binAdult <-cbind(newAdult, cleanAdultTrain[14])
View(binAdult)

#transform class to numeric
str(binAdult$class)
binAdult$class <- as.numeric(binAdult$class)

#remove any rows with NA values
row.has.na <- apply(binAdult, 1, function(x){any(is.na(x))})
sum(row.has.na)
binAdult <- binAdult[!row.has.na,]



#classifer


#split into training and test sets

#split randomly into 80% training and 20% testing datasets
train_index <- sample(1:nrow(binAdult), 0.8 * nrow(binAdult))
test_index <- setdiff(1:nrow(binAdult), train_index)
                            
#Assign indexes to split the binAdult dataset into training and testing
training <- binAdult[train_index,]
testing <- binAdult[test_index,]


library(mlbench);library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     number = 10,
                     verboseIter=TRUE)

# ensure reproducibility of results by setting the seed to a known value
set.seed(1)
#use knn
mod21.knn<- train(class~., data=training, 
                  method="knn", tuneGrid=expand.grid(.k=3), trControl=ctrl)

print(mod21.knn)
summary(mod21.knn$finalModel)
names(mod21.knn)

#Evaluation
library(caret)
library(partykit)
library(mlbench)
library(RWeka)
library(C50)
library(datasets)
library(rpart)
confusionMatrix.train(mod21.knn)




