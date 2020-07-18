



install.packages("caret")
library(caret)
install.packages("Rtools")
library(Rtools)
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
install.packages("e1071")
library("e1071")
install.packages("class")
library("class")
library(caTools)
library(dplyr)
#load the dataset
zoo <- read.csv(file.choose())
View(zoo)


#table of glass type
table(zoo$type)

#table of glass type
table(zoo$animal.name)

zoo1 <- zoo[,2:18]
str(zoo1)

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))


#Apply the normalization function to zoo dataset
zoo_norm <- as.data.frame(lapply(zoo[2:17], norm))
View(zoo_norm)
anyNA(zoo_norm)
str(zoo_norm)

zoo$type <- as.factor(zoo$type)
anyNA(zoo$type)
str(zoo$type)
zoo$type

#Join the standardized data with the target column
data <- cbind(zoo_norm,zoo[18])
#Check if there are any missing values to impute. 
anyNA(data)
help("anyNA")

#splitting data randomly by considering glass type as splitting parameter with 70:30 ratio
set.seed(101)


sample <- sample.split(data,SplitRatio = 0.70)

train <- subset(data,sample==TRUE)

test <- subset(data,sample==FALSE)

dim(train[1:16])
dim(test[1:16])
length(train$type) 
str(train$type)
train$type

library("class")
#KNN model buildingg with random sampling 

zoo_pred <- knn(train[1:16],test[1:16],train$type,k=1)

#Error in prediction
error <- mean(zoo_pred!=test$type)
zoo_pred
test$type

str(zoo_pred)
str(test$type)

#Confusion Matrix
confusionMatrix(zoo_pred,test$type)
#from summary accuracy is 100% it means this sampling is relevant for prediction
# now we have to check for accuracy with different value k

# iterations of for k values
zoo_pred <- NULL
error.rate <- NULL

for (i in 1:10) {
  zoo_pred <- knn(train[1:16],test[1:16],train$type,k=i)
  error.rate[i] <- mean(zoo_pred!=test$type)
  
}

knn.error1 <- as.data.frame(cbind(k=1:10,error.type =error.rate))


ggplot(knn.error1,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')


zoo_pred2 <- knn(train[1:16],test[1:16],train$type,k=2)

#Error in prediction
error <- mean(zoo_pred2!=test$type)

#Confusion Matrix
confusionMatrix(zoo_pred2,test$type)
#from summary accuracy is 91% it means this sampling is relevant for prediction
# now we can conclude that from k=1 is optimum value




