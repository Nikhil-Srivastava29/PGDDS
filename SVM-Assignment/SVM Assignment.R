library(kernlab)
library(readr)
library(caret)
library(caTools)

digit_rec_test <- read.csv("mnist_test.csv", stringsAsFactors = F,header = F)
digit_rec_train <- read.csv("mnist_train.csv", stringsAsFactors = F,header = F)

#Understanding Dimensions
dim(digit_rec_train) # 60000 Rows, 785 Columns
dim(digit_rec_test)  # 10000 Rows, 785 Columns

# Reduce training size to 10% of training set to reduce computation
trainsamplesize <- .05*nrow(digit_rec_train)
train_sample <- sample(1:nrow(digit_rec_train), trainsamplesize, replace=FALSE)
train = digit_rec_train[train_sample,]


testsamplesize <- .1*nrow(digit_rec_test)
test_sample <- sample(1:nrow(digit_rec_test), testsamplesize, replace=FALSE)
test = digit_rec_test[test_sample,]


colnames(test)[1]<-"digit"
colnames(train)[1]<-"digit"


colname <- vector(mode = "character",length = ncol(train)-1)
for(i in 2:ncol(train)-1)
{
  colname[i] <-paste("column_",i,sep="")
}

colnames(test)[2:785]<-colname
colnames(train)[2:785] <- colname

#Structure of the dataset
str(train)

#printing first few rows
head(train)

#Exploring the data
summary(train)

# Checking missing value
sapply(train, function(x) sum(is.na(x))) # No missing values


train$digit <- as.factor(train$digit)
test$digit <- as.factor(test$digit)


#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)

# Accuracy : 0.913           
# 95% CI : (0.8938, 0.9297)

#Using RBF Kernel
Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit)

#Accuracy : 0.947           
#95% CI : (0.9312, 0.9601)

############   Hyperparameter tuning and Cross Validation #####################

#Performing Cross Validation using TrainControl
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" - our Evaluation metric is Accuracy.

metric <- "Accuracy"

set.seed(7)
grid <- expand.grid(.sigma=c(0.015 ,0.025, 0.05), .C=c(0.1,0.5,1,2))

fit.svm <- train(digit~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

plot(fit.svm)


# Constructing final model with sigma(gamma) = 0.05 and Cost(c) =0.1
Final_model <- ksvm(digit~ ., data = train, scale = FALSE, gamma=0.05,cost=0.1 ,kernel = "rbfdot")
Eval_Final_model<- predict(Final_model, test)

confusionMatrix(Eval_Final_model,test$digit)

#Accuracy : 0.947           
#95% CI : (0.9312, 0.9601)