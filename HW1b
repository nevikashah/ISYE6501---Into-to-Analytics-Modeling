#import kknn library
library (kknn)

#set working directory 
setwd("/Users/nevi.r.shah@ey.com/documents/Homework_ISYE")

#import credit card data without headers 
cc_data <- read.table("credit_card_data.txt", header=FALSE, stringsAsFactors=FALSE)

#setting seed value in order to produce same results if code is script is again 
set.seed(1)

#separate data set into training and testing. 80% will be train and 20% will be test
random_row<- sample(1:nrow(cc_data),as.integer(0.8*nrow(cc_data)))

#set train data set
trainData = cc_data[random_row,]

#set test data set
testData = cc_data[-random_row,]

#use train.kkn function to find out best kernal to use (performs loocv)
train.kknn(as.factor(V11)~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data = trainData, kmax = 30, scale = TRUE)

#create data frame for training data and initalize in order to later populate 
predicted_train <- rep(0,(nrow(trainData)))

#k = 19  and kernel = "optimal" (as indicated in our loocv output)
#now using the "optimal" method to retrain on train data
for (i in 1:nrow(trainData)){
  model=kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,trainData[-i,],trainData[i,],k=19,kernel="optimal", scale = TRUE)
  predicted_train[i]<- as.integer(fitted(model)+0.5) 
}

#calculate train accuracy value by calculating average of all predictions
train_accuracy<- sum(predicted_train == trainData[,11]) / nrow(trainData)

#obtain train accuracy 
train_accuracy

#repeat process for test validation and measure effectiveness of train data 
#train accuracy should be higher than test accuracy
predicted_test <- rep(0,(nrow(testData)))
for (i in 1:nrow(testData)){
  model=kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,testData[-i,],testData[i,],k=19,kernel="optimal", scale = TRUE)
  predicted_test[i]<- as.integer(fitted(model)+0.5)
}

#calculate test accuracy 
test_accuracy<- sum(predicted_test == testData[,11]) / nrow(testData)

#obtain test accuracy 
test_accuracy

#test accuracy is __% and train accuracy is __%. 
#as I predicted, test accuracy is less than train accuracy
