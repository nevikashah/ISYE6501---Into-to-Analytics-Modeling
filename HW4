setwd("/Users/nevi.r.shah@ey.com/desktop")

#QUESTION 9.1
rm(list = ls())
#read in data
usc <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#apply principal component analysis
pca <- prcomp(usc[,-16], center = TRUE, scale = TRUE)
#view summary
summary(pca)



#plot PCs vs variance 
screeplot(pca,main = "Which PCs", type = "line")
#kaiser method says variance greater than 1 is important
abline(h=1, col="red") 


#thus we will use 5 as our PC number
k=5
#we combine our first 5 pcs
crimePCA <- cbind(pca$x[,1:k], usc[,16])
#create data frame to pass into linear model 
DFcrimePCA <- data.frame(crimePCA)
lmMod <- lm(V6~., data = DFcrimePCA)
#view summary 
summary(lmMod) 

#performing transformation
bet0<- lmMod$coefficients[1] #get intercepts for transformation 
betVec <- lmMod$coefficients[2:(k+1)] #create vector of model coeff
alph <- pca$rotation[,1:k] %*% betVec #to make alpha vector multiply coeff by rotated matrix 
mu <- sapply(usc[,-16],mean) #mu or mean
sig <- sapply(usc[,-16],sd) #sigma or sd
OGalph <- alph/sig #original alpha value 
OGbet0 <- bet0 - sum(alph*mu/sig) #original beta value 
mtrx <- as.matrix(usc[,1:15]) #create matrix 
preds <- mtrx %*% OGalph + OGbet0 #get my estimated model 

#get accuracy 
sumsqerr = sum((preds - usc[,16])^2)
total = sum((usc[,16] - mean(usc[,16]))^2)
#calculate rsquared using sum of squared error and total sum of squares 
rsquared <- (1 - (sumsqerr/total))
#view rsquare to interpret our model 
rsquared

#use model to predict given parameters of last weeks homework 
newprediction <- data.frame(M= 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,
                       LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, 
                       Wealth = 3200, Ineq = 20.1, Prob = 0.040,Time = 39.0)

#create df of PCs using last weeks homework data
pdf <- data.frame(predict(pca, newprediction)) 
#pass PCA dataframe into model 
myPrediction <- predict(lmMod, pdf)
#view model 
myPrediction
#the prediction looks better than last weeks homework which was extremely low as compared to many data points in the original data set 



#QUESTION 10.1a
library("tree")
set.seed(1)
crime <- read.delim("uscrime.txt", header = TRUE)
#calculate total sum of squares
total <- sum((crime[,16] - mean(crime[,16]))^2)

#create regression using Crime as the Y var
crimetree <- tree(Crime ~., data = crime)
plot(crimetree)
text(crimetree)
summary(crimetree)

#performed k=10 cross validation and plot the node numbers against the deviance
set.seed(2)
crimetree_cv <- cv.tree(crimetree, K=10)
#plot cv object and plot the node size by the deviance 
plot(crimetree_cv$size, crimetree_cv$dev, type = "b")

#best node number is 5  because it minimizes the deviance according to the plot 
crimetree_pruned <- prune.tree(crimetree, best = 5)
plot(crimetree_pruned)
text(crimetree_pruned)
title("Pruned Tree")
summary(crimetree_pruned)

#the R2 value of the pruned model 
crimetree_pred2 <- predict(crimetree_pruned, data=crime[,-16])
RSS <- sum((crimetree_pred2 - crime[,16])^2)
r2_2 = 1- RSS/total
r2_2

#this is a pretty food R^2 value. However, looking at my two tree models crimetree_pruned and crimetree, it looks like the model that is not pruned has a lower deviance (the one with 7 nodes). 
#I believe this could perhaps be because of overfitting. It seems the less nodes I have the better the deviance gets 

#QUESTION 10.1B
library("randomForest")
set.seed(2)
rm(list = ls())
uscrime <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#create random forest model 
forest<- crimeRF <- randomForest(Crime~., data = uscrime)

#find which number of trees minimizes the numbers error
which.min(forest$mse)
#it's 130 - i will use this number and play around with the mtry value 

crimeRF1 <- randomForest(Crime~., data = uscrime, mtry = 4, importance = TRUE, ntree = 130)
crimeRF1

crimeRF2 <- randomForest(Crime~., data = uscrime, mtry = 5, importance = TRUE, ntree = 130)
crimeRF2

crimeRF3 <- randomForest(Crime~., data = uscrime, mtry = 8, importance = TRUE, ntree = 130)
crimeRF3 

#looking at the % of variance explained, it looks like the smaller the mtry value the better the % of variance explained 
#However, I believe this percentage of this model is still very low. This could be due to the fact that there are only 47 data points in the data set 

#QUESTION 10.2 
##One real life application of logistic regression is if we are trying to look at the survival for a certain disease. Some of the predictors can be sex, if they received treatment, and age. From a logistic regression we can use use these predictors to ultimately predict the response on whether or not someone survived (1) or died (0). 

#QUESTION 10.3 
rm(list = ls())
credit <- read.table("germancredit.txt", sep = " ") 


credit$V21[credit$V21 == 1] <- 0
credit$V21[credit$V21 == 2] <- 1

head(credit)
table(credit$V21)

train <- credit[1:800,]
test <- credit[801:1000,]

logistic <- glm(V21 ~ ., family = "binomial"(link = "logit"), data = train)
summary(logistic)

prediction = predict(logistic, test, type='response')

#create baseline prediction with every variable included and threshold of .5. 
table(test$V21, round(prediction))
#It looks like the prediction matrix correctly classifies 147 out of 200. That's about a 73.5% accuracy 

#to improve this model, I manually am getting rid of any predictors that have a pvalue of less than .01. The follow equations say that for each
#predictor with multiple categories. The variable with the highest signficance 
train$V1A14[train$V1 == "A14"] <- 1
train$V1A14[train$V1 != "A14"] <- 0
train$V3A34[train$V3 == "A34"] <- 1
train$V3A34[train$V3 != "A34"] <- 0
train$V4A41[train$V4 == "A41"] <- 1
train$V4A41[train$V4 != "A41"] <- 0
train$V9A93[train$V9 == "A93"] <- 1
train$V9A93[train$V9 != "A93"] <- 0
logistic2 <- glm(V21 ~ V1A14+V2+V3A34+V4A41+V8+V9A93, data = train, family=binomial(link="logit"))
summary(logistic2)

#next ill use test data and follow the same steps 
test$V1A14[test$V1 == "A14"] <- 1
test$V1A14[test$V1 != "A14"] <- 0
test$V3A34[test$V3 == "A34"] <- 1
test$V3A34[test$V3 != "A34"] <- 0
test$V4A41[test$V4 == "A41"] <- 1
test$V4A41[test$V4 != "A41"] <- 0
test$V9A93[test$V9 == "A93"] <- 1
test$V9A93[test$V9 != "A93"] <- 0

logistic3 <- glm(V21 ~ V1A14+V2+V3A34+V4A41+V8+V9A93, data = test, family=binomial(link="logit"))
summary(logistic3)
 
predict <- predict(logistic3, newdata=test[,-21], type="response")
predmatrix1 <- as.matrix(table(round(predict), test$V21))
predmatrix1

threshold <- .5
threshold1 <- .6
threshold2 <- .7

#we want to minimize 5*false positive + false negative cost 
#accuracy = 144/200 = 72%
#cost = 5*17 + 39 = 124
predmatrix <- as.matrix(table(round(predict > threshold), test$V21))
predmatrix

#accuracy = 150/200 = 75%
#cost = 5*4 + 46 = 66
predmatrix1 <- as.matrix(table(round(predict > threshold1), test$V21))
predmatrix1

#accuracy = 139/200 =  69.5
#cost = 5*1 + 54 = 59 
predmatrix2 <- as.matrix(table(round(predict > threshold2), test$V21))
predmatrix2

#based on my 3 models, it seems that a threshold of .7 yields the lowest cost using the formula 5*False Negative + False Positive
                
