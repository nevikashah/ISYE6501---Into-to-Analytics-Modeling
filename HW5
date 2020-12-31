setwd("/Users/nevi.r.shah@ey.com/desktop")
library("glmnet")
library("caret")
library("lattice")
library("ggplot2")

crime <- read.delim("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
#QUESTION 11.1 - STEPWISE 

#backward elimination 
backward <- lm(Crime~., data = crime)
step(backward,direction = "backward")
step(backward, direction = "backward", trace = 0)

#forward selection 
forward <- lm(Crime~1, data = crime)
step(forward, scope = formula(lm(Crime~., data = crime)), direction = "forward", trace = 0)

#stepwise - both  
control <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
bothStep <- lm(Crime~., data = crime, )
stepped <- step(both, scope = list(lower = formula(lm(Crime~1, data = crime)), 
                                   upper = formula(lm(Crime~., data = crime))), direction = "both", trControl = control)

lmBoth <- lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = crime)
summary(lmBoth)

#perhaps we can take out M.F since it has a p-value of .1. Let's see if that changes anything 
#R^2 of this model is .7888
#i want to test how good this model actually is on the entire data set using LOOCV

#perform cross validation with K  = 47 because the dataset is so small 
TSS <- sum((crime$Crime - mean(crime$Crime))^2)
SSE <- 0 
for(i in 1:nrow(crime)) {
  loocv = lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, data = crime[-i,])
  predicted <- predict(loocv, newdata=crime[i,])
  SSE <- SSE + ((predicted - crime[i,16])^2)
}
rsquared <- 1-(SSE/TSS) #rsquared of .667 which is not as good as the original model but we can expect still good for testing on cross validated model 

#for parts 2 and 3 let's scale the data 
columns <- colnames(crime[,-2])[1:14]
scale <- function(dataframe, columns) {
  scaled <- dataframe
  
  for(item in columns) {
    mean <- mean(dataframe[,item])
    stdev <- sd(dataframe[,item])
    scaled[,item] <- sapply(scaled[,item], function(x)(x-mean)/stdev)
  }
  
  return(scaled)
}
scaledData <- scale(crime, columns)

#QUESTION 11.1 - LASSO 
set.seed(123)
lasso <- cv.glmnet(x=as.matrix(scaledData[,-16]), 
                   y = as.matrix(scaledData[,16]),
                   alpha = 1,
                   nfolds = 8,
                   nlambda = 20, 
                   type.measure = "mse",
                   family = "gaussian",
                   standardize = TRUE)

lasso 
plot(lasso)
lasso$lambda.min
cbind(lasso$lambda, lasso$cvm, lasso$nzero)
coef(lasso, s = lasso$lambda.min)

lassoMod <- lm(Crime ~ M+So+Ed+Po1+LF+M.F+NW+U2+Ineq+Prob, data = scaledData)
#rsquared is .7767 which is better than our both stepwise model. We will not rerun this model after we have done variable selection to avoid circulat reasoning 

#QUESTION 11.1 - ELASTIC NET  
rsquaredElastic=c()
for (item in 0:10) {
  elastic = cv.glmnet(x=as.matrix(scaledData[,-16]),y=as.matrix(scaledData$Crime),
                      alpha=item/10,nfolds = 5,
                      type.measure="mse",
                      family="gaussian")
}

rsquaredElastic = cbind(rsquaredElastic, 
                        elastic$glmnet.fit$dev.ratio[which(elastic$glmnet.fit$lambda == elastic$lambda.min)])
rsquaredElastic

rsquared=c()
for (i in 0:10) {
  elastic = cv.glmnet(x=as.matrix(scaledData[,-16]),y=as.matrix(scaledData$Crime), 
                      alpha=i/10, nfolds = 5,type.measure="mse", family="gaussian")
  
  minIndex = which(elastic$glmnet.fit$lambda == elastic$lambda.min)
  #dev.ratio is the rsquared value 
  rsquared = cbind(r2, elastic$glmnet.fit$dev.ratio[minIndex])
}
rsquared

#get alpha 
alpha = which.max(rsquared)
alpha

#the max alpha value is .7924326 so we will use "9" for our alpha value 
#now lets rebuild our elastic model 

elasticNew =cv.glmnet(x=as.matrix(scaledData[,-16]),y=as.matrix(scaledData$Crime),alpha=9,nfolds = 9,type.measure="mse",family="gaussian")
coef(elasticNew, s=elasticNew$lambda.min)
elasticLM <- lm(Crime~ M+So+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = scaledData)
#R2 value is .7752

#looking at the three different techniques to variable selection it looks like stepwise regression yielded the highest Rsquared value. 

#QUESTION 12.1 
#As a product manager, I am constantly trying to understand what will make my product sell. We have to consider a number of factors that will influence customers to buy in and purchase my product. I could use a design of experiments approach by first understanding and benchmarking of successful products in regard to pricing, packaging, and delivery. Once I figure out my levels of each factor, I could run a survey asking what the market prefers to truly understand what makes a good product sell. 

#QUESTION 12.2
library("FrF2")
set.seed(123)
fr <- FrF2(16, 10)
#in the data frame for each of the 16 factors (columns), we can see which of the 10 factors (columns) should be included

#QUESTION 13.1

#a. Binomial: we can use binomial distribution for patient and hospital experiments if a patient will live or die due to a cancer treatment 
#b. Geometric: geometric distribution could be represented with the number of interviews taken before getting enough practice to successfull have a good interview and ultimaely land a job
#c. Poisson: we can use poisson distribution to understand the probablity of an amazon customer service rep getting a phone call at different times of the day 
#d. Exponential: the number of covid cases in new york city during the initial stages of outbreak prior to social distancing laws 
#e. Weibull: can be used to calculate the probability that a part will fail or break down after being manufactured in a certain number of years in the future 








