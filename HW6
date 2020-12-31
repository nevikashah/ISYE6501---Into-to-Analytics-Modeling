setwd("/Users/nevi.r.shah@ey.com/desktop")
install.packages("mice")
library("mice")
bcData <- read.table("breast-cancer-wisconsin.data.txt", header=FALSE, stringsAsFactors=FALSE, sep = ",")
head(bcData)
set.seed(123)

#its obvious that v7 is where the data is missing since it cannot calculate its summary
summary(bcData)

#return every row where data is missing - where it finds a "?"
bcData[which(bcData$V7 =="?"),]

#returns the indices of missing data given what we found in the above function 
unknown<-which(bcData$V7 =="?")
#view indeces 
unknown


#part 14.1-1
#create a mode function 
modey <- function(item){
  val <- unique(item)
  val[which.max(tabulate(match(item, val)))]
}

#use modey function created and find v7 mode 
v7Mode <-as.numeric(modey(bcData[-unknown,"V7"]))

#view mode value 
v7Mode
#this is 1

#impute the mode value of 1
impute <- bcData
impute[unknown,]$V7<- v7Mode
impute$V7<- as.integer(impute$V7)
head(impute, 24)

#part 14.1-2
#create linear reg
updated <- bcData[-unknown, 2:10]
updated$V7 <- as.integer(updated$V7)

#regress all other predictors on V7 and use step to understand which predictors are the most important 
linmod <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10, updated)
step(linmod)

#pull significant predictors 
linmod2 <- lm(V7~V2+V4+V5+V8, updated)
summary(linmod2)

#predict V7 missing values for each index
V7Prediction <- predict(linmod2, newdata = bcData[unknown,])

#impute this predicted value into the data set using indeces we know are missing value  
impute2 <- bcData
impute2[unknown,]$V7 <-V7Prediction
impute2$V7 <- as.numeric(impute2$V7)
head(impute2, 24)

#part 14.1-3
#regression with perturbation 
prediction_perturb <- rnorm(length(unknown), V7Prediction, sd(V7Prediction))
prediction_perturb

#assign data to variable 
imputed <- bcData
#impute perturbed prediction into dataset 
imputed[unknown,]$V7 <- V7Prediction
#impute variable as decimal given there are decimal values
imputed$V7 <- as.numeric(imputed$V7)
head(imputed, 24) 
