#setwd
setwd("/Users/nevi.r.shah@ey.com/desktop")

#Question 7.1
#My best friend is business owner and creator of a fitness app that was released in 2018. Something she could consider doing is tracking the quarterly earnings (both in app purchases and purchase of app itself) as a method of forecasting. We would need to gather all the data from the last two years per quarter to predict our revenue for the upcoming year. I would expect the alpha value to be closer to 0, I  think that for consumer behavior includes lots of randomness especially regarding a consumer service app like fitness.  There are many days when people may or may not use the app that cannot be explained. 
#QUESTION 7.2

#read in data
temp <- read.table("temps.txt", header = TRUE)

#create vector from data 
tempV <- as.vector(unlist(temp[,2:21]))

#create time series from vector data 
tempsTimeSeries <- ts(data = tempV, frequency=123, start=1996)

#plot time 
plot(tempsTimeSeries)

#set seed for reproducible results 
set.seed(1)

#create HW model
tempHoltMultip <- HoltWinters(tempsTimeSeries, alpha= NULL, beta = NULL, seasonal = "multiplicative")

#view model
tempHoltMultip

#see if smoothing did occur visually
plot(tempHoltMultip)

#fitted model to see seasonality
tempHoltMultip$fitted

#plot fitted and see what level, trend and season look like 
plot(tempHoltMultip$fitted)

#create seasonality matrox
szn <- matrix(tempHoltMultip$fitted[,4],ncol=123)
colnames(szn)<-as.vector(t(temp[,1]))

#1996 not in seasonality 
rownames(szn)<-c(1997:2015)
szn2<-t(szn)

#create seasonality dataframe
datf<-data.frame(matrix(nrow=nrow(szn2),ncol=ncol(szn2)))

#label axis 
colnames(datf)<-colnames(szn2)
rownames(datf)<-rownames(szn2)

for(year in 2:ncol(datf)){
  #intialize St value
  datf[1,year]<-0 
  #get july average as this is the heart of summer
  avgmu <-mean(season[1:31,year-1]) 
  #get standard deviation of july to calculate c and t
  std<-sd(season[1:31,year-1]) 
  #threshold T is 3-5 times the SD so i will use 4
  threshold<-4*std 
  cval <- 1 * std
  #if there is a change i want to denote what date the change it is; intializing this change var
  change<-NULL 
  
  #loop through each row to find cusum for each year of when summer ends per year
  #i used if else loop to catch certain conditions and print different outputs depending on the cusum and apply cusum formula 
  for(i in 2:nrow(datf)){datf[i,year]<-max(0,datf[i-1,year]+(avgmu-season[i,year]-cval))
  #if value is greater than threshold append the date to the change list 
  if (datf[i,year]>=threshold){change<-append(change,datf[i,year])}}
  #if there is no change 
  if (length(change)==0){cat(colnames(datf[year]),"--> summer ends same time as last year \n")
  }else{cat(colnames(datf[year]),"ALERT: change detected")}
}

#QUESTION 8.1
#In participating in this masters programs, along with working, my hope is that after I graduate, both of these independent variables will ultimately be predictors of my salary. In order to entice students to join this program, OMSA can do a linear regression model with Salary as the response variable with explanatory variables as experience, education, program track, gpa etc. in order to show prospective students the effect this program and other characteristics can have on their salary. 

#Question 8.2
#read in data
crime <- read.table("uscrime.txt", header = TRUE)

#create linear model 
crimemod <- lm(Crime ~., data = crime)

#view summary of data 
summary(crimemod)

#create data frame with given values
newdata <- data.frame(M=14, So=0, Ed=10, Po1=12, Po2=15.5, LF=.640, M.F=94, Pop=150, NW=1.1, U1=.120, U2=3.6, Wealth=3200, Ineq=20.1, Prob=.04, Time=39)
#the r^2 value

#pass data frame into predict function to find crime rate 
n <- predict(crimemod, newdata, interval = "confidence")
#view n
n
