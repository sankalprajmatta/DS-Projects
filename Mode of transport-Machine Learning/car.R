setwd("D:/BABI/BABI projects/Cars-Mode of transport")
getwd()

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("caTools")
install.packages("rms")
install.packages("ROCR")
install.packages("pROC")
install.packages("Hmisc")
install.packages("DMwR")
install.packages("caret")
install.packages("InformationValue")
install.packages("blorr")
install.packages("ineq")
install.packages("e1071") 
install.packages("xgboost")
install.packages("ipred")
install.packages("rpart")
install.packages("car")


library(ggplot2)
library(readr)
library(dplyr)
library(GGally)
library(caTools)
library(Hmisc)
library(stringi)
library(rms)
library(ROCR)
library(pROC)
library(DMwR)
library(caret)
library(InformationValue)
library(blorr)
library(ineq)
library(class)
library(e1071)
library(xgboost)
library(ipred)
library(rpart)
library(car)

#--------------------------------------------------------------------------
Car= read.csv("Cars_edited.csv")
str(Car)
summary(Car)
dim(Car)
head(Car)
tail(Car)
colnames(Car)
-------------------------------------------------------------------
#UNIVARIATE ANALYISIS
#done for both categorical and continuous variables
  
#must be numeric for histogram plotting
#HISTOGRAM 
hist(Car$Age)
hist(Car$Engineer)
hist(Car$MBA)
hist(Car$Work.Exp)
hist(Car$Salary)
hist(Car$Distance)
hist(Car$license)

#Using Boxplot now
boxplot(Car)
# There are outliers in:Age,Work.Exp, Salary,Distance

#BIVARIATE ANALYSIS
plot(Car$Work.Exp,Car$Salary)
plot(Car$Age,Car$Work.Exp)
plot(Car$Age,Car$Salary)

#MISSING VALUE DETECTION
null= is.na(Car)
summary(null)
colSums(is.na(Car))
Car<- na.omit(Car)
colSums(is.na(Car))
str(Car)

#Dimension
dim(Car)


#Labelling the Dependent Variable
Car$Transport <- ifelse(Car$Transport=="Car",1,0)
Car$Transport <- as.factor(Car$Transport)

Car$Gender <- ifelse(Car$Gender =="Male",1,0)
Car$Gender <- as.factor(Car$Gender)

str(Car)

#-----------------------------------------------------------------------------
# Multicollinearity

ggcorr(Car,label= TRUE)
# There is multicollinearity between 3 variables: Age, Work.Exp and Salary
# Multicollinearity occurs when two or more predictors in a regression
# equation are correlated.
#We should not treat multicollinearity because right now logistic 
#regression does notget affected by it. Although we could perform PCFA
#in order to reduce the variables and group the similar and highly
#correlated variables together to forma single variable.

dim(Car)
head(Car,10)
tail(Car,10)
# We can use Principle Component Analysis
#-------------------------------------------------------

set.seed(848)
table(Car$Transport)
split<- sample.split(Car$Transport,SplitRatio = 0.70)
dim(Car)
trainData <- subset(Car, split == TRUE)
dim(trainData)
testData <- subset(Car, split==  FALSE)
dim(testData)


#SMOTE DATA PREPARATION

#---------------------------------------------------------------------
#  Logistic Resgression
dim(smote.train)

car.logistic <- glm(smote.train$Transport~., data = smote.train,
                    family = "binomial")
summary(car.logistic)

# VARIANCE INFLATION FACTOR
vif(car.logistic)
# The VIF if more than 5 for all the variables except for the 
# Engineer variable.

# Predicting on the Train Data itself
logistic.pred= predict(car.logistic,smote.train)
logit.predict <- ifelse(logistic.pred<.5,0,1)
logit.predict<- as.factor(logit.predict)
caret::confusionMatrix(logit.predict,smote.train$Transport)

#Accuracy : 0.9855
#Sensitivity : 0.9884          
#Specificity : 0.9826


logistic.pred= predict(car.logistic,smote.test)
logit_pred <- ifelse(logistic.pred>.5,1,0)
logit_pred<- as.factor(logit_pred)
caret::confusionMatrix(logit_pred,smote.test$Transport)



#Accuracy : 0.9624
#Sensitivity : 0.9826         
#Specificity : 0.8333

# 15 out of 18 people who travelled by Car were predicted right.


#---------------------------------------------------------------------------
  
#KNN
str(smote.train)

knn_fit <- caret::train(Transport~.,data = smote.train,method = "knn",
                        trControl= trainControl(method = "cv", number = 10),
                        tuneLength = 10)
summary(knn_fit)
# Therefore, we get the value of K to be used as = 5
knn.fit= knn(smote.train[,-9],smote.test[,-9],smote.train[,9],k=5)
knn.fit= as.factor(knn.fit)
caret::confusionMatrix(knn.fit,smote.test[,9])
# Accuracy : 0.9474
# Sensitivity : 0.9478          
# Specificity : 0.9444
# 17 people out of 18 people who travelled by Car were predicted right.
#---------------------------------------------------------------------------
  
  
car.naive= naiveBayes(smote.train$Transport~., data= smote.train)
car.naive
# The average Salary given Car as a means of transport is 12.73 and the standard
# deviation for the same means is 5.40.

naive.predict= predict(car.naive,newdata = smote.test)
caret::confusionMatrix(naive.predict,smote.test$Transport)

#Accuracy : 0.9474 
#Sensitivity : 0.9652          
#Specificity : 0.8333 
#15 people out of 18 were predicted right for using car as a means of transport.

#-------------------------------------------------------------------------

# Bagging

bagmodel= bagging(as.numeric(smote.train$Transport)~.,data= smote.train,
        control= rpart.control(maxdepth = 5,minsplit = 4))
bag.pred<- predict(bagmodel, newdata=smote.test)
table.bag<- table(smote.test$Transport,bag.pred>0.5)
table.bag
# This is a highly overfit model.
# There is no false. All the predictions which are 1, are correctly predicted.
# Hence the 
#sensitivity is (18+0)/18=100%
#Accuracy is = (18+0)/(115+18)=13.533%

#-----------------------------------------------------------------------------

# Boosting

tp_xgb<- vector()
str(smote.train)
str(smote.test)
# setting The Transport Variable of the Train set as numeric
smote.train$Transport=as.numeric(smote.train$Transport)
smote.train$Transport= ifelse(smote.train$Transport==2,1,0)
# setting the other variables od Train Data as numeric 
smote.train$Gender<- as.numeric(smote.train$Gender)

# setting The Transport Variable of the TEST set as numeric
smote.test$Transport=as.numeric(smote.test$Transport)
smote.test$Transport= ifelse(smote.test$Transport==2,1,0)
# setting the other variables od Train Data as numeric 
smote.test$Gender<- as.numeric(smote.test$Gender)

# Converting into Matrix
features_smote.train<-as.matrix(smote.train[,1:8])
label_smote.train<- as.matrix(smote.train$Transport)
features_smote.test= as.matrix(smote.test[,1:8])

# Making an initial Model
xgbmodel<-xgboost(data= features_smote.train, 
                  label = label_smote.train,
                  eta=0.001,
                  max_depth=5,
                  min_child_weight=3,
                  nrounds=1000,
                  nfold=5,
                  objective= "binary:logistic",
                  verbose =0,
                  early_stopping_rounds =100)
    
xgb.predict= predict(xgbmodel,newdata=features_smote.test)                  
xgb.pred= ifelse(xgb.predict>0.5,1,0)   
tab_xgb=table(xgb.pred,smote.test$Transport)
sum(diag(tab_xgb))/nrow(smote.test)
# The accuracy is 93.2

# Finding the Best Model(Tuning XGB Model)

# Making the FIT Model for the "max_depth"
car_xgb<- vector()
md<- c(1,3,5,7,9,15)

for(i in md)
{
  xgb.fit<-xgboost(data= features_smote.train, 
                    label = label_smote.train,
                    eta=0.001,
                    max_depth=md,
                    min_child_weight=3,
                    nrounds=1000,
                    nfold=5,
                    objective= "binary:logistic",
                    verbose =0,
                    early_stopping_rounds =100)

smote.test$xgb.pred= predict(xgb.fit,features_smote.test)
car_xgb<- cbind(car_xgb,sum(smote.test$Transport==1 & smote.test$xgb.pred>0.5))
}

car_xgb

# Since we get the same values in all the iterations , we select the least value
# of max_depth= 1

# Making the next fit model for the "eta"
car_xgb2<-vector()
lr<- c(0.001,0.01,0.1,0.3,0.5,0.7,1)
for(i in lr)
{
  xgb.fit<-xgboost(data= features_smote.train, 
                   label = label_smote.train,
                   eta=i,
                   max_depth=1,
                   min_child_weight=3,
                   nrounds=1000,
                   nfold=5,
                   objective= "binary:logistic",
                   verbose =1,
                   early_stopping_rounds =100)
  
  smote.test$xgb.pred= predict(xgb.fit,features_smote.test)
  car_xgb2<- cbind(car_xgb2,sum(smote.test$Transport==1 & smote.test$xgb.pred>0.5))
}

car_xgb2
# As the number of true positives at eta=1 is the highest =17, we'll select
# eta=1

# Making the next fit model for the "nrounds" 
car_xgb3<- vector()
nr<-c(2,10,50,100,1000,10000)
for(i in nr)
{
  xgb.fit<-xgboost(data= features_smote.train, 
                   label = label_smote.train,
                   eta=0.001,
                   max_depth=5,
                   min_child_weight=3,
                   nrounds=nr,
                   nfold=5,
                   objective= "binary:logistic",
                   verbose =1,
                   early_stopping_rounds =100)
  
  smote.test$xgb.pred= predict(xgb.fit,features_smote.test)
  car_xgb3<- cbind(car_xgb3,sum(smote.test$Transport==1 & smote.test$xgb.pred>0.5))
}
car_xgb3

# Since we get the same values in all the iterations , we select the least value
# of nrounds= 10

# FINAL MODEL
xgbmodel.final<-xgboost(data= features_smote.train, 
                  label = label_smote.train,
                  eta=1,
                  max_depth=1,
                  min_child_weight=3,
                  nrounds=100,
                  nfold=5,
                  objective= "binary:logistic",
                  verbose =0,
                  early_stopping_rounds =100)

xgb.predict= predict(xgbmodel.final,newdata=features_smote.test)                  
xgb.pred= ifelse(xgb.predict>0.5,1,0)   
tab_xgb=table(xgb.pred,smote.test$Transport)
sum(diag(tab_xgb))/nrow(smote.test)

