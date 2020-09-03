setwd("F:/BABI/BABI projects/CellPhone Churn")
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
install.packages("stringi")

library(ggplot2)
library(readr)
library(dplyr)
library(GGally)
library(caTools)
library(rms)
library(ROCR)
library(pROC)
library(stringi)
library(Hmisc)

#1.1
cellphone= read.csv("Cellphone.csv")
str(cellphone)
attach(cellphone)
colnames(cellphone)[1]<- 'Churn'
str(cellphone)


#UNIVARIATE ANALYISIS
#done for both categorical and continuous variables

#must be numeric for histogram plotting
#HISTOGRAM
hist(DataUsage)
hist(AverageFee)
hist(MonthlyCharge)
hist(RoamMins)
hist(DayMins)
#average fee , Roam Mins are normally distributed

#Using Boxplot now
boxplot(cellphone)
# There are outliers in - Accountweeks, DayMins, DayCalls, MonthlyCharge, AverageFee,RoamMins

#BIVARIATE ANALYSIS
plot(DayMins,DayCalls)

qplot(DataPlan,fill=DataUsage,data=cellphone, geom="bar",
 main = "Dataplan Vs Data Usage",
 xlab = "Data Plan",
 ylab = "Data Usage")

plot(DataUsage,MonthlyCharge)
plot(DataUsage,AverageFee)
plot(RoamMins,MonthlyCharge)

----------------------------------------------------------------------------------------
  1.2

# OUTLIER DETECTION
#- USING BOXPLOTS

boxplot(cellphone)
# There are outliers in - Accountweeks, DayMins, DayCalls, MonthlyCharge, AverageFee,RoamMins

#MISSING VALUE DETECTION
null= is.na(cellphone)
summary(null)
#OR
colSums(is.na(cellphone))

#there are no missing values in our dataset

summary(cellphone)
dim(cellphone)
head(cellphone,10)
tail(cellphone,10)

----------------------------------------------------------------------------------------
  1.3


ggcorr(cellphone,label=TRUE)
#There is multicollenearity between 5 variables- Data Plan, Monthly Usage, Monthly Charge,
#Data Usage, Data mins
#Multicollinearity occurs when two or more predictors in a regression equation are correlated.

#We should not treat multicollinearity because right now logistic regression does not
#get affected by it. Although we could perform PCFA in order to reduce the variables and
#group the similar and highly correlated variables together to forma single variable.

----------------------------------------------------------------------------------------
str(cellphone)
colnames(cellphone)
  1.4

#The data consists of all numeric and integer variables. 
#There is the issue of multicollinearity amongst the variable
#There are outliers in certain variable rows
#There are no missing values in the data set
#The predictor variables/independent variables are:
 # "AccountWeeks"    "ContractRenewal" "DataPlan"  "DataUsage"   
 # "CustServCalls"   "DayMins"         "DayCalls"        
 #"MonthlyCharge"   "AverageFee"      "RoamMins"       
#The Dependent variable is : Churn

--------------------------------------------------------------------------------
  
  2.1 APPLYING LOGISTIC REGRESSION

set.seed(1000)
split = sample.split(cellphone$Churn, SplitRatio = .70)

cell.train= subset(cellphone, split== TRUE)
cell.test= subset(cellphone,split== FALSE)    

dim(cell.train)
dim(cell.test)
prop.table(table(cellphone$Churn))
 
#LOGISTIC REGRESSION MODEL

train.lm1= glm(Churn~., data = cell.train,family = binomial )
summary(train.lm1)

# We see that AccountWeeks, ContractRenewal,DataPlan, CustServCalls and RoamMins
# are significant predictor of Churn.

# We will make another logistic regression model, this time without Data Usage
# Monthly charge(these two are highly correlated variables)

train.lm2= glm(Churn~AccountWeeks+ContractRenewal+DataPlan+CustServCalls+
               DayMins+DayCalls+AverageFee+RoamMins, data = cell.train,
               family = binomial)
summary(train.lm2)
# We see that:
# 1.ContractRenewal,DataPlan, CustServCalls, DayMins,AverageFee and RoamMins 
# are significant predictors of Churn.
# 2.We see that except for AccountWeeks and DayCalls , 
# everyother variable is significant.So, removing these 2 variables

#**Therefore, we create a final Regression model 
train.lm.final= glm(Churn~ContractRenewal+DataUsage+CustServCalls+DayMins
                    + AverageFee+RoamMins,data=cell.train,family = binomial())

summary(train.lm.final)

# VARIANCE INFLATION FACTOR
vif(train.lm)
#This shows that the vraiables have multicollinearity problem.
#DataPlan , DAtaUsage, DayMins, MonthlyCharge, and AverageFee are the correlated variables.
# The Relationship netween the variables has lead to a an inflation in their variances
#which means an instability in the coefficient itself.

plot(DataUsage,MonthlyCharge)
#this plot shows how closely related are these variables to each other.

---------------------------------------------------------------------------------
# LIKELIHOOD
 
train.lm.final$coefficients
likelihood<- exp(train.lm.final$coefficients)
likelihood
# 1.6873 is the ODDS RATIO for CustServCalls
# We can interpret that for 1 unit increase in CustServCalls , the churn 
#increases by .6873%

# Predictions on the Test Set now

# FOR Model 1:

predict.test= predict(train.lm1, type = "response", newdata = cell.test)
predict.test

#Confusion matrix with threshold Of 0.5 for MODEL1

table(cell.test$Churn,predict.test>0.5)
832/(832+23)
26/(119+26)

# For Model Final:

predict.test= predict(train.lm.final, type = "response", newdata = cell.test)
predict.test

#Confusion matrix with threshold Of 0.5
# The first argument is Rows and the second argument is the columns

table(cell.test$Churn,predict.test>0.5)
833/(833+22)
28/(117+28)


# ACCURACY

#The SENSITIVITY is= 97.30 %
#The SPECIFICITY is= 17.93 %


#WE trade off sensitivity with specificity

table(cell.test$Churn,predict.test>0.45)
#The SENSITIVITY is= 94.85 %
828/(828+27)
#The SPECIFICITY is= 29.45 %
33/(112+33)

# AUC
# AUC is done by examining the ROC curve
#(ROC shows the continumm of sensitivities and specificities)

train.lm.final$fitted.values
plot(cell.train$Churn,train.lm.final$fitted.values)
roc(cell.test$Churn,predict.test)
plot(roc(cell.test$Churn,predict.test))
# Area under ROC curve is 80.39%

#  OR

ROCRpred= prediction(predict.test,cell.test$Churn)
perf= performance(ROCRpred,"tpr","fpr")
plot(perf)
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)


# 1.a good ROC curve is one that accomplishes both high Senitivity
# and high specificity)
# 2.area under the curve is= 80.39%. 
# The ROC is kindof a counterpart for determining the quality of fit of the model.
# 3.If i take a churn and non-churn at random, there is an 80.39 percent 
# chance that my model will indicicate a higher risk of Churn than Non-Churn.

---------------------------------
  # KS
  
  
# The Kolmogorov Smirnov statistic is the max difference between the 
# TP and the FP rate.
  #H0: The predicted and the actual values belong to the same distribution
  #H1: The predicted and the actual values aren't alike/similiar
logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS
 #The P-value is = .50110, which is greater than alpha of .05. 
we fail to reject the null hypothesis that and therefore, the predictions on the 
test set are quite similar to the actual test set itself.

-------------------------------------------------------------------
  
#GINI 
  # a GINI coefficient meansure the inequality among values of a frequency
  # distribution.
  # A gini of 0 means perfect equality, where all values are the same.
  
 logis_GINI=2*auc.perf@y.values[[1]]-1
 logis_GINI
 # Therefore, innthis case, GINI might not be the best measure to check for
 logistic regression.
 