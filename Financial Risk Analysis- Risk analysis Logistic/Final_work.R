# Setting up the path

setwd("D:/BABI/BABI projects/Financial Risk Analysis")
getwd()

#Importing the libraries

library(readxl)
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
library(DMwR)
library(caret)
library(randomForest)
library(varImp)
library(mice)
library(MASS)
library(psych)
library(StatMeasures)
library(tidyverse)
library(ggplot2)

data= read_xlsx("raw-data.xlsx")  
attach(data)
colnames(data)=make.names(colnames(data))

# Decriptive Analysis
str(data)
dim(data)
summary(data)
colSums(is.na(data))

head(data,10)
tail(data,10)
names(data)

------------------------------------------------------------------------------
# Exploratory Analysis

# Univariate Analysis

hist(data[2])
hist(data[c(3,5)])
hist(data[30])
hist(data[1:12])
hist(data[13:24])
hist(data[25:36])
hist(data[37:41])
plot(data[,42])

# Bivariate Analysis

# Total Assets vs Cash Profit
ggplot(data) +
  aes(x = `Total assets`, y = `Cash profit`) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()

# Total Assets vs Net Working Capital

ggplot(data) +
  aes(x = `Total assets`, y = `Net working capital`) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()

# Total Liabilities vs Borrowings
ggplot(data) +
  aes(x = `Total liabilities`, y = Borrowings) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()

# Sales Vs Cumulative Retained Profits

ggplot(data) +
  aes(x = Sales, y = `Cumulative retained profits`) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_minimal()

# Cash Profit vs Current Liabilities and provisions

ggplot(data) +
  aes(x = `Cash profit`, y = `Current liabilities & provisions`) +
  geom_line(size = 1.76, colour = "#0c4c8a") +
  theme_minimal()

# Total Capital vs Total Liabilities

ggplot(data) +
  aes(x = `Total liabilities`, y = `Total capital`) +
  geom_point(size = 2L, colour = "#0c4c8a") +
  theme_minimal()

# Total Assests vs Total Liabilities

ggplot(data) +
  aes(x = Sales, y = `Total expenses`) +
  geom_line(size = 1.62, colour = "#0c4c8a") +
  theme_minimal()
plot(data$`Total assets`,data$`Total liabilities`)
#----------------------------------------------------------------------------
  # Outlier Detection
  
boxplot(data[,(1:26)])
boxplot(data[,(27:41)])

  # Missing value Detection

colSums(is.na(data))
#------------------------------------------------------------------------------
# Creating a Default variable from the 'Net worth next year' variable
data$default= ifelse(`Networth Next Year`<0,1,0)
names(data)
anyNA(data)


#cor.test(dataM$default, dataM$`Total income`, method="pearson")
#REMOVING THE VARIABLES WITH HIGH MISSING VALUES AND SOME WHICH AREN'T AS
# IMPORTANT BY BANK'S PERSPECTIVE
#VARIABLES REMOVED INITIALLY = 1,2,6,18,19,22,25,32,34,42:48,52(INDEX)
# 'Num', 'Networth Next Year', 'Change in Stock','Income from Financial Services',
# 'Other income','Deposits accepted by commercial banks','Investments',
# 'Creditors Turnover','Debtors Turnover','Finished Goods Turnover',
# 'Wip Turover' 'Raw Material Turnover','Equity Face Value','PE on BSE'


dataM= data[,-c(1,2,6,18,19,22,25,32,34,42:48,52)]
names(dataM)

# There are some variables which have super collinearity with each other
# we will make them into ratios and remove two more variables;
# Ratios created  :1. Sales/Total Expenses 2. Total income/Total Assets

dataM$Sales.per.TotalExpenses=dataM$Sales/dataM$Total.expenses
dataM$Totalincome.per.TotalAssets= dataM$Total.income/dataM$Total.assets

# VAriables removed after these ratios creation - Totaal income, Total expenses
# Other 2 variables are important and will be used to create further new ratios

dataM=dataM[,-c(3,4)]
colSums(is.na(dataM))
dim(dataM)
#leaving us with 36 variables now including default

# Missing Value imputation usimg MICE

NAdata= mice(dataM,meth="pmm",seed=848,maxit = 30,m=5)
NAdata$imp
NAdata$loggedEvents
NAdata <- complete(NAdata)

colSums(is.na(NAdata))
names(NAdata)
dim(NAdata)
# Studying the multicollinearity

mat1= cor(NAdata)
corrplot::corrplot(mat1, method = "number" ,type= "lower", number.cex=0.7)

#-------------------------------------------------------------------------------
  # CREATING NEW VARIABLES FOR THE RAW DATASET
  
Mdata=NAdata
names(Mdata)

#Ratios for Profitability
Mdata$CashProfit.per.TotalAssets= Mdata$Cash.profit/Mdata$Total.assets
Mdata$Cum.Profit.per.Sales= Mdata$Cumulative.retained.profits/Mdata$Sales
Mdata$Sales.per.TotalAssets <- Mdata$Sales/Mdata$Total.assets
Mdata$Sales.per.CurentAssets= Mdata$Sales/Mdata$Current.assets
Mdata$PBT.per.TotalCaptital= Mdata$PBT/Mdata$Total.capital
Mdata$PAT.per.TotalAssets= Mdata$Profit.after.tax/Mdata$Total.assets
Mdata$PAT.per.Sales= Mdata$Profit.after.tax/Mdata$Sales
Mdata$PAT.per.TotalCapital= Mdata$Profit.after.tax/Mdata$Total.capital
Mdata$PAT.per.TotalAssets= Mdata$Profit.after.tax/Mdata$Total.assets
Mdata$PBT.per.TotalAssests= Mdata$PBT/Mdata$Total.assets
Mdata$PBT.per.Sales= Mdata$PBT/Mdata$Sales 
Mdata$PBT.per.TotalCapital= Mdata$PBT/Mdata$Total.capital
Mdata$PBDITA.per.Sales= Mdata$PBDITA/Mdata$Sales 
#Ratios for Leverage 
Mdata$Borrowings.per.TotalAssets= Mdata$Borrowings/Mdata$Total.assets
Mdata$Borrowings.per.TotalLiabilities= Mdata$Borrowings/Mdata$Total.liabilities
Mdata$Borrowings.per.TotalCapital= Mdata$Borrowings/Mdata$Total.capital
Mdata$CurrentLiability.per.CurrentAssets= Mdata$Current.liabilities...provisions/Mdata$Current.assets
Mdata$CurrentLiability.per.TotalAssets= Mdata$Current.liabilities...provisions/Mdata$Total.assets
#Ratios for Liquidity
Mdata$NetFixedAssets.per.TotalLiabilities= Mdata$Net.fixed.assets/Mdata$Total.liabilities
Mdata$Cum.RetainedProfits.per.CurrentAssets= Mdata$Cumulative.retained.profits/Mdata$Current.assets
Mdata$Networth.per.TotalLiabilities= Mdata$Net.worth/Mdata$Total.liabilities
Mdata$Cum.RetainedProfit.per.TotalLiabilities= Mdata$Cumulative.retained.profits/Mdata$Total.liabilities
Mdata$PAT.per.CurrentLiability <- Mdata$Profit.after.tax/Mdata$Current.liabilities...provisions
Mdata$CashProfit.per.CurrentLiabilities= Mdata$Cash.profit/Mdata$Current.liabilities...provisions
Mdata$TotalCapital.per.TotalLiabilities= Mdata$Total.capital/Mdata$Total.liabilities
Mdata$TotalCapital.per.CurrentLiabilities= Mdata$Total.capital/Mdata$Current.liabilities...provisions
#Ratios for Size
Mdata$ShareholderFunds.per.TotalAssets= Mdata$Shareholders.funds/Mdata$Total.assets
Mdata$Captialemployed.per.Networth= Mdata$Capital.employed/Mdata$Net.worth
Mdata$Reserves.per.TotalCapital= Mdata$Reserves.and.funds/Mdata$Total.capital
Mdata$ShareholderFunds.per.TotalLiability= Mdata$Shareholders.funds/Mdata$Total.liabilities
Mdata$ShareholderFunds.per.TotalCapital= Mdata$Shareholders.funds/Mdata$Total.capital
Mdata$capitalEmp..per.ShareholderFunds= Mdata$Capital.employed/Mdata$Shareholders.funds
Mdata$NetFixedAssets.per.CurrentAssets= Mdata$Net.fixed.assets/Mdata$Current.assets
Mdata$NetFixedAssets.per.TotalAssets= Mdata$Net.fixed.assets/Mdata$Total.assets

Mdata$Sales.per.TotalAssets <- Mdata$Sales/Mdata$Total.assets
Mdata$PAT.per.CurrentLiability <- Mdata$Profit.after.tax/Mdata$Current.liabilities...provisions
Mdata$Sales.per.CurentAssets= Mdata$Sales/Mdata$Current.assets
Mdata$ShareholderFunds.per.TotalAssets= Mdata$Shareholders.funds/Mdata$Total.assets
Mdata$Borrowings.per.TotalAssets= Mdata$Borrowings/Mdata$Total.assets
Mdata$Borrowings.per.TotalLiabilities= Mdata$Borrowings/Mdata$Total.liabilities
Mdata$Borrowings.per.TotalCapital= Mdata$Borrowings/Mdata$Total.capital
Mdata$Cum.RetainedProfit.per.TotalLiabilities= Mdata$Cumulative.retained.profits/Mdata$Total.liabilities
Mdata$CashProfit.per.CurrentLiabilities= Mdata$Cash.profit/Mdata$Current.liabilities...provisions
Mdata$CashProfit.per.TotalAssets= Mdata$Cash.profit/Mdata$Total.assets
Mdata$Cum.Profit.per.Sales= Mdata$Cumulative.retained.profits/Mdata$Sales
Mdata$Captialemployed.per.Networth= Mdata$Capital.employed/Mdata$Net.worth
Mdata$Reserves.per.TotalCapital= Mdata$Reserves.and.funds/Mdata$Total.capital
Mdata$PBT.per.TotalCaptital= Mdata$PBT/Mdata$Total.capital
Mdata$PAT.per.TotalAssets= Mdata$Profit.after.tax/Mdata$Total.assets
Mdata$PAT.per.Sales= Mdata$Profit.after.tax/Mdata$Sales
Mdata$PAT.per.TotalCapital= Mdata$Profit.after.tax/Mdata$Total.capital
Mdata$PAT.per.TotalAssets= Mdata$Profit.after.tax/Mdata$Total.assets
Mdata$Networth.per.TotalLiabilities= Mdata$Net.worth/Mdata$Total.liabilities
Mdata$PBT.per.TotalAssests= Mdata$PBT/Mdata$Total.assets
Mdata$PBT.per.Sales= Mdata$PBT/Mdata$Sales 
Mdata$PBT.per.TotalCapital= Mdata$PBT/Mdata$Total.capital
Mdata$TotalCapital.per.TotalLiabilities= Mdata$Total.capital/Mdata$Total.liabilities
Mdata$TotalCapital.per.CurrentLiabilities= Mdata$Total.capital/Mdata$Current.liabilities...provisions
Mdata$CurrentLiability.per.CurrentAssets= Mdata$Current.liabilities...provisions/Mdata$Current.assets
Mdata$CurrentLiability.per.TotalAssets= Mdata$Current.liabilities...provisions/Mdata$Total.assets
Mdata$ShareholderFunds.per.TotalLiability= Mdata$Shareholders.funds/Mdata$Total.liabilities
Mdata$ShareholderFunds.per.TotalCapital= Mdata$Shareholders.funds/Mdata$Total.capital
Mdata$capitalEmp..per.ShareholderFunds= Mdata$Capital.employed/Mdata$Shareholders.funds
Mdata$Cum.RetainedProfits.per.CurrentAssets= Mdata$Cumulative.retained.profits/Mdata$Current.assets
Mdata$NetFixedAssets.per.CurrentAssets= Mdata$Net.fixed.assets/Mdata$Current.assets
Mdata$NetFixedAssets.per.TotalLiabilities= Mdata$Net.fixed.assets/Mdata$Total.liabilities
Mdata$NetFixedAssets.per.TotalAssets= Mdata$Net.fixed.assets/Mdata$Total.assets
Mdata$PBDITA.per.Sales= Mdata$PBDITA/Mdata$Sales

# Correlation matrix

cor.test(Mdata$default,Mdata$ShareholderFunds.per.TotalLiability,
         method="pearson")

big.data= Mdata
names(big.data)
anyNA(big.data)
big.data=na.omit(big.data)

mat= cor(big.data)
corrplot::corrplot(mat,method = "number", type = "lower",
                   number.cex = 0.7)
#----------------------------------------------------------------------------
  # OUTLIER TREATMENT
  # Outlier correction
  #Using FLOORING AND CIELING CAPPING
outdata=big.data
names(outdata)
anyNA(outdata)

a=c(1:33,35:69)

for(val in a){
  qnt<- quantile(outdata[,val],probs = c(0.25,0.75))
  cap<- quantile(outdata[,val],probs = c(0.10,0.85))
  
  h= 1.5*IQR(outdata[,val])
  outdata[,val][outdata[,val]>(qnt[2]+h)]<- cap[2]
  outdata[,val][outdata[,val]<(qnt[1]-h)]<- cap[1]
}

boxplot(outdata)
anyNA(outdata)
names(outdata)
dataset1=outdata


#------------------------------------------------------------------------------
  # Finding out the significant variables
  
dataset1=outdata   # outlier treated data


names(dataset1)

dim(dataset1)

# Removing the variables by VIF by building the model

dataset1= dataset1[,-c(1:6,12:19,31,32,42,48,50,62,64,68)]
m1=  glm(dataset1$default~.,data=dataset1,family = binomial())
summary(m1)  
vif(m1)

mat=cor(dataset1)
corrplot::corrplot(mat,method = "number", type = "lower",
                   number.cex = 0.7)


# further reducing the variables using stepAIC to give the minimum AIC value

stepAIC(m1,direction = "backward")



train= outdata[,c(9,11,24,28,30,34,35,36,38,44,45,46,47,49,
                51,53,54,56,58,61,66)]

names(train)
mat=cor(train)
corrplot::corrplot(mat,method = "number", type = "lower",
                   number.cex = 0.7)


#----------------------------------------------------------------------------    
# SMOTE data preparation

names(train)
dim(train)

train$default= as.factor(train$default)
smote.train= SMOTE(train$default~., train)

names(smote.train)
dim(smote.train)

#Checking the proportion of the Default in SMOTE data

table(smote.train$default)
prop.table(table(smote.train$default))

train$default=as.numeric(train$default)

# Checking the Collinearity Graph
mat=cor(train)
corrplot::corrplot(mat, method="number",type = "lower", number.cex = 0.7)


# Creating the FINAL model using the SMOTE DATA

FinalModel=  glm(smote.train$default~.,data=smote.train,family = binomial())
summary(FinalModel)  
vif(FinalModel)

# Predictions on the train data set itself

pred= predict(FinalModel,smote.train)
pred.train= ifelse(pred<0.5,0,1)

# Converting the Default and the pred.train into Factors
smote.train$default=as.factor(smote.train$default)
pred.train= as.factor(pred.train)

# Creating a Confusion Matrix for the predictions on the Train Data

caret::confusionMatrix(smote.train$default,pred.train,positive='1')

#---------------------------------------------------------------------------
  # PREPARING THE VALIDATION DATA SET

# Reading the Validation dataset into test 
test= read_xlsx("validation_data.xlsx")
names(test)
anyNA(test)

# Removing the basic variables which are majoritarily missing
test= test[,-c(1,6,18,19,22,25,32,34,42:48,52)]
dim(test)
colnames(test)= make.names(colnames(test))

# Creating Two new ratios( same as we did for the Raw dataset)
test$Sales.per.TotalExpenses=test$Sales/test$Total.expenses
test$Totalincome.per.TotalAssets= test$Total.income/test$Total.assets
False
# Removing Total Income and Total expenses as we have 
# used them already in our ratios
test=test[,-c(5,4)]
colSums(is.na(test))
# Imputing missing values into test set before creating new variables
testmice=mice(test,method = "pmm",m=5,maxit=10,seed=8488)
test=complete(testmice)
anyNA(test)
# creating variables

# Ratios for Profitability
# Ratios for Size
# Ratios for Liquidity
# Ratios for Leverage


test$Sales.per.TotalAssets <- test$Sales/test$Total.assets
test$PAT.per.CurrentLiability <- test$Profit.after.tax/test$Current.liabilities...provisions
test$Sales.per.CurentAssets= test$Sales/test$Current.assets
test$ShareholderFunds.per.TotalAssets= test$Shareholders.funds/test$Total.assets
test$Borrowings.per.TotalAssets= test$Borrowings/test$Total.assets
test$Borrowings.per.TotalLiabilities= test$Borrowings/test$Total.liabilities
test$Borrowings.per.TotalCapital= test$Borrowings/test$Total.capital
test$Cum.RetainedProfit.per.TotalLiabilities= test$Cumulative.retained.profits/test$Total.liabilities
test$CashProfit.per.CurrentLiabilities= test$Cash.profit/test$Current.liabilities...provisions
test$CashProfit.per.TotalAssets= test$Cash.profit/test$Total.assets
test$Cum.Profit.per.Sales= test$Cumulative.retained.profits/test$Sales
test$Captialemployed.per.Networth= test$Capital.employed/test$Net.worth
test$Reserves.per.TotalCapital= test$Reserves.and.funds/test$Total.capital
test$PBT.per.TotalCaptital= test$PBT/test$Total.capital
test$PAT.per.TotalAssets= test$Profit.after.tax/test$Total.assets
test$PAT.per.Sales= test$Profit.after.tax/test$Sales
test$PAT.per.TotalCapital= test$Profit.after.tax/test$Total.capital
test$PAT.per.TotalAssets= test$Profit.after.tax/test$Total.assets
test$Networth.per.TotalLiabilities= test$Net.worth/test$Total.liabilities
test$PBT.per.TotalAssests= test$PBT/test$Total.assets
test$PBT.per.Sales= test$PBT/test$Sales 
test$PBT.per.TotalCapital= test$PBT/test$Total.capital
test$TotalCapital.per.TotalLiabilities= test$Total.capital/test$Total.liabilities
test$TotalCapital.per.CurrentLiabilities= test$Total.capital/test$Current.liabilities...provisions
test$CurrentLiability.per.CurrentAssets= test$Current.liabilities...provisions/test$Current.assets
test$CurrentLiability.per.TotalAssets= test$Current.liabilities...provisions/test$Total.assets
test$ShareholderFunds.per.TotalLiability= test$Shareholders.funds/test$Total.liabilities
test$ShareholderFunds.per.TotalCapital= test$Shareholders.funds/test$Total.capital
test$capitalEmp..per.ShareholderFunds= test$Capital.employed/test$Shareholders.funds
test$Cum.RetainedProfits.per.CurrentAssets= test$Cumulative.retained.profits/test$Current.assets
test$NetFixedAssets.per.CurrentAssets= test$Net.fixed.assets/test$Current.assets
test$NetFixedAssets.per.TotalLiabilities= test$Net.fixed.assets/test$Total.liabilities
test$NetFixedAssets.per.TotalAssets= test$Net.fixed.assets/test$Total.assets
test$PBDITA.per.Sales= test$PBDITA/test$Sales

# Making a new validation set containing the finally selected columns
names(test)
valdata= test[,c(10,12,25,29,31,35,36,38,44,45,46,47,49,51,53,54,56,58,61,66)]
names(valdata)
names(train)
anyNA(valdata)
# adding default to validation test
valdata$default= as.factor(test$Default...1)


# checking that the structure of the Train and the test is the same
dim(valdata)
dim(smote.train)

# Predicting on the TEST
pred= predict(FinalModel,valdata)
predicted= ifelse(pred<0.4,0,1)


# Converting into factor

predicted=as.factor(predicted)
actual= valdata$default


# Making a Confusion Matrix for the Validation set
caret::confusionMatrix(predicted,actual,positive='1')


# Creating a column of Probability of Default
valdata$Probability.of.Default=pred

#------------------------------------------------------------------------------
# arranging the data in ascending order depending upon the propabilty of default



# Arranging in Descending order

valdata=valdata %>% arrange(desc(valdata$Probability.of.Default))


#creating Deciles
# Type 1
probs= seq(0,1 ,length=11)
dec= quantile(valdata$Probability.of.Default,probs,na.rm = TRUE)  
valdata$Deciles= cut(valdata$Probability.of.Default, unique(dec),
                     lowest.include=TRUE,right = FALSE)
names(valdata)
table(valdata$Deciles)



# Type 2
valdata <- valdata %>% mutate(quartile = ntile(-valdata$Probability.of.Default,10))
View(valdata)

valdata$Deciles=valdata$quartile
defaulter<- data.table::data.table(valdata) 
defaulter
valdata_decile <- defaulter[,list(`# Defaulter` <- sum(default==1),
                                  Total  <-length(default)) , by = Deciles][order(Deciles)]  
valdata_decile

#-----------------------------------------------------------------------------


