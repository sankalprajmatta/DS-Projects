setwd("D:/BABI/Capstone/Loan Default")
getwd()


library(readxl)
library(party)
library(corrplot)
library(GGally)
library(grDevices)
library(mice)
library(caret)
library(esquisse)
library(DataExplorer)
library(desc)
library(funModeling)
library(vcd)
library(DMwR)
library(xgboost)
library(caTools)
library(rpart)
library(rpart.plot)
library(car)
library(e1071)
library(Boruta)
library(class)
library(ROCR)
library(Metrics)
library(ineq)
library(randomForest)


data<- read_xlsx("loan default_data.xlsx")
data=as.data.frame(data)
str(data)
summary(data)
colSums(is.na(data))
describe(data)
head(data)
tail(data)
dim(data)

#----------------------------------------------------------------
#  Data Visualisation

# Bivariate Analysis
str(ndata)
mat1= cor(ndata[,-25])
corrplot::corrplot(mat1, method = "number" ,type= "lower", number.cex=0.7)

ggcorr(data,label = TRUE)



esquisse::esquisser(data)

# Bivariate
# Verification Status VS Annual Income
ggplot(data) +
  aes(x = verification_status, weight = annual_inc) +
  geom_bar(fill = "#440154") +
  labs(x = "Verification Status", y = "income") +
  theme_minimal()



# Delinquincy VS Total Accounts
ggplot(data) +
  aes(x = delinq_2yrs, y = total_acc) +
  geom_point(size = 1.04, colour = "#440154") +
  theme_minimal()


# Home Ownership VS Loan Status according to Annual income
ggplot(data) +
  aes(x = home_ownership, fill = loan_status, weight = annual_inc) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal()

# Home Ownership VS Funded amount invested according to Loan Status
ggplot(data) +
  aes(x = home_ownership, y = funded_amnt_inv, fill = loan_status) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()

# Funded amount invest by Grade
ggplot(data) +
  aes(x = funded_amnt_inv, fill = grade) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal()

# Open account by Grade
ggplot(data) +
  aes(x = open_acc, fill = grade) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal()



boxplot(data4$total_acc~data4$loan_status)
boxplot(data4$total_rec_prncp~data4$loan_status)
boxplot(data4$emp_length~data4$loan_status)
boxplot(data4$total_rec_late_fee~data4$loan_status)

# Univariate Analysis

# plotting categorical variables

ggplot(data) +
  aes(x = term) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = grade) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = emp_length) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = home_ownership) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = verification_status) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = purpose) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = addr_state) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = application_type) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data) +
  aes(x = loan_status) +
  geom_bar(fill = "#0c4c8a") +
  theme_minimal()

# date variables

ggplot(data2) +
  aes(x = earliest_cr_line) +
  geom_histogram(bins = 30L, fill = "#482878") +
  theme_minimal()

ggplot(data2) +
  aes(x = last_credit_pull_d) +
  geom_histogram(bins = 30L, fill = "#482878") +
  theme_minimal()

ggplot(data2) +
  aes(x = last_pymnt_d) +
  geom_histogram(bins = 30L, fill = "#482878") +
  theme_minimal()

ggplot(data2) +
  aes(x = issue_d) +
  geom_histogram(bins = 30L, fill = "#482878") +
  theme_minimal()

boxplot(data4[,c(14:15)])
boxplot(data4[,c(16)])
boxplot(data4[,c(17:18)])
boxplot(data4[,c(19:22)])

plot_histogram(ndata[,c(1:9)])
plot_histogram(ndata[,c(10:17)])
plot_histogram(ndata[,c(18:19)])

barplot(data$grade)


# ---------------------------------------------------------------
# Descriptive Analysis

names(data)
str(data)
summary(data)
colSums(is.na(data))
# Some unnecessary variables are to be removed
# -> Variables with more than 50% missing values:
# 1. Description 2. Months since last delinquincy 3.Next payment date

# -> Variables which have suppercollinearity and are almost identical
# 1. Loan amount 2. Funded amount 3. out_principal 4. Total payment

# -> Variables which have only 1 same value thorught the records
# 1. Recoveries 2. Collection recovery fee 3. Application Type

data1= data[,-c(1,15,22,38,2,3,27,29,34,35,40)]

str(data1)

#---------------------------------------------------------------------

# Combining some of the levels of purpose , as they contribute less
library(forcats)
data1$purpose=as.factor(data1$purpose)
levels(data1$purpose)
data1$purpose=fct_collapse(data1$purpose, others = c("other","car","educational","house",
                                       "major_purchase","medical","moving",
                                       "renewable_energy","small_business",
                                       "vacation","wedding"))

# Variable Transformation for Character Variables

# One hot-encoding to make Dummy Variables
#=========================================

# 1.TERM 2.Home OWNERSHIP 3. VERIFICATION STATUS

data2=fastDummies::dummy_cols(data1,select_columns = 
                                c("term","home_ownership",
                                  "verification_status","purpose"))
names(data2)
# Covernting into factors 
#=========================
# 1. Grade

data2$grade=as.factor(data2$grade)



# Covernting into numeric
#=========================


# 1. Payment plan

data2$pymnt_plan[which(data2$pymnt_plan=="n")]= 0
data2$pymnt_plan[which(data2$pymnt_plan=="y")]= 1
data2$pymnt_plan=as.numeric(data2$pymnt_plan)

# 2. Emp_length


data2$emp_length[which(data2$emp_length== "< 1")]<- 0
data2$emp_length[which(data2$emp_length== "10+")]<-10 
data2$emp_length[which(data2$emp_length== "n/a")]<-NA

data2$emp_length= as.numeric(data2$emp_length)
sum(is.na(data2$emp_length))




# Conversion of the Target variable into a factor
data2$loan_status[which(data2$loan_status== "Fully Paid")]<-0
data2$loan_status[which(data2$loan_status== "Default")]<-1
data2$loan_status=as.factor(data2$loan_status)

prop.table(table(data2$loan_status))

#data1$loan_status=as.numeric(data1$loan_status)

# removing the original variables after Dummy creation
names(data2)
data3= data2[,-c(2,7,9,11,12,13,36)]
str(data3)

# Missing Value Treatment

sum(is.na(data3))
data4= na.omit(data3)

# making grade into numeric too
data4$grade=as.numeric(data4$grade)


# CONVERTING POSIXCT TO NUMERIC

#1. Converting the POSIXCT type variable into DATE type

data4$issue_d=as.Date(data4$issue_d)
data4$earliest_cr_line=as.Date(data4$earliest_cr_line)
data4$last_pymnt_d=as.Date(data4$last_pymnt_d)
data4$last_credit_pull_d=as.Date(data4$last_credit_pull_d)

# converting to data type and then as numeric
data4$issue_d=as.numeric(data4$issue_d)
data4$earliest_cr_line=as.numeric(data4$earliest_cr_line)
data4$last_pymnt_d=as.numeric(data4$last_pymnt_d)
data4$last_credit_pull_d=as.numeric(data4$last_credit_pull_d)
#-------------------------------------------------------------------
str(data4)
names(data4)
# CHECK MULTICOLLINEARITY FOR THE DATA


matrix=cor(data4[,-24])
corrplot::corrplot(matrix, method = "number" ,
                   type= "lower", number.cex=0.7)

# outliers treatment

# Making separate dataset containing only numerical data

nums <- unlist(lapply(data4, is.numeric))  
ndata= data4[,nums]
names(ndata)



# Outliers Treatment of numerical dataframe

outdata = ndata 

a=c(1:35)

for(val in a){
  qnt<- quantile(outdata[,val],probs = c(0.25,0.75))
  cap<- quantile(outdata[,val],probs = c(0.05,0.95))
  
  h= 1.5*IQR(outdata[,val])
  outdata[,val][outdata[,val]>(qnt[2]+h)]<- cap[2]
  outdata[,val][outdata[,val]<(qnt[1]-h)]<- cap[1]
}

boxplot(outdata[,29:35])

# Combining the outliers treated data with the rest of the data
finaldata= cbind(data4$loan_status,outdata)
str(finaldata)
colnames(finaldata)[1]="loan_status"

names(finaldata)  
# checking the boxplot after otlier treatment
boxplot(finaldata$funded_amnt_inv~ finaldata$loan_status)
boxplot(finaldata[,c(9:21)])
boxplot(finaldata[,c(22:35)])
print(profiling_num(data),digits=2)

# 
# Checking the multicollinarity after outlier treatment


matrix=cor(finaldata[,-1])
corrplot::corrplot(matrix, method = "number" ,
                   type= "lower", number.cex=0.7)

# removing the "total_rec_late_fee " variable as its 
# standard deviation is zero.
finaldata= finaldata[,-21]

#--------------------------------------------------------
# WORKING ON MODEL CREATION 

#------------------------------------------------------------
  # NORMALIZATION
  

scaledata=scale(finaldata[,-1])
class(scaledata)
apply(scaledata, 2, mean)
apply(scaledata,2,sd)
scaledata=as.data.frame(scaledata)
str(scaledata)
finaldata1=cbind(scaledata,finaldata$loan_status)
str(finaldata1)
colnames(finaldata1)[35]="loan_status"
str(finaldata1)



#-------------------------------------------------------------------
#dividng the data into train and test
finaldata=finaldata1 
split= sample.split(finaldata$loan_status, SplitRatio = 0.80)

train= subset(finaldata,split==TRUE)
valdata= subset(finaldata,split==FALSE)
dim(train)
dim(valdata)
p1=prop.table(table(train$loan_status))
p2=prop.table(table(valdata$loan_status))
str(train)
p1
p2
# balancing using SMOTE

class(train)
smotetrain= SMOTE(loan_status~., train)
prop.table(table(smotetrain$loan_status))
dim(smotetrain)

str(finaldata)

#-------------------------------------------------------------------
# FOR FINDING VARAIBLE IMPORTANCE
# BORUTA

set.seed(123)
boruta.train <- Boruta(smotetrain$loan_status~., data = smotetrain,
                       doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

# All our variables are declared important
#--------------------------------------------------------------------
# XGBOOST


features_train<- as.matrix(smotetrain[,c(1:34)])
label_train<- as.matrix(smotetrain$loan_status)

features_valdata= as.matrix(valdata[,c(1:34)])



dim(valdata)
dim(smotetrain)
attach(smotetrain)

xgbmodel.final<-xgboost(data= features_train, 
                        label = label_train,
                        eta=.3,
                        max_depth=10,
                        min_child_weight=5,
                        nrounds=100,
                        nfold=50,
                        objective= "binary:logistic",
                        verbose =0,
                        early_stopping_rounds =100)
summary(xgbmodel.final)

# ACCURACY on TRAIN DATA
xgb.predictT= predict(xgbmodel.final,newdata=features_train,type=response)                  
xgb.predT= ifelse(xgb.predictT>0.5,1,0) 
xgb.predT=as.factor(xgb.predT)
caret::confusionMatrix(xgb.predT,smotetrain$loan_status)

#ACCURACY on VALIDATION DATA

xgb.predictV= predict(xgbmodel.final,newdata=features_valdata,type=response)                  
xgb.predV= ifelse(xgb.predictV>0.5,1,0) 
xgb.predV=as.factor(xgb.predV)
caret::confusionMatrix(xgb.predV,valdata$loan_status)


mat=xgb.importance(feature_names = colnames(smotetrain), model = xgbmodel.final)
print(mat)



#----------------------------------------------------------------------
  
# CART

ctrl= rpart.control(minsplit=100, xval=10) 
random_model= rpart(smotetrain$loan_status~., data= smotetrain,
                    control= ctrl)
caret::varImp(random_model)
varImpPlot(random_model)
# Accuracy for the TRAIN set
rand.predict_train= predict(random_model, newdata= smotetrain,type= "class" )
caret::confusionMatrix(rand.predict,smotetrain$loan_status)

# Accuracy for the TEST set
rand.predict_test= predict(random_model, newdata= valdata,type= "class" )
caret::confusionMatrix(rand.predict_test,valdata$loan_status)

# VARIABLE IMPORTANCE
caret::varImp(random_model)
#------------------------------------------------------------------------

# RANDOM FOREST
randomforest_model= randomForest(smotetrain$loan_status~., data= smotetrain,
                    control= ctrl)
summary(randomforest_model)
# Accuracy for the TRAIN set
randforest.predictT= predict(randomforest_model, newdata= smotetrain,type= "class" )
caret::confusionMatrix(randforest.predictT,smotetrain$loan_status)

# Accuracy for the TEST set
randforest.predictV= predict(randomforest_model, newdata= valdata,type= "class" )
caret::confusionMatrix(randforest.predictV,valdata$loan_status,,positive="1")

# VARIABLE IMPORTANCE
vr=caret::varImp(randomforest_model)
varImpPlot(randomforest_model, 10)

plot(randomforest_model)
#---------------------------------------------------------------------

# logistic regression
names(smotetrain)

log_model= glm(smotetrain$loan_status~., data=smotetrain,
              family=binomial())
summary(log_model)
vif(log_model)

# Accuracy for the TRAIN set

log.predictT= predict(log_model,smotetrain,type="response")
log.predT= ifelse(log.predictT>0.5,1,0) 
log.predT=as.factor(log.predT)
caret::confusionMatrix(log.predT,smotetrain$loan_status)

# Accuracy for the TEST set
log.predictV= predict(log_model,valdata,type="response")
log.predV= ifelse(log.predictV>0.5,1,0) 
log.predV=as.factor(log.predV)
caret::confusionMatrix(log.predV,valdata$loan_status)


imp=varImp(log_model, scale= FALSE)
print(imp)
plot(imp)




#--------------------------------------------------------------------

#KNN



# accuracy for the TRAIN set
knn.fit= knn(smotetrain[,-35],smotetrain[,-35],smotetrain[,35],k=5)
knn.fitT= as.factor(knn.fit)
caret::confusionMatrix(knn.fitT,smotetrain$loan_status)

# Accuracy for the TEST set
knn.fit1= knn(smotetrain[,-35],valdata[,-35],smotetrain[,35],k=5)
knn.fitV= as.factor(knn.fit1)
caret::confusionMatrix(knn.fitV,valdata$loan_status)

#---------------------------------------------------------------------

# NAIVE BAYES

naive_model= naiveBayes(smotetrain$loan_status~., data= smotetrain)
summary(naive_model)

# Accuracy for the TRAIN set
naive.predT= predict(naive_model,newdata = smotetrain)
caret::confusionMatrix(naive.predT,smotetrain$loan_status)


# Accuracy for the TEST set
naive.predV= predict(naive_model,newdata = valdata)
caret::confusionMatrix(naive.predV,valdata$loan_status)

#---------------------------------------------------------------------

# SUPPORT VECTOR MACHINE

svm_model = svm(formula = smotetrain$loan_status ~ ., 
                 data = smotetrain, 
                 type = 'C-classification', 
                 kernel = 'linear') 
summary(svm_model)
# Accuracy for the TRAIN set
svm.predT = predict(svm_model, newdata = smotetrain[,-35]) 
caret::confusionMatrix(svm.predT,smotetrain$loan_status)

# Accuracy for the TEST set
svm.predV = predict(svm_model, newdata = valdata[,-35]) 
caret::confusionMatrix(svm.predV,valdata$loan_status)


#-------------------------------------------------------------------
# LDA

control <- trainControl(method="repeatedcv", number=10, repeats=3)
lda_model <- train(loan_status~., data=smotetrain,
                   method="lda",trControl=control)
summary(lda_model)
# Accuracy for the TRAIN set
lda.predT = predict(lda_model, newdata = smotetrain[,-35]) 
caret::confusionMatrix(lda.predT,smotetrain$loan_status)

# Accuracy for the TEST set
lda.predV = predict(lda_model, newdata = valdata[,-35]) 
caret::confusionMatrix(lda.predV,valdata$loan_status)

importance(lda_model)

#====================================================================
#                    COMPARISONS OF THE MODELS
#====================================================================

# ********** ON THE BASIS OF MSE, MAE, RMSE, R2 values ****************
#_____________________________________________________________________

# 1. XGBOOST


sm=as.numeric(smotetrain$loan_status)
sm= ifelse(sm==1,0,1)
vd= as.numeric(valdata$loan_status)
vd= ifelse(vd==1,0,1)

# TRAIN
x1= xgb.predictT

fmse = mse(x1, sm)
fmae = MAE(x1, sm)
frmse = RMSE(x1, sm)
fr2 = R2(x1, sm, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

# TEST

x2= xgb.predictV

fmse = mse(x2, vd)
fmae = MAE(x2, vd)
frmse = RMSE(x2, vd)
fr2 = R2(x2, vd, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)


# 2. RANDOM FOREST

# TRAIN
r1= as.numeric(randforest.predictT)
r1= ifelse(r1==1,0,1)

fmse = mse(r1, sm)
fmae = MAE(r1, sm)
frmse = RMSE(r1, sm)
fr2 = R2(r1, sm, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

# TEST

r2= as.numeric(randforest.predictV)
r2= ifelse(r2==1,0,1)

fmse = mse(r2, vd)
fmae = MAE(r2, vd)
frmse = RMSE(r2, vd)
fr2 = R2(r2, vd, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

# 3. Logistic Regression

# TRAIN
fmse = mse(log.predictT, sm)
fmae = MAE(log.predictT, sm)
frmse = RMSE(log.predictT, sm)
fr2 = R2(log.predictT, sm, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

#TEST
fmse = mse(log.predictV, vd)
fmae = MAE(log.predictV, vd)
frmse = RMSE(log.predictV, vd)
fr2 = R2(log.predictV, vd, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

# 4. KNN 

#TRAIN
k1= as.numeric(knn.fit)
k1= ifelse(k1==1,0,1)

fmse = mse(k1, sm)
fmae = MAE(k1, sm)
frmse = RMSE(k1, sm)
fr2 = R2(k1, sm, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

#TEST
k2= as.numeric(knn.fit1)
k2= ifelse(k2==1,0,1)
fmse = mse(k2, vd)
fmae = MAE(k2, vd)
frmse = RMSE(k2, vd)
fr2 = R2(k2, vd, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)


# 5. NAIVE BAYES

#TRAIN
n1= as.numeric(naive.predT)
n1= ifelse(n1==1,0,1)

fmse = mse(n1, sm)
fmae = MAE(n1, sm)
frmse = RMSE(n1, sm)
fr2 = R2(n1, sm, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)


#TEST
n2= as.numeric(naive.predV)
n2= ifelse(n2==1,0,1)

fmse = mse(n2, vd)
fmae = MAE(n2, vd)
frmse = RMSE(n2, vd)
fr2 = R2(n2, vd, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)



# 6. SVM

#TRAIN
s1= as.numeric(svm.predT)
s1= ifelse(s1==1,0,1)

fmse = mse(s1, sm)
fmae = MAE(s1, sm)
frmse = RMSE(s1, sm)
fr2 = R2(s1, sm, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

#TEST

s2= as.numeric(svm.predV)
s2= ifelse(s2==1,0,1)
fmse = mse(s2, vd)
fmae = MAE(s2, vd)
frmse = RMSE(s2, vd)
fr2 = R2(s2, vd, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)


# 7. LDA

#TRAIN
l1= as.numeric(lda.predT)
l1= ifelse(l1==1,0,1)

fmse = mse(l1, sm)
fmae = MAE(l1, sm)
frmse = RMSE(l1, sm)
fr2 = R2(l1, sm, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)

#TEST
l2= as.numeric(lda.predV)
l2= ifelse(l2==1,0,1)

fmse = mse(l2, vd)
fmae = MAE(l2, vd)
frmse = RMSE(l2, vd)
fr2 = R2(l2, vd, form = "traditional")

cat(" MAE:", fmae, "\n", "MSE:", fmse, "\n", 
    "RMSE:", frmse, "\n", "R-squared:", fr2)


#____________________________________________________________________

#******************** ROC-AUC and K-S Statistic *********************
#____________________________________________________________________

# 1. XGBOOST

#TRAIN
xgp= prediction(xgb.predictT,smotetrain$loan_status)
perf= performance(xgp,"tpr","fpr")
plot(perf,colorize=TRUE)
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(xgb.predictT,type = "Gini")
gini.train.knn 

#TEST

xgp= prediction(xgb.predictV,valdata$loan_status)
perf= performance(xgp,"tpr","fpr")
plot(perf,colorize=TRUE)
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(xgb.predictV,type = "Gini")
gini.train.knn 

#RANDOM FOREST

#TRAIN
rfp= prediction(r1,smotetrain$loan_status)
perf= performance(rfp,"tpr","fpr")
plot(perf,colorize=TRUE )

auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(r1,type = "Gini")
gini.train.knn 

#TEST

rfp= prediction(r2,valdata$loan_status)
perf= performance(rfp,"tpr","fpr")
plot(perf,colorize=TRUE )
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(r2,type = "Gini")
gini.train.knn 

# LOGISTIC 

#TRAIN
lrp= prediction(log.predictT,smotetrain$loan_status)
perf= performance(lrp,"tpr","fpr")
plot(perf,colorize=TRUE )

auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(log.predictT,type = "Gini")
gini.train.knn 

#TEST

lrp= prediction(log.predictV,valdata$loan_status)
perf= performance(lrp,"tpr","fpr")
plot(perf,colorize=TRUE )
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(log.predictV,type = "Gini")
gini.train.knn 

# KNN


#TRAIN
knnp= prediction(k1,smotetrain$loan_status)
perf= performance(knnp,"tpr","fpr")
plot(perf,colorize=TRUE )

auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(k1,type = "Gini")
gini.train.knn 

#TEST

knnp= prediction(k2,valdata$loan_status)
perf= performance(knnp,"tpr","fpr")
plot(perf,colorize=TRUE )
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(k2,type = "Gini")
gini.train.knn 

# NAIVE BAYES


#TRAIN
nbp= prediction(n1,smotetrain$loan_status)
perf= performance(nbp,"tpr","fpr")
plot(perf,colorize=TRUE)

auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(n1,type = "Gini")
gini.train.knn 

#TEST

nbp= prediction(n2,valdata$loan_status)
perf= performance(nbp,"tpr","fpr")
plot(perf,colorize=TRUE )
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(n2,type = "Gini")
gini.train.knn 

# SUPPORT VECTOR MACHINE


#TRAIN
svmp= prediction(s1,smotetrain$loan_status)
perf= performance(svmp,"tpr","fpr")
plot(perf,colorize=TRUE )

auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(s1,type = "Gini")
gini.train.knn 

#TEST

svmp= prediction(s2,valdata$loan_status)
perf= performance(svmp,"tpr","fpr")
plot(perf,colorize=TRUE )
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(s2,type = "Gini")
gini.train.knn 

# LINEAR DISCRIMANT ANALYSIS

#TRAIN
ldap= prediction(l1,smotetrain$loan_status)
perf= performance(ldap,"tpr","fpr")
plot(perf,colorize=TRUE )

auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(l1,type = "Gini")
gini.train.knn 

#TEST

ldap= prediction(l2,valdata$loan_status)
perf= performance(ldap,"tpr","fpr")
plot(perf,colorize=TRUE )
auc.perf=performance(ROCRpred,"auc")
as.numeric(auc.perf@y.values)

logis_KS= max(perf@y.values[[1]]-perf@x.values[[1]]) 
logis_KS

gini.train.knn = ineq(l2,type = "Gini")
gini.train.knn 

#-------------------------------------------------------------------

esquisse::esquisser(data)

str(finaldata1)
