setwd("D:/BABI/BABI projects/Tableau")
getwd()

install.packages("WriteXLS")
install.packages("xlsx")
install.packages("openxlsx")
install.packages("mice")
install.packages("VIM")

library(xlsxjars)
library(WriteXLS)
library(openxlsx)
library(xlsx)
library(readxl)
library(mice)
library(VIM)
data= read_xlsx("Car claims for insurance.xlsx")
summary(data)
str(data)
names(data)
colSums(is.na(data))
dim(data)
str(data$YOJ)
data$YOJ<-data$YOJ+2000


names(data)[1]<-"CUSTOMER_ID"
names(data)[2]<-"OWNER_KIDS"
names(data)[4]<-"KIDSatHOME"
names(data)[7]<-"PARENTS"
names(data)[14]<-"PURPOSE_CAR"
names(data)[15]<-"WORTH_CAR"
names(data)[21]<-"AREA_TYPE"
#----------------------------------------------------------
  
data$MSTATUS
data$MSTATUS[data$MSTATUS=="z_No"]<-"No"

data$GENDER
data$GENDER[data$GENDER=="z_F"]<-"F"                   

data$EDUCATION      
data$EDUCATION[data$EDUCATION=="z_High School"]<-"High School"

data$EDUCATION[data$EDUCATION=="<High School"]<-"High School"


data$OCCUPATION
data$OCCUPATION[data$OCCUPATION=="z_Blue Collar"]<-"Blue Collar"

data$CAR_TYPE
data$CAR_TYPE[data$CAR_TYPE=="z_SUV"]<-"SUV"

data$AREA_TYPE
data$AREA_TYPE[data$AREA_TYPE=="z_Highly Rural/ Rural"]<-"Highly Rural/ Rural"

write.xlsx(data,"Car_Insurance.xlsx")

write.csv(data,"Car_Insurance.csv",row.names = TRUE)
WriteXLS::WriteXLS(data,ExcelFileName = "Car_Insurance.XLSX",
                   col.names = TRUE,row.names = FALSE)
WriteXLS()
#----------------------------------------------------------------------
write.xlsx(data,"Car_Insurance.xlsx")
write.xlsx()
----------------------------------------------------------------------  
  md.pattern(data)
TestData.final <- mice(TestData,m=5,maxit=50,defaultMethod = "logreg",
                       seed=500 )
TestData.final$imp
TestData.final <- complete(TestData.final,1)

d<- mice::mice(data,m=5,maxit=20,method='cart',seed=500 )
d$imp
data$OCCUPATION=as.factor(data$OCCUPATION)
d$imp$OCCUPATION
d$imp$YOJ
str(data)
d<- complete(d,1)
dim(d)
colSums(is.na(data))


str(data)
knnImputation(data, k = 10, to.impute = c(5,6,8,12,20),
              using = 1:nrow(data))

knn.impute(data, k = 10, cat.var = c(12),
           to.impute = c(5,6,8,12,20), using = 1:nrow(data))
