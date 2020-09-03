setwd("D:/BABI/BABI projects/Market And Retail Analytics")
getwd()
install.packages("arules")
install.packages("readxlsb")
install.packages("arulesViz")
library(arulesViz)
library(readxlsb)
library(arules)
library(GGally)
library(readxl)
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
library(rpart.plot)
library(class)
library(e1071)
library(xgboost)
library(ipred)


data= read_xlsx("Cafe_for_R.xlsx")
str(data)
attach(data)
`Bill Number`=as.factor(`Bill Number`)

data.agg= split(data$`Item Desc`,data$`Bill Number`)
head(data.agg)
data.agg

data.agg2= list()
for( i in 1: length(data.agg)){
  data.agg2[i]=unique(data.agg[i])}

head(data.agg2)

#--------------------------------
#  creating rules

# most frequrnty brought items
trans= as(data.agg2,"transactions")
summary(trans)
inspect(trans[1:10])






freq= itemFrequency(trans)
freq= freq[order(-freq)]
freq["NIRVANA HOOKAH SINGLE"]
barplot(freq[1:10], col = "purple")

itemFrequencyPlot(trans,topN=10, popCol= "red")
itemFrequencyPlot(trans,support=0.03)
?itemFrequencyPlot
rules= apriori(data=trans,
               parameter = list(support=0.001,
                                confidence=0.08,
                                maxlen=2))

inspect(sort(rules,by="lift"))
head(quality(rules))


plot(rules, measure = "support", shading = "lift",
     interactive = FALSE)


