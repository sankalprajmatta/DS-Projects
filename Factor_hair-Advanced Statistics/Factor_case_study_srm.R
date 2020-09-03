setwd("C:/Users/mitta/Downloads/Factor_hair-AdvancedS")
getwd()

library(GGally)
library(readr)
library(Hmisc)
library(stats)
library(pwr)
library(pastecs)
library(psych)


hair= read.csv("Factor-Hair-Revised.csv")
str(hair)

#DESCRIPTIVE ANALYSIS:


summary(hair)


attach(hair)
names(hair)
describe(hair)
dim(hair) 
stat.desc(hair)
head(hair,n=10)
tail(hair,n=10)

# OUTLIER DETECTION
boxplot(hair)
#The outiers are present int the given attributes: Ecom , S0alesFImage, OrdBilling, Delspeed

# Checking for null values

null=is.na(hair)
null
summary(null)# Shows there are no null values

# UNIVARIATE ANALYSIS
hist(hair)
boxplot(hair)

#BIVARIATE ANALYSIS
#These plots are being made after seeing the correlation matrix and thus, figuring out which variables are 
# to an extent related to each other and then we can visualize their relationship.
plot(ProdLine,OrdBilling, col='blue')
plot(CompRes,DelSpeed,col='red')
plot(Ecom,SalesFImage, col= 'green') 
plot(TechSup,WartyClaim, col= 'purple')



str(hair)
------------------------------------------------------------------------------------------------
2.

#CHECKING FOR MULTICOLINERAITY
library(corrplot)
ggcorr(hair[,2:13],label= TRUE)


# we see that the dependent variable 'Satisfcation' is variably dependent on the various other 
#independent variables
# There is high co-relation amongst various pairs of variables showing multicollinearity.
# We'll test for multicollinearity using multiple-regression too.

model= lm(Satisfaction~ProdQual+Ecom+TechSup+CompRes+Advertising+ProdLine+SalesFImage+ComPricing+WartyClaim+
            OrdBilling+DelSpeed)
summary(model)
# we see that the amalgamated  effect of each one upon the other makes the explanatory power
#extremely difficult.
# There are 3 variables which are significantly affecting the satisfaction -
# Product Quality , Ecommerce, Salesforce Image
# and we cannot say if Product Quality is more significant than Ecommerce or Salesforce Image .
# This shows that there is multicolinearlity becasue there are multiple variables which are affecting the
# dependent variable Satisfaction. Multicollinearity is not a problem.

-----------------------------------------------------------------------------------------------
3. 

#simple linear regression for the dependent variable with every independent variable

model1= lm(Satisfaction~ProdQual)
summary(model1) # 23.65% of the changes in Satisfcation are explained by Product Quality 

model2= lm(Satisfaction~Ecom)
summary(model2) # 7.99% of the changes in Satisfcation are explained by E-Commerce

model3= lm(Satisfaction~TechSup)
summary(model3) # 1.26% of the changes in Satisfcation are explained by Technical Support

model4= lm(Satisfaction~CompRes)
summary(model4) # 36.39% of the changes in Satisfcation are explained by Complaint Resolution

model5= lm(Satisfaction~Advertising)
summary(model5) # 9.2% of the changes in Satisfcation are explained by Advertising

model6= lm(Satisfaction~ProdLine)
summary(model6) # 30.31% of the changes in Satisfcation are explained by Product Line 

model7= lm(Satisfaction~SalesFImage)
summary(model7) # 25.02% of the changes in Satisfcation are explained by Salesforce Image

model8= lm(Satisfaction~ComPricing)
summary(model8) # 4.33% of the changes in Satisfcation are explained by Competitive Pricing

model9= lm(Satisfaction~WartyClaim)
summary(model9) # 3.15% of the changes in Satisfcation are explained by Warranty and Claims

model10= lm(Satisfaction~OrdBilling)
summary(model10) # 27.22% of the changes in Satisfcation are explained by Order and billing

model11= lm(Satisfaction~DelSpeed)
summary(model11) # 33.3% of the changes in Satisfcation are explained by Delivery Speed


---------------------------------------------------------------------------------------------------

  
4.
# Perform PCA/Factor analysis by extracting 4 factors . Interpret the output and name the Factors

# We will find the correlation matrix so as to find a pure number ebteween -1 and +1 so as to have an idea 
# interrelations between the pairs of variables
# we will perform PCA by using the independent variables exclusing the dependent variable. we will thus reduce
# the dimensionality of the dataset and converge the 11 variables into as less as 4-5 factors which are able to 
# explain those variables as a generalization
library(psych)

Hair_factor= hair[,2:12]
str(Hair_factor)

ev= eigen(cor(Hair_factor))
print(ev,digits= 5)

eigenvalue= ev$values
eigenvalue
# These eigen values are in decsending order

factor= c(1,2,3,4,5,6,7,8,9,10,11)

# We will now make a SCREE PLOT of the factors and the eigen values

scree= data.frame(factor,eigenvalue)

plot(scree, main= "Scree Plot", col= "Blue",
     ylim= c(0,4))
lines(scree, col="Red")
# We observe that there are 4 significant factors according to Kaiezer Rule, rest factor will be discarded

# First, we will create an UNROTATED MATRIX and scrutinize the factor loadings and the communality 

unrotate= principal(Hair_factor, nfactors = 4, rotate="none")
print(unrotate)

# the picture isn't very clear

rotate= principal(Hair_factor,nfactors = 4, rotate = "verimax")
print(rotate)

# Communality H2 tells the ability of all the 4 factors to capture as much of the variance of the variables
# as possible.

fa= fa(Hair_factor, nfactors = 4, rotate = "none",fm= "pa")
print(fa)
fa.diagram(fa)

# Thus, we know know the vraiables which belong to a generalized component factor.
# DelSpeed + CompRes + OrdBilling + ProdLine belong to PA1 and by pondering about the characteristics of
# these variables, we understand that we can rename that component as = Orders_and_Service

# Salesforce Image + E-Commerce + Advertising + Competitive Pricing belong to PA2 and by contemplating about the 
# characteristics of these variables, we understand that we can rename that component as= Marketising_Quality

# TechSup + WartyClaim belong to PA3 and we understand that 
# we can rename that component as= Support_Assistance

# ProdQual is an indepdent factor and will remian as it is and doesn't belong to any of the generalized groups

PCA_hair= fa$scores
PCA_hair

# Now , we will combine the columns of the original dataset and this PCA_hair dataset and create
# a new dataset by the name= new_hair containing 5 columns.

library(dplyr)
new_hair= cbind(PCA_hair, hair[,13])
new_hair= as.data.frame(new_hair)
str(new_hair)


colnames(new_hair)[1]<- 'Orders_and_Service'
colnames(new_hair)[2]<- 'Marketising_Quality'
colnames(new_hair)[3]<- 'Support_Assistance'
colnames(new_hair)[4]<- 'Product_Quality'
colnames(new_hair)[5]<- 'Customer Satisfaction'

str(new_hair)
attach(new_hair)  # This is the new dataset with 5 columns and 100 rows
----------------------------------------------------------------------------------------
  
5. 
#Perform Multiple linear regression with customer satisfaction as dependent variables 
#and the four factors as independent variables. 

multimodel= lm(Satisfaction~Orders_and_Service+Marketising_Quality+Support_Assistance+Product_Quality, 
               data = new_hair)

summary(multimodel)
anova(multimodel)

# Yhat= 6.918 + .902x1 + .129x2 + .0026x3 + .5158x4


# multipled R Squared : .6971 implies 69.71% of the variations in Customer Satisfaction is explained
# the independent variables namely : Orders_and_Service , Marketising_Quality , Support_Assistance ,
# and Product_Quality

# The Significant independant factors are : Orders_and_Service , Marketising_Quality and Product_Quality 
# and the non-significant factor is Support_Assistance because it only explains 2.65% of the changes and 
# it can also be neglected leaving us with 3 independant factors derived from factor analysis

prediction= predict(multimodel)
actual= Satisfaction
backtrack= data.frame(actual,prediction)
plot(actual, col= 'red')
lines(actual, col='red')
plot(prediction, col='blue')
lines(prediction, col='blue')

# We can clearly infer that the prediction falls majoritarily in line with the actual data.

# Now we'll check for the confidance level

confint(multimodel, level = 0.95)
?confint()

# Therefore we will not take the slopes which are extracted by Multiple Regression because we are 
# conservative and will take only the slopes corresponding to the 2.5% of the predicted levels.
# As we concluded that the Support_Assistance didn't contribute much to the predictions we can neglect it
# a dn still teh model will be valid.

multimodel2= lm(Satisfaction~Orders_and_Service+Marketising_Quality+Product_Quality, data = new_hair)
summary(multimodel2)

# We see that in absence of the factor Support_Assistance doesn't affect the variance explaining feature
# of model and thus , we can also suffice with the three factors.

# These 3 factors will explain the variance in the satisfaction 69.71 % . 