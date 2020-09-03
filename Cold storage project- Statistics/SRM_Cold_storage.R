setwd("C:/Users/mitta/Downloads/Cold storage project")
getwd()

install.packages("pwr")

library(readr)
library(Hmisc)
library(stats)
library(pwr)
library(effsize)


storage_temp= read.csv("Cold_Storage_Temp_Data.csv")
str(storage_temp)

#DESCRIPTIVE ANALYSIS:

  
summary(storage_temp)

attach(storage_temp)
describe(storage_temp) # it gives the descriptio of all the variables of our dataset

#To check the outliers in the data set 
boxplot(storage_temp) 
#The boxplots of the four variables show that there are no outliers except in the TEMPERATURE variable.
hist(Temperature)
barchart()
?barchart
qplot(Month,fill=Season,data=storage_temp, geom="bar",
     main = "Month VS Season",
     xlab = "Month",
     ylab = "Season")
qplot(Season, Temperature, data = storage_temp,
      main = "season Vs Temperature",
      xlab = "Season",
      ylab = "Temperature")

library(corrplot)
library(GGally)
ggcorr(storage_temp)
ggcorr(storage_temp, label= TRUE ) 
#This shows that none of the variables have any correlation with each other.



null=is.na(Temperature)
null

1.

?mean
summer_temp = storage_temp[which(Season== "Summer"),"Temperature"]
Winter_temp = storage_temp[which(Season== "Winter"),"Temperature"]
Rainy_temp = storage_temp[which(Season== "Rainy"),"Temperature"]
mean_summer_temp= mean(summer_temp)
mean_Winter_temp= mean(Winter_temp)
mean_Rainy_temp= mean(Rainy_temp)
mean_summer_temp
mean_Winter_temp
mean_Rainy_temp

----------------------------------------------------------------------------------
2.

all_temp= storage_temp[,"Temperature"]
mean_of_entire=mean(all_temp) 
print(paste0("The mean of the entire year is= ",mean_of_entire ))

------------------------------------------------------------------------------------
3.

sd_of_entire= sd(all_temp)
print(paste0("The standard deviation of all the temperatures is ", sd_of_entire))  


------------------------------------------------------------------------------------
4.

np1= pnorm(2,mean = 2.96274,sd= .508589, lower.tail = TRUE  ) 
print(paste0("The probability of the temperature having fallen below 2 deg C is =", np1))

------------------------------------------------------------------------------------
5.
np2= pnorm(4,mean = 2.96274,sd= .508589, lower.tail = FALSE )
print(paste0("The probability of temperature having gone above 4 deg C is =", np2))  

-----------------------------------------------------------------------------------

6.

np3= pnorm(4,mean = 2.96274,sd= .508589, lower.tail = TRUE  )- np1
np3  
NP= np1+np2 # Here, NP is the combined probabilities of the temperature lying outside the range of 2-4 Degrees.
print(paste0("the probability that the temperature will go out of the 2-4 Degrees range is = ",NP))
print(paste0("The percentage will be = ", NP*100))
#this is greater thann 2.5 and less than 5
#The penalty to the AMC will be= 10%

----------------------------------------------------------------------------------------
# 1. 
#    H0= The Temperature was = 3.9 Deg C
#    H1= The Temperature was > 3.9 Deg C

 
read.csv("Cold_Storage_Mar2018.csv")
data= read.csv("Cold_Storage_Mar2018.csv")
str(data)
summary(data)
mean_of_sample= mean(data[,"Temperature"])


z_value = ((mean_of_sample- 3.9)/sd_of_entire)*35^0.5
print(paste0("The COMPUTED Z-Value is= " , z_value))
Z_critical= qnorm(1-0.1)
print(paste0("The CRITICAL Z-Value is= " , Z_critical))


?pnorm
# At ALPHA=1%, the critical Z-value is= 1.2815

...................


p_value1=1-pnorm(z_value)
print(paste0("The P-Value is= ", p_value1))
print("The value of Alpha is= 0.01")
print("The Critical value of Z > the Computed Z,the P-Value > alpha. So, we FAIL TO REJECT THE NULL HYPOTHESIS")
 

# we can also compare by the critical value of the mean, Xc= 4.0100 .This value is derived from the critical value of Zc
#Since our Xc is greater than X. 
#The NULL HYPOTHESIS cannot be rejected.
#The temperature < 3.9 at the cold storage and the PROBLEM IS AT THE RETAILER/OUTLET SITE. 

--------------------------------------------------------------------------------------------------------------

2. 
# H0= The Temperature was = 3.9 Deg C
# H1= The Temperature was > 3.9 Deg C
#   
sd_of_sample= sd(data[,"Temperature"])
print(paste0("The estimated standard deviation of the population is = " , sd_of_sample)) 
mean_of_sample
mean_of_entire
?t.test
print("We are using a direct function to calculate the answers, that is t.test")
t.test(data$Temperature, alternative = "greater", mu= 3.9, conf.level = 0.9)
print(paste0("P-value through t.test()= ",.0047))
T_critical= qt(1-.1/2,34)
print(paste0("The value of the CRITICAL T-Stat is= ", T_critical))
print("Now, we are calculating the values of T-stat manually")
tstat=(mean_of_sample- 3.9)/(sd_of_sample/(35^0.5))
print(paste0("The value of the COMPUTED T-Stat is= ", tstat))
p_value2=1-pt(tstat,34)
print(paste0("The P-Value is = ", p_value2))
print(paste0("The value of Alpha = ", 0.1))
print("The Critical value of T-Stat < the Computed T-Stat,the P-Value < alpha. So, we REJECT THE NULL HYPOTHESIS")


# The NULL HYPOTHESIS IS REJECTED and the temperature at the cold storage is at fault and its temperature is MORE THAN 3.9
-------------------------------------------------------------------------------------------------------
  
3. 

(A z-test uses the population standard error whereas the t-test uses the estimated standard error.
  Thus, the z-test is more accurate and more powerful.)
T_power=power.t.test(35,delta= 0, sd= 0.15,sig.level = .1, type = "one.sample", alternative = "one.sided")



pwr.norm.test(.0742, 35, sig.level = .1, alternative = "greater")




