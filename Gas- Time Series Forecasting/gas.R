
install.packages("forecast")
install.packages("tseries")
install.packages("dygraphs")
install.packages("TTR")
install.packages("xts")
install.packages("TSstudio")

library(forecast)
library(tseries)
library(dygraphs)
library(TTR)
library(xts)
library(TSstudio)

data<- forecast::gas
data

str(data)
View(data)
summary(data)
class(data)
gas = window(data, start=c(1970,1), end= c(1995,8))
plot(gas)


#--------------------------------------------------------------------------
  
  #Visualizing the time series
plot.ts(gas)

monthplot(gas)

ts_seasonal(gas,type = "all")

boxplot(gas~cycle(gas), main = "Boxplot for Gas Dataset")
#----------------------------------------------------------------------------
  
# Decomposition of the time series into its different components
  
decomp_gas= stl(gas_production, s.window = "periodic")
plot(decomp_gas)
# Here, we see that Trend plays a very big role in the making of this 
# time series. The Residuals are also a significant part if our Time series.
# But there is no seasonality component in here. Therefore, we can say that only 
# Trend and residual component are present in our time series.
  


#----------------------------------------------------------------------
  
# Checking the Periodicity of the Time-series data
 
periodicity(gas)
   
  
#-------------------------------------------------------------------------
  
  
# Check the Stationarity of the Series
  
# Using Augmented Dickey Fuller Test
# Formation of The Hypothesis of Augmented Dickey Fuller Test
  
#   Null Hypothesis :      H0: The time series is Non-Stationary
#  Alternate Hypothesis:  H1: The time series is Stationary
  adf.test(gas, alternative = "stationary",k=12)
# The p-value comes oput to be .84 which is much bigger than the Alpha of 0.05. 
# Therefore, we fail to reject the null hypothesis of the series being non_stationary.
# We'll accept the Null Hypothesis stating that the time series is 
# non-staionary.
  
  # Using Visual Interpretaion from the Decomposed plots
  
plot(decomp_gas)
  
# Had the series been stationary, there would have been no Trend or Seasonality.
# But there is a presence of Trend in the series. Therefore, also visually
# talking , the series is not stationary.

# Therefore, we don't need to deseasonlize the time series because its already 
# unaffected by the seasonal component.

# The series is non-stationary.

# Checking the Correaltions 
acf(gas, lag.max = 100)
pacf(gas,lag.max = 20)

#--------------------------------------------------------------------
# Converting the time series into Stationary series.
# using Differencing of the data

diff_gas= diff(log(gas), differences = 1)
dygraph(diff_gas)  
adf.test(diff_gas, alternative = "stationary",k=12)  


# here, we see that the p-value is much lowwer than our alpha of 0.05. 
# Therefore, we know thta if P-value is less than Alpha, then we reject the Null
# hypothesis and accept the laterantive Hypothesis of the series being 
# Stationary.

# Hence, by doing the differencing og the time series one time, we are able to convert the 
# non-stationary time series into stationary time-series.

# Now, we'll find out the ACF and PACF for the differenced time series.
acf(diff_gas,main="ACF for Difference Time Series")
pacf(diff_gas,main="PACF for Difference Time Series")  



# From here , we'll take q=3 as ACf value gives us the value of q to be taken.
# we'll take p=2 as PACF gives us the value of p to be taken.

# Splitting the Time series data set into Train and Test parts.
# We'' use the data from 1970 inthe Training part .
# We'll use the data from 1994 for the test data .
train_gas= window(diff_gas,end= c(1993,12))
dygraph(train_gas)
test_gas = window(diff_gas, start=c(1994,1))
dygraph(test_gas)

acf(train_gas,lag.max = 20)
pacf(train_gas,lag.max = 20)
# Therfore, the value of p= 2, q=3

# Creating Manual Arima Models
# The manual arima takes as argumemts the values of acf , pcaf and 
# differencing in the order of c(p,d,q)

# With p=2 nd q=3
manual_arima= arima(exp(train_gas), order = c(2,0,3))
summary(manual_arima)

# Aic= -763 

manual_fit= fitted(manual_arima)
ts.plot(exp(train_gas), manual_fit, col= c("red","blue"))

# Interpretation of the Residuals of the mManual Arima Model

hist(manual_arima_final$residuals,col = "purple")

ggtsdisplay(residuals(manual_arima_final), lag.max = 25, main = "Model Residuals")


# Performaning L-jung Box-Test

H0 : Residuals are independent
H1 : Residuals are not independent

Box.test(manual_arima$residuals)

#----------------------------------------------------------------------

# Applying Auto Arima

auto_arima= auto.arima(exp(train_gas),seasonal = TRUE)
summary(auto_arima)

# Aic = -795.07


auto_fit<- fitted(auto_arima)

ts.plot(exp(train_gas), auto_fit, col= c("red","blue"))
#-------------------------------------------------------------------------

# Interpreting The Residuals of the Auto Arima Model
ggtsdisplay(residuals(auto_arima),lag.max = 10, main = "Residuals")

hist(auto_arima$residuals,col = "purple")


# Performaning L-jung Box-Test

H0 : Residuals are independent
H1 : Residuals are not independent


Box.test(auto_arima$residuals)

# Forecasting with arima model

# Manual Arima Forecasting

manual_forecast= forecast(manual_arima,h=20)
plot(manual_forecast)
accuracy(manual_forecast,exp(test_gas))

#  The Accuracy on:
# Train Data= 4.610
# Test Data= 5.573

test_forecast(forecast.obj = manual_forecast,actual = exp(diff_gas),
              test = exp(test_gas))

# Auto Arima Forecasting

auto_forecast = forecast(auto_arima,h=20)
plot(auto_forecast)
accuracy(auto_forecast,exp(test_gas))

#  The Accuracy on:
# Train Data= 3.8726
# Test Data= 5.261

test_forecast(forecast.obj = auto_forecast,actual = exp(diff_gas),
              test = exp(test_gas))  

#-----------------------------------------------------------------------
# The auto arima performs better than manual Arima
# Forecasting on 12 months ahead
final= forecast(auto_arima,h=32)
plot(final)
ts.plot(exp(train_gas),auto_fit,col=c("red","blue"))
plot(exp(train_gas),col=c("blue"))
plot(auto_fit,col=c("blue"))


test_forecast(forecast.obj = auto_forecast,actual = exp(diff_gas),test = exp(test_gas))

