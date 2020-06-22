#####################################################################################
##                                                                                 ##
##                Handling Time Series & Regression Based Forecasting              ##
##                                                                                 ##
#####################################################################################
# Install the required packages and load the libraries
install.packages("forecast")
library(forecast)

# Load the data
getwd()
setwd("/Users/mjchoi/Documents/DMBA-Assignment2")
souvenir.data <- read.csv("SouvenirSales.csv")
View(souvenir.data)

#####################################################################################
## 1. Construct a time series using all the sales data.                            ##
##    Re-create the plots shown in Figure 17.17 below.                             ##
#####################################################################################
# Create a time series for monthly sales in Australian dollars using ts()
sales.ts <- ts(souvenir.data$Sales, start = c(1995, 1), end = c(2001, 12), freq = 12)
# Plot the series
plot(sales.ts, xlab = "Time", ylab = "Sales (Australian $)", ylim = c(0, 110000))

# Create a time series for monthly sale in log-scale object using ts() 
log.sales.ts <- ts(souvenir.data$Sales, start = c(1995, 1), end = c(2001, 12), freq = 12)
# Plot the series using log()
plot(log(sales.ts), xlab = "Time", ylab = "log(Sales)", ylim = c(7, 12))


#####################################################################################
## 2. Based on the two time series plots what components should be included within ##
##    a regression model to capture the details observed?                          ##
#####################################################################################
# Answer:
# The plots do not show U-shaped or exponential curves, but increasing linear trend.
# Also there is a pattern of consistently high or low seasonal values. 
# Therefore, linear trend with seasonality should be included. 


#####################################################################################
## 3. Partition the data into training and validation sets using the details       ##
##    provided in the scenario, as described in the Overview section.              ##
#####################################################################################
# Validation set contains the last 12 months of data (year 2001).
nValid <- 12

# Training partition will be from first observation up to the last record before
# the start of the validation partition
nTrain <- length(sales.ts) - nValid

# Partition the data
# Use window function to create a subset of the origital time series 
train.ts <- window(sales.ts, start = c(1995, 1), end = c(1995, nTrain))
valid.ts <- window(sales.ts, start = c(1995, nTrain + 1),
                   end = c(1995, nTrain + nValid))

train.ts
valid.ts


#####################################################################################
## 4. Creat a regression model with Sales (in Australian dollars) as the outcome   ##
##    variable, and with a linear trend and monthly seasonality.                   ##
##    Remember to fit only the training data. Call this model A.                   ##
#####################################################################################
# Produce a linear trend model with seasonality
modelA.lm <- tslm(sales.ts ~ trend + season)

# Plot the series
plot(sales.ts, xlab = "Time", ylab = "Sales (in Australian dollars)", 
     ylim = c(0, 110000), bty = "l")

# Fit linear trend line (solid red line) for regression model 
?lines()
lines(modelA.lm$fitted, col = "red", lwd = 2)

# Fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend + season)


#####################################################################################
## 5. Examine the estimated coefficients: which month tends to have the highest    ##
##    average sales during the year? Why is this reasonable given that the sales   ##
##    relate to an Australian business? How many predictors have been used in the  ##
##    regression model?                                                            ##
#####################################################################################
?forecast()
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)
train.lm.pred
summary(train.lm.pred)

# Answer:
# Season12 shows the highest coefficient of 32469.6, which means December tends
# to have the highest average sales during the year. 
# This is reasonable because December in Australia is summer (like NZ) 
# and it's a peak season for tourism. 
# Two predictors, trend and season, have been used in the regression model.


#####################################################################################
## 6. The estimated trend coefficient in model A is 245.4. What does this mean?    ##
#####################################################################################
# Answer:
# It means the magnitude of increase in souvenir sales each month is 245.4. 


#####################################################################################
## 7. Creat a regression model with an exponential trend and seasonality.          ##
##    Remember to fit only the training data. Call this model B.                   ##
#####################################################################################
# Produce a exponential trend model with seasonality
modelB.lm <- tslm(log(sales.ts) ~ trend + season)

# Plot the series
plot(sales.ts, xlab = "Time", ylab = "log(Sales)", 
     ylim = c(0, 110000), bty = "l")

# Fit exponential trend line (solid red line) for regression model 
lines(exp(modelB.lm$fitted), col = "blue", lwd = 2)


#####################################################################################
## 8. Fitting a model to log(Sales) with a linear trend is equivalent to fitting   ##
##    a model to Sales (in dollars) with what type of trend?
#####################################################################################
# Answer:
# Exponential trend


#####################################################################################
## 9. The estimated trend coefficient in model B is 0.02. What does this mean?     ##
#####################################################################################
# Answer:
# It means the magnitude of increase in souvenir sales each month is 2%.


#####################################################################################
## 10. Use model B to forecast the sales for January and February 2002. Use all    ##
##     available sales data to create a revised regression model.                  ##
#####################################################################################
# Fit exponential trend using tslm() with argument lambda = 0
train.lm.expo.trend <- tslm(train.ts ~ trend + season, lambda = 0)
train.lm.expo.trend.pred <- forecast(train.lm.expo.trend, h = nValid + 2, level = 0)
train.lm.expo.trend.pred

# Answer:
# Forecast for Jan 2020: 12601.017
# Forecast for Feb 2020: 17062.994

# Fit linear trend using tslm() with argument lambda = 1
train.lm.linear.trend <- tslm(train.ts ~ trend + season, lambda = 1)
train.lm.linear.trend.pred <- forecast(train.lm.linear.trend, h = nValid, level = 0)

plot(train.lm.expo.trend.pred, ylim = c(0, 110000), ylab = "Sales", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1995, 2003), flty = 2) 
axis(1, at = seq(1995, 2003, 1), labels = format(seq(1995, 2003, 1)))
lines(train.lm.expo.trend.pred$fitted, lwd = 2, col = "blue")
lines(train.lm.linear.trend.pred$fitted, lwd = 2, col= "black", lty = 3)
lines(train.lm.linear.trend.pred$mean, lwd = 2, col= "red", lty = 3)
lines(train.ts)
lines(valid.ts)


#####################################################################################
## 11. Compare the two regression models (A and B - fitted to the training         ##
##     partition) in terms of forecast accuracy on the validation set. When        ##
##     comparing accuracy, use the $mean component of each forecast and compare    ##
##     against the actual validation time series. Plot the $fitted component of    ##
##     each forecast against the training times series.                            ##
##     How well does each forecast compare against the validation set?             ##
##     How well does each plotted fitted model compare against the training set?   ##
#####################################################################################




#####################################################################################
## 12. Continuing with model B, create an ACF plot until lag 15 for the forecast   ##
##     errors ($residuals component of the Model B regression model). Now fit      ##
##     an AR model with lag 2 [ARIMA(2,0,0)] to the forecast errors.               ##
#####################################################################################
# Create an ACF plot
?Acf()
Acf(train.lm.expo.trend$residuals, lag.max = 15)

# Fit an AR model
?Arima()
train.res.arima <- Arima(train.lm.expo.trend$residuals, order = c(2,0,0))
train.res.arima
train.res.arima.pred <- forecast(train.res.arima, h = nValid)
summary(train.res.arima)


#####################################################################################
## 13. Examining the ACF plot and the estimated coefficients of the AR(2) model    ##
##     (and their statistical significance), what can we learn about the forecasts ##
##     that result from model B?                                                   ##
#####################################################################################
# According to the output of summary(train.res.arima) 
# Estimated coefficient of ar1: 0.3072
# Standard error for ar1: 0.1090 (an estimate of the standard deviation of the coefficient)
# Therefore, t-statistic = coefficient / standard error
0.3072 / 0.1090  # 2.818349

# Estimated coefficient of ar2: 0.3687
# Standard error for ar2: 0.1102 
# Calculate t-statistics for ar2
0.3687 / 0.1102  # 3.345735

# Answer:
# Both t-statistics are greater than 2, which means they are statically significant.


#####################################################################################
## 14. Use the autocorrelation information to calculate an improved forecast for   ##
##     January 2002, using model B and the AR(2) model above.                      ##
#####################################################################################
# Incorporate residual forecasts
Jan.2002.forecast <- forecast(train.lm.expo.trend, h = nValid + 1)
Jan.2002.forecast

Jan.2002.error.forecast <- forecast(train.res.arima, h = nValid + 1)
Jan.2002.error.forecast

improved.forecast <- Jan.2002.forecast$mean + Jan.2002.error.forecast$mean
improved.forecast

# Answer:
# Output suggests that improved forecast for January 2002 is 12601.02


#####################################################################################
## 15. Plot the residuals component of the Model B regression model against the    ##
##     residuals of the AR(2) model. Does the AR(2) model help to reduce error     ##
##     contained in the original Model B?                                          ##
#####################################################################################
