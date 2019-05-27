install.packages("tseries")
install.packages("forecast")
library(tseries)
library(forecast)
plot(Nile)

# Chart data appears to be stationary
# We'll asses it for a trend
ndiffs(Nile)

# This indicates that the data contains a trend
# so the time series is differenced once ( lag =1)
diff_Nile <- diff(Nile, lag =1)
plot(diff_Nile)

# Show both time series side by side for comparision
# Take a snapshot of settings
default_settings <- par(no.readonly = TRUE)
# Display charts in 1 row and 2 col
par(mfrow = c(1,2))
plot(Nile)
plot(diff_Nile)

# Reset settings back to the default values
par(default_settings)

# We'll assess whether there's a presence of
# in the differenced time series
ndiffs(diff_Nile)

# We're sure there isn't a trend in the time series

# Apply the ADF test to differenced time series
adf.test(diff_Nile)

# Results show that the time series is now stationary
# so we can proceed to the next step
# H0 = data needs to be differenced to make it stationary

# next step is to identify one or more reasonable models by
# examining autocorrelation and partial autocorrelation plots
# ACF and PACF plots.


Acf(diff_Nile, main = "Autocorrelation plot for differenced nile time series")

#partial autocorrlation plot
pacf(diff_Nile, main = " Partial Autocorrelation plot for differenced nile time series")

# Next step - fitting the ARIMA model
# We use the original dataset for the Arima model
# and modify the d-value to suit our area of findings
# and d = 1
# We apply the model to the original time series
arima_model <- Arima(Nile, order = c(0, 1, 1)) # This is wrong
arima_model

# Accuracy measures using the MAPE
# mean absolute percentage error
# Measures the prediction of accuracy
accuracy(arima_model)

# The MAPE is 13% so this means there is a forecast
# error of 13% in this arima model

# Evaluating the q-q norm
# Produces a normal q-q plot of the values in y
# Adding a q-q line shows us a theoratical quantile-quantile plot
# this line passes through the first and third probability quartiles.
qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

# Test to examine whether the data and the model
# are closely fit or not
# box test function provides a test that autocorrelates
# are all 0 (Null Hypothesis)
# h0 = all values are zero
Box.test(arima_model$residuals, type = "Ljung-Box")

# Results show the model appears to fit the data well

# Forecast three years ahead for the Nile time series
forecast(arima_model, 3)

# Plot the time series prediction, this shows
# the forecast and the 80%, 95% confidence bands
plot(forecast(arima_model, 3), xlab = "Year", ylab = "Annual flow")

# Using the auto arima function
# Automated Arima forecast
auto_arima_model <- auto.arima(Nile)
auto_arima_model

# Compare accuracy of both models
accuracy(arima_model)
accuracy(auto_arima_model)

qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)

Box.test(auto_arima_model$residuals, type = "Ljung-Box")
# It has a stronger p value than the other one

# Plot information for predicted values
plot(forecast(auto_arima_model, 3), 
     xlab = "Year", 
     ylab = "Annual flow")

auto_arima_model
arima_model


# Plot information for predicted values
plot(forecast(auto_arima_model, 30), 
     xlab = "Year", 
     ylab = "Annual flow")

auto_arima_model
arima_model

# Plotting again
plot(forecast(auto_arima_model, 10), 
     xlab = "Year", 
     ylab = "Annual flow")

auto_arima_model
arima_model

# Seasonal decomposition
# airline passengers dataset contains
# monthly totals (thousands) of international
# airline passengers 1949-1960
plot(AirPassengers)
# This is a multiplicative (same pattern but gets bigger)
# Before running this behaviour has to be removed 

# variability is increasing - multiplicative model
# Before we decompose the time series into trend, seasonal
# and irregular components we need to convert from
# a multiplicative to additive model using a log 
# transfer
log_of_Airpassengers <- log(AirPassengers)
plot(log_of_Airpassengers)
# Varience has now stabalised

seasonal_decomposition <- stl(log_of_Airpassengers, s.window = "period")
plot(seasonal_decomposition)

# Components are currently in logged format
# We need to convert back to the original metric
converted_AirPassengers <- exp(seasonal_decomposition$time.series)
plot(converted_AirPassengers)
converted_AirPassengers

seasonal_adj_AirPassengers <- seasadj(seasonal_decomposition)
seasonplot(AirPassengers, 12, col = rainbow(12), year.labels = TRUE,
           main = "Seasonal plot of Airpassengers")

seasonplot(seasonal_adj_AirPassengers, 12, col = rainbow(12), year.labels = TRUE,
           main = "Seasonal element removed from plot of Airpassengers")

# This chart is stationary now checking for trend
ndiffs(seasonal_adj_AirPassengers)
diff_seasonally_adj_airpassenger <- diff(seasonal_adj_AirPassengers, 
                                         lag = 1)
ndiffs(diff_seasonally_adj_airpassenger)

# Data needs to be differenced to make it stationary = H0
adf.test(diff_seasonally_adj_airpassenger)    

auto_arima_model <- auto.arima(seasonal_adj_AirPassengers)
auto_arima_model                                         

accuracy(auto_arima_model)                                         
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)


forecast(auto_arima_model)
plot(forecast(auto_arima_model, 3), xlab = "Year"
     , ylab = "Annual Passengers")

# Plan for testing..
# Extract the number of periods out of the original time series first
# and then compare with predicted time series forecast


