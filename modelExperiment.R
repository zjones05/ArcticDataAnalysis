install.packages("zoo")
install.packages("data.table")
install.packages("tseries")
install.packages("ggplot2")
install.packages("arrow")
install.packages("forecast")
install.packages("reshape2")
install.packages("Metrics")
library(Metrics)
require(forecast)
require(arrow)
require(tseries)
require(zoo)
require(data.table)
require(ggplot2)
library(nlme)

#----------CREATING TIME SERIES-----------
set.seed(123)
n <- 10000

# ARMA component
arma_part <- arima.sim(n = n, model = list(ar = 0.7, ma = -0.3))

# Seasonal component (like cycles in weather)
seasonality <- sin(2 * pi * (1:n) / 200)  # cycle every 200 steps
seasonality <- seasonality * 1.5 
plot(seasonality)
# stable amplitude

# Noise (stationary)
noise <- rnorm(n, mean = 0, sd = 0.5)

# Combine to make the time series
synthetic_ts <- arma_part + seasonality + noise

#plot time series
plot(synthetic_ts,type="l")

#testing stationarity
adf.test(synthetic_ts)

#----------SPLITTING DATA INTO TRAINING AND TEST SETS----------
train_size <- floor(0.8 * n)  # Training set (80%)

train_data <- synthetic_ts[1:train_size]  # First 80% of data for training
test_data <- synthetic_ts[(train_size + 1):n]  # Last 20% of data for testing

t_train <- 1:train_size  # Time for training data
t_test <- (train_size + 1):n  # Time for test data

#----------FIT MODELS ON TRAINING DATA----------
#auto ARMA
autoAr_model <- auto.arima(train_data)
autoAr_model

# AR Model (Auto-Regressive model)
ar_model <- Arima(train_data, order = c(4, 0, 0)) #AR(4)
ar_forecast <- forecast(ar_model,h=15)  # Forecast one step ahead
ar_model

# MA Model (Moving Average model)
ma_model <- Arima(train_data, order = c(0, 0, 1)) #MA(1)
ma_forecast <- forecast(ma_model,h=15) # Forecast one step ahead
ma_model

# ARMA Model (Auto-Regressive Moving Average model)
arma_model <- Arima(train_data, order = c(4, 0, 1))  # ARMA(4, 0, 1)
arma_forecast <- forecast(arma_model,h=15)  # Forecast one step ahead
arma_model

# ARMA + kt Model (ARMA with quadratic trend)
arma_kt_model <- lm(train_data ~ t_train + I(t_train^2))  # ARMA + quadratic trend (kt)
kt_pred <- predict(arma_kt_model, newdata = data.frame(t_train = t_test))  # Predict for test data

# Sinusoidal Model (a*sin(kt))
#t_train_scaled <- t_train / max(t_train)  # Scale time for training
#t_test_scaled <- t_test / max(t_train)  # Scale time for test
sin_model <- nls(train_data ~ a * sin(b * t_train + c),  # Fit sinusoidal model
                 start = list(a = 1, b = 2 * pi / 200, c = 0),
                 control = list(maxiter = 500))
sin_pred <- predict(sin_model, newdata = data.frame(t_train = t_test))
sin_model

#----------PLOT ACTUAL VS PREDICTED VALUES----------
#Original time series plot
plot(synthetic_ts, type = "l", col = "black",
     main = "Full Time Series",
     ylab = "Value", xlab = "Time")

# Define the zoom window for AR, MA, and ARMA plots
zoom_range <- 1:15

# AR Model forecast plot
plot(t_test[zoom_range], test_data[zoom_range], type = "l", col = "black",
     main = "Zoomed: AR Model Forecast vs Test Data",
     ylab = "Value", xlab = "Time",
     xlim = range(t_test[zoom_range]), 
     ylim = range(c(test_data[zoom_range], ar_forecast$mean[zoom_range])))

lines(t_test[zoom_range], ar_forecast$mean[zoom_range], col = "blue", lwd = 2)

legend("topright", legend = c("Actual Data", "AR Forecast"), 
       col = c("black", "blue"), lty = 3)

# MA Model forecast plot
plot(t_test[zoom_range], test_data[zoom_range], type = "l", col = "black",
     main = "Zoomed: MA Model Forecast vs Test Data",
     ylab = "Value", xlab = "Time",
     xlim = range(t_test[zoom_range]), 
     ylim = range(c(test_data[zoom_range], ma_forecast$mean[zoom_range])))

lines(t_test[zoom_range], ma_forecast$mean[zoom_range], col = "green", lwd = 2)

legend("topright", legend = c("Actual Data", "MA Forecast"), 
       col = c("black", "green"), lty = 3)

# ARMA Model forecast plot
plot(t_test[zoom_range], test_data[zoom_range], type = "l", col = "black",
     main = "Zoomed: ARMA Model Forecast vs Test Data",
     ylab = "Value", xlab = "Time",
     xlim = range(t_test[zoom_range]), 
     ylim = range(c(test_data[zoom_range], arma_forecast$mean[zoom_range])))

lines(t_test[zoom_range], arma_forecast$mean[zoom_range], col = "purple", lwd = 2)

legend("topright", legend = c("Actual Data", "ARMA Forecast"), 
       col = c("black", "purple"), lty = 3)

# ARMA + kt Model forecast plot
plot(t_test, test_data, type = "l", col = "black", main = "ARMA + kt Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(t_test, kt_pred, col = "orange")
legend("topright", legend = c("Actual Data", "ARMA + kt Forecast"), col = c("black", "orange"), lty = 3)

# Sine Model forecast plot
plot(t_test, test_data, type = "l", col = "black", main = "Sinusoidal Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(t_test, sin_pred, col = "red")
legend("topright", legend = c("Actual Data", "Sinusoidal Forecast"), col = c("black", "red"), lty = 3)
length(test_data)
length(sin_pred)

#---------ANALYSIS----------
#Autocorrelation
acf(ar_model$residuals)
pacf(ar_model$residuals)

acf(ma_model$residuals)
pacf(ma_model$residuals)

acf(arma_model$residuals)
pacf(arma_model$residuals)

#Mean Absolute Error
maeAR <- mae(test_data[zoom_range],ar_forecast$mean[zoom_range])
maeAR

maeMA <- mae(test_data[zoom_range],ma_forecast$mean[zoom_range])
maeMA

maeARMA <- mae(test_data[zoom_range],arma_forecast$mean[zoom_range])
maeARMA

maeARMA_kt <- mae(test_data,kt_pred)
maeARMA_kt

maeSIN <- mae(test_data,sin_pred)
maeSIN
