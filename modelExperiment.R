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
seasonality <- seasonality * 1.5          # stable amplitude

# Noise (stationary)
noise <- rnorm(n, mean = 0, sd = 0.5)

# Combine to make the time series
synthetic_ts <- arma_part + seasonality + noise

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
ar_model <- Arima(train_data, order = c(4, 0, 0))
ar_forecast <- forecast(ar_model, h = length(test_data))  # Forecast for test data
ar_model

# MA Model (Moving Average model)
ma_model <- Arima(train_data, order = c(0, 0, 1))  # MA(1)
ma_forecast <- forecast(ma_model, h = length(test_data))  # Forecast for test data
ma_model

# ARMA Model (Auto-Regressive Moving Average model)
arma_model <- Arima(train_data, order = c(4, 0, 1))  # ARMA(1, 1)
arma_forecast <- forecast(arma_model, h = length(test_data))  # Forecast for test data
arma_model

# ARMA + kt Model (ARMA with quadratic trend)
arma_kt_model <- lm(train_data ~ t_train + I(t_train^2))  # ARMA + quadratic trend (kt)
kt_pred <- predict(arma_kt_model, newdata = data.frame(t_train = t_test))  # Predict for test data

# Sinusoidal Model (a*sin(kt))
t_train_scaled <- t_train / max(t_train)  # Scale time for training
t_test_scaled <- t_test / max(t_train)  # Scale time for test

sin_model <- nls(train_data ~ a * sin(b * t_train_scaled + c),  # Fit sinusoidal model
                 start = list(a = 1, b = 2 * pi, c = 0),
                 control = list(maxiter = 500))
sin_pred <- predict(sin_model, newdata = data.frame(t_train_scaled = t_test_scaled))  # Predict for test data

#----------PLOT ACTUAL VS PREDICTED VALUES----------
# Plot it
plot(synthetic_ts, type = "l", col = "steelblue",
     main = "Stationary Real-World-Like Time Series",
     ylab = "Value", xlab = "Time")

# AR Model forecast plot
plot(t_test, test_data, type = "l", col = "black", main = "AR Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(t_test, ar_forecast$mean, col = "blue", lwd = 2)
legend("topright", legend = c("Actual Data", "AR Forecast"), col = c("black", "blue"), lty = 3)

# MA Model forecast plot
plot(t_test, test_data, type = "l", col = "black", main = "MA Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(t_test, ma_forecast$mean, col = "green")
legend("topright", legend = c("Actual Data", "MA Forecast"), col = c("black", "green"), lty = 3)

# ARMA Model forecast plot
plot(t_test, test_data, type = "l", col = "black", main = "ARMA Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(t_test, arma_forecast$mean, col = "purple")
legend("topright", legend = c("Actual Data", "ARMA Forecast"), col = c("black", "purple"), lty = 3)

# ARMA + kt Model forecast plot
plot(t_test, test_data, type = "l", col = "black", main = "ARMA + kt Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(t_test, kt_pred, col = "orange")
legend("topright", legend = c("Actual Data", "ARMA + kt Forecast"), col = c("black", "orange"), lty = 3)

# Sinusoidal Model forecast plot
plot(t_test, test_data, type = "l", col = "black", main = "Sinusoidal Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(t_test, sin_pred, col = "red")
legend("topright", legend = c("Actual Data", "Sinusoidal Forecast"), col = c("black", "red"), lty = 3)

#MAE
maeAR <- mae(t_test,ar_forecast$mean)
maeAR

maeMA <- mae(t_test,ma_forecast$mean)
maeMA

maeARMA <- mae(t_test,arma_forecast$mean)
maeARMA

maeARMA_kt <- mae(t_test,kt_pred)
maeARMA_kt

maeSIN <- mae(t_test,sin_pred)
maeSIN