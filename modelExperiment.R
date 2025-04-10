install.packages("zoo")
install.packages("data.table")
install.packages("tseries")
install.packages("ggplot2")
install.packages("arrow")
install.packages("forecast")
install.packages("reshape2")
require(forecast)
require(arrow)
require(tseries)
require(zoo)
require(data.table)
require(ggplot2)
library(nlme)


#----------CREATING TIME SERIES USING POPULATION COVARIANCE FORMULA-----------
set.seed(123)

# Parameters
n <- 10000  # Number of data points
sigma_sq <- 1  # Variance of the time series
rho <- 0.6  # Autocorrelation coefficient (lag 1)

# Initialize the time series
synthetic_ts <- numeric(n)

# Generate the first value randomly (assuming mean 0 for simplicity)
synthetic_ts[1] <- rnorm(1, mean = 0, sd = sqrt(sigma_sq))

# Generate the rest of the time series using the population covariance formula
for (t in 2:n) {
  # Generate the next value based on the autocovariance function
  synthetic_ts[t] <- rho * synthetic_ts[t-1] + sqrt(1 - rho^2) * rnorm(1, mean = 0, sd = sqrt(sigma_sq))
}

#----------SPLITTING DATA INTO TRAINING AND TEST SETS----------
train_size <- floor(0.8 * n)  # Training set (80%)
train_data <- synthetic_ts[1:train_size]  # First 80% of data for training
test_data <- synthetic_ts[(train_size + 1):n]  # Last 20% of data for testing
t_train <- 1:train_size  # Time for training data
t_test <- (train_size + 1):n  # Time for test data

#----------FIT MODELS ON TRAINING DATA----------

# AR Model (Auto-Regressive model)
ar_model <- arima(train_data, order = c(1, 0, 0))  # AR(1)
ar_forecast <- forecast(ar_model, h = length(test_data))  # Forecast for test data
ar_model

# MA Model (Moving Average model)
ma_model <- arima(train_data, order = c(0, 0, 1))  # MA(1)
ma_forecast <- forecast(ma_model, h = length(test_data))  # Forecast for test data
ma_model

# ARMA Model (Auto-Regressive Moving Average model)
arma_model <- arima(train_data, order = c(1, 0, 1))  # ARMA(1, 1)
arma_forecast <- forecast(arma_model, h = length(test_data))  # Forecast for test data
arma_model
auto.arima(train_data)

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

# AR Model forecast plot
plot(test_data, type = "l", col = "black", main = "AR Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(ar_forecast$mean, col = "blue")
legend("topright", legend = c("Actual Data", "AR Forecast"), col = c("black", "blue"), lty = 3)

# MA Model forecast plot
plot(test_data, type = "l", col = "black", main = "MA Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(ma_forecast$mean, col = "green")
legend("topright", legend = c("Actual Data", "MA Forecast"), col = c("black", "green"), lty = 3)

# ARMA Model forecast plot
plot(test_data, type = "l", col = "black", main = "ARMA Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(arma_forecast$mean, col = "purple")
legend("topright", legend = c("Actual Data", "ARMA Forecast"), col = c("black", "purple"), lty = 3)

# ARMA + kt Model forecast plot
plot(test_data, type = "l", col = "black", main = "ARMA + kt Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(kt_pred, col = "orange")
legend("topright", legend = c("Actual Data", "ARMA + kt Forecast"), col = c("black", "orange"), lty = 3)

# Sinusoidal Model forecast plot
plot(test_data, type = "l", col = "black", main = "Sinusoidal Model Forecast vs Test Data", ylab = "Value", xlab = "Time")
lines(sin_pred, col = "red")
legend("topright", legend = c("Actual Data", "Sinusoidal Forecast"), col = c("black", "red"), lty = 3)

# Original synthetic time series plot (just for reference)
plot(synthetic_ts, type = "l", main = "Full Synthetic Time Series", ylab = "Value", xlab = "Time")