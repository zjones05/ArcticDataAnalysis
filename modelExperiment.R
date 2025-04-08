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


#----------CREATING TIME SERIES-----------
#set the seed for reproducibility
set.seed(123)

#generate a time series of 10,000 random data points
n <- 10000
lambda <- 0.1
time_series <- ts(abs(rexp(n, rate = lambda)))

#adf test
adf.test(time_series)

#plot the time series
plot(time_series, main = "Random Exponential Time Series (10,000 Points)", 
     col = "blue", type ="l")

#----------FIT MODELS----------
# Apply AR Model
ar_model <- arima(time_series, order = c(1, 0, 0))
ar_model
acf(ar_model$residuals)
pacf(ar_model$residuals)

# Apply MA Model
ma_model <- arima(time_series, order = c(0, 0, 1))
ma_model
acf(ma_model$residuals)
pacf(ma_model$residuals)

# Apply ARMA Model
arma_model <- arima(time_series, order = c(1, 0, 1))
arma_model
acf(arma_model$residuals)
pacf(arma_model$residuals)

# Apply ARMA + Linear Trend Model
arma_linear_model <- arima(time_series, order = c(1, 0, 1), xreg = 1:n)
arma_linear_model
acf(arma_linear_model$residuals)
pacf(arma_linear_model$residuals)

# Apply Sine + Noise Model
omega <- 0.05
a <- 2
sine_noise <- a * sin(omega * (1:n)) + rnorm(n)
sin_noise_model <- arima(time_series, order = c(1, 0, 1), xreg = sine_noise)
sin_noise_model
acf(sin_noise_model$residuals)
pacf(sin_noise_model$residuals)


# Compare model AIC and BIC
cat("AR Model AIC:", ar_model$aic, " BIC:", AIC(ar_model, k = log(n)), "\n")
cat("MA Model AIC:", ma_model$aic, " BIC:", AIC(ma_model, k = log(n)), "\n")
cat("ARMA Model AIC:", arma_model$aic, " BIC:", AIC(arma_model, 
                                                    k = log(n)), "\n")
cat("ARMA + Linear Model AIC:", arma_linear_model$aic, " BIC:", 
    AIC(arma_linear_model, k = log(n)), "\n")
cat("Sine + Noise Model AIC:", sin_noise_model$aic, " BIC:", 
    AIC(sin_noise_model, k = log(n)), "\n")

#Ljung-Box Test
checkresiduals(ar_model)
checkresiduals(ma_model)
checkresiduals(arma_model)
checkresiduals(arma_linear_model)
checkresiduals(sin_noise_model)


#----------TESTING SET AND TRAINING SET----------
#set the training size (80% of the total data)
trainSize <- round(0.8 * length(time_series))

#split the time series into training and testing sets
trainSet <- window(time_series, end = trainSize)
testSet <- window(time_series, start = trainSize + 1)
length(trainSet)
length(testSet)

#fit the MA model on the training set
maModelTrain <- arima(trainSet, order = c(0, 0, 1))
maModelTrain

#predict using the trained model
predictions <- predict(maModelTrain, n.ahead = length(testSet))

#plot the predictions against the actual test data
plot(testSet, col = "red", main = "Actual vs Predicted", ylab = "Values")
lines(predictions$pred, col = "blue", lty = 2)


#----------CHOOSING MODEL AUTOMATICALLY----------
# Load libraries
library(forecast)
library(ggplot2)
library(tseries)
library(reshape2)

# Set seed and generate predictable stationary data
set.seed(42)
n <- 10000
t <- 1:n
signal <- 2 * sin(2 * pi * t / 365)
noise <- rnorm(n, sd = 0.3)
data <- signal + noise

# Plot data
ggplot(data.frame(t = t, y = data), aes(x = t, y = y)) +
  geom_line() +
  ggtitle("Synthetic Time Series: Sine + Moderate Noise") +
  theme_minimal()

# Train/test split
train_size <- floor(0.8 * n)
train <- data[1:train_size]
test <- data[(train_size + 1):n]

# MSE function
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# 1. AR(10) model
ar_model <- Arima(train, order = c(10, 0, 0))
ar_pred <- forecast(ar_model, h = length(test))$mean
ar_model

# 2. MA(10) model
ma_model <- Arima(train, order = c(0, 0, 10))
ma_pred <- forecast(ma_model, h = length(test))$mean

# 3. ARMA(5,5) model
arma_model <- auto.arima(train, max.p = 5, max.q = 5, max.d = 1, seasonal = FALSE)
arma_pred <- forecast(arma_model, h = length(test))$mean

# 4. ARMA(5,5) + linear trend
xreg_train <- 1:train_size
xreg_test <- (train_size + 1):n
arma_lin_model <- Arima(train, order = c(5, 0, 5), xreg = xreg_train)
arma_lin_pred <- forecast(arma_lin_model, xreg = xreg_test, h = length(test))$mean

# Define time variables for clarity
t_train <- 1:train_size
t_test <- (train_size + 1):n

# Fit sine model using t_train
sine_model <- nls(train ~ a * sin(b * t_train + c) + d,
                  start = list(a = 2, b = 2 * pi / 365, c = 0, d = 0))

# Predict on test time points using t_test
sine_pred <- predict(sine_model, newdata = list(t_train = t_test))


# MSE results
cat("AR(10) MSE:", mse(test, ar_pred), "\n")
cat("MA(10) MSE:", mse(test, ma_pred), "\n")
cat("ARMA(5,5) MSE:", mse(test, arma_pred), "\n")
cat("ARMA + Linear MSE:", mse(test, arma_lin_pred), "\n")
cat("Sine + Noise MSE:", mse(test, sine_pred), "\n")

# Plot predictions
plot_df <- data.frame(
  time = t_test,
  True = test,
  AR = ar_pred,
  MA = ma_pred,
  ARMA = arma_pred,
  ARMA_Linear = arma_lin_pred,
  Sine = sine_pred
)


melted <- melt(plot_df, id = "time")

ggplot(melted, aes(x = time, y = value, color = variable)) +
  geom_line() +
  ggtitle("Model Predictions vs. True Values") +
  theme_minimal()

