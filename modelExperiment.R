install.packages("zoo")
install.packages("data.table")
install.packages("tseries")
install.packages("ggplot2")
install.packages("arrow")
install.packages("forecast")
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

# Apply MA Model
ma_model <- arima(time_series, order = c(0, 0, 1))
ma_model

# Apply ARMA Model
arma_model <- arima(time_series, order = c(1, 0, 1))
arma_model

# Apply ARMA + Linear Trend Model
arma_linear_model <- arima(time_series, order = c(1, 0, 1), xreg = 1:n)
arma_linear_model

# Apply Sine + Noise Model
omega <- 0.05
a <- 2
sine_noise <- a * sin(omega * (1:n)) + rnorm(n)
sin_noise_model <- arima(time_series, order = c(1, 0, 1), xreg = sine_noise)
sin_noise_model

#AIC number: ma < ar < arma + linear < arma < sine
#MA model has best fit

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

# Predict using the trained model
predictions <- predict(maModelTrain, n.ahead = length(testSet))

# Plot the predictions against the actual test data
plot(testSet, col = "red", main = "Actual vs Predicted", ylab = "Values")
lines(predictions$pred, col = "blue", lty = 2)
