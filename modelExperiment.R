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
time_series <- ts(abs(rnorm(n)))

#display the first few points
print(head(time_series))

#plot the time series
plot(time_series, main = "Random Time Series (10,000 Points)", col = "blue", 
     type ="p")
