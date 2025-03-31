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


# Set the number of data points
n <- 10000

# Generate a sequence of x values from 0 to 2π with n points
x <- seq(0, 2 * pi, length.out = n)

# Compute the sine of each x value
y <- sin(x)

# Plot the sine wave
plot(x, y, type = "l", col = "blue", 
     main = "Sine Wave with 10,000 Data Points", xlab = "x", ylab = "sin(x)")
