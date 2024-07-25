# Install and load necessary packages
install.packages("rugarch")
install.packages("tseries")
install.packages("xts")
install.packages("forecast")
library(rugarch)
library(tseries)
library(xts)
library(forecast)

install.packages("FinTS")
library(FinTS)

# Load the data
data <- read.csv('C:\\Users\\Dell\\Desktop\\MICAH\\Mahindra.csv')
# Inspect the first few rows of the data
head(data)

# Convert the date column to Date type if necessary
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Create a time series object for adjusted close prices
market <- xts(data$Adj.Close, order.by = data$Date)

# Calculate percentage returns
returns <- 100 * diff(log(market))  # log returns * 100
returns <- returns[!is.na(returns)]  # Remove NA values

# Plot the returns
plot(returns, main="Returns", ylab="Percentage Returns", xlab="Date")

# Fit an ARCH model to the cleaned returns
arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                        distribution.model = "norm")
arch_fit <- ugarchfit(spec = arch_spec, data = returns)

# Print the summary of the fitted model
print(arch_fit)

# Plot the fitted model's conditional volatility
plot(arch_fit, which = 3)
arch_fit <- ugarchfit(spec = arch_spec, data = returns)

# Fit a GARCH model to the cleaned returns
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")

garch_fit <- ugarchfit(spec = garch_spec, data = returns)

# Print the summary of the fitted model
print(garch_fit)

# Plot the fitted model's conditional volatility
plot(garch_fit, which = 3)

# Forecast the volatility
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 90)

# Extract and print the forecasted variances
forecasted_variance <- garch_forecast@forecast$sigmaFor
tail(forecasted_variance, 3)

# Plot the forecasted volatility
plot(garch_forecast, which = 1)