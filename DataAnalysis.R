# Get the data
library(forecast)
library(tseries)
library(ggplot2)
library(ModelMetrics)
data = read.csv('/Users/anna/Classes/msds604/Final_Project/train.csv',header = TRUE)
Bankruptcy <- ts(data$Bankruptcy_Rate, start = c(1987, 1), frequency = 12)
Unemployment<- ts(data$Unemployment_Rate, start = c(1987, 1), frequency = 12)
Population <- ts(data$Population, start = c(1987, 1), frequency = 12)
HousePrice <- ts(data$House_Price_Index, start = c(1987, 1), frequency = 12)
t <- time(Bankruptcy)

# Plot the time series
par(mfrow=c(2,2))
plot(Bankruptcy)
plot(Unemployment)
plot(Population)
plot(HousePrice)

# Summary of the data
DF <- data.frame(t, Bankruptcy, Unemployment, Population, HousePrice)
as.table(summary(DF))

# Look at the cross-correlations
cor(DF)

# Transformation
BoxCox.lambda(Bankruptcy) # 0.2390347 !=1, should be transformed


