library(forecast)
library(tseries)
library(caret)

# Plot the raw data
data <-  read.csv('train.csv',header = TRUE)
Bankruptcy <- ts(data$Bankruptcy_Rate, start = c(1987, 1), frequency = 12)
plot(Bankruptcy, main = "Monthly Bankruptcy Rate of Canada", ylab = " % ", xlab = "Time")
lambda_bankruptcy <-  BoxCox.lambda(Bankruptcy)

# Split to train/validate
Bankruptcy_train <-  ts(data$Bankruptcy_Rate[1:276], start = c(1987, 1), frequency = 12)
Bankruptcy_vali <-  ts(data$Bankruptcy_Rate[277:336], start = c(2010, 1), frequency = 12)

plot(BoxCox(Bankruptcy, lambda = lambda_bankruptcy))

# notice there are trend and seasonality
acf(BoxCox(Bankruptcy, lambda = lambda_bankruptcy), lag.max =60)
# check for stationarity
adf.test(Bankruptcy_train)

# differencing the raw data once
train_diff <- diff(BoxCox(Bankruptcy_train, lambda = lambda_bankruptcy))
plot(train_diff)

# check stationarity again
adf.test(train_diff)

# check seasonality
acf(train_diff, lag.max = 144)

# differencing seasonality
# train_diff_12 <- diff(train_diff, lag = 12)
# plot(train_diff_12)
# acf(train_diff_12, lag.max = 144)

ndiffs(BoxCox(Bankruptcy_train, lambda = lambda_bankruptcy)) # ndiffs=1
nsdiffs(BoxCox(Bankruptcy_train, lambda = lambda_bankruptcy), m=12) # nsdiffs=0

##################################
# auto.arima selected
##################################

auto.arima(Bankruptcy)

bankmodel1 <- Arima(Bankruptcy_train, order = c(1,1,3), seasonal = list(order = c(1,0,0), period = 12), method = "CSS-ML", lambda = lambda_bankruptcy)
summary(bankmodel1)

# Assumption Verification
# zero-mean and homoscedasticity
plot(bankmodel1$residuals, main="Residuals vs. Time", ylab = "Residuals")
abline(h=0, col="red")

# zero correlation
acf(bankmodel1$residuals, main = "ACF of Residuals")

# normality
qqnorm(bankmodel1$residuals)
qqline(bankmodel1$residuals, col = "red")
shapiro.test(bankmodel1$residuals)

# model fitting
fit_bank <- BoxCox(Bankruptcy_train, lambda = lambda_bankruptcy)-bankmodel1$residuals
plot(Bankruptcy_train, main = "Figure-9")
lines(InvBoxCox(fit_bank, lambda = lambda_bankruptcy), col="red")
legend("bottomright", legend=c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex=0.7)

# forecasting
forecast_bank <- forecast(object = bankmodel1, h=72, level=0.95, lambda = lambda_bankruptcy, biasadj = FALSE)
plot(forecast_bank)
lines(InvBoxCox(fit_bank, lambda = lambda_bankruptcy), col="red")
lines(Bankruptcy_vali, col="black")
legend("bottomright", legend=c("Observed", "Forecasted", "Fitted"), lty = 1, col = c("black", "blue", "red"), cex=0.5)

accuracy(forecast_bank, Bankruptcy_vali)

##################################
# manually picked p, q, P, and Q
##################################

# choosing q
acf(train_diff, lag.max = 144)
# choosing p
pacf(train_diff, lag.max = 144)

# p = 3, q = 2, P = 1, Q = 1
bankmodel2 <- Arima(Bankruptcy_train, order = c(3,1,2), seasonal = list(order = c(1,0,1), period = 12), method = "ML", lambda = lambda_bankruptcy)
summary(bankmodel2)

# Assumption Verification
# zero-mean and homoscedasticity
plot(bankmodel2$residuals, main="Residuals vs. Time", ylab = "Residuals")
abline(h=0, col="red")

# zero correlation
acf(bankmodel2$residuals, main = "ACF of Residuals")

# normality
qqnorm(bankmodel2$residuals)
qqline(bankmodel2$residuals, col = "red")
shapiro.test(bankmodel2$residuals)

# Training data fitting
fit_bank2 <- BoxCox(Bankruptcy_train, lambda = lambda_bankruptcy)-bankmodel2$residuals
plot(Bankruptcy_train)
lines(InvBoxCox(fit_bank2, lambda = lambda_bankruptcy), col="red")
legend("bottomright", legend=c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex=0.7)

# forecasting
forecast_bank2 <- forecast(object = bankmodel2, h=72, level=0.95, lambda = lambda_bankruptcy, biasadj = FALSE)
plot(forecast_bank2)
lines(InvBoxCox(fit_bank2, lambda = lambda_bankruptcy), col="red")
lines(Bankruptcy_vali, col="black")
legend("bottomright", legend=c("Observed", "Forecasted", "Fitted"), lty = 1, col = c("black", "blue", "red"), cex=0.5)

# RMSE
accuracy(forecast_bank2, Bankruptcy_vali)
