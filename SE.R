library(forecast)

# Plot the raw data
data = read.csv('/Users/anna/Classes/msds604/Final_Project/train.csv',header = TRUE)
Bankruptcy <- ts(data$Bankruptcy_Rate, start = c(1987, 1), frequency = 12)
plot(Bankruptcy, main = "Monthly Bankruptcy Rate of Canada", ylab = " % ", xlab = "Time")
lambda = BoxCox.lambda(Bankruptcy)

# Split to train/validate
Bankruptcy_train = ts(data$Bankruptcy_Rate[1:276], start = c(1987, 1), frequency = 12)
Bankruptcy_vali = ts(data$Bankruptcy_Rate[277:336], start = c(2010, 1), frequency = 12)

# Tripple ES -- Additive
bank.HW2 <- HoltWinters(x = BoxCox(Bankruptcy_train,lambda =lambda),seasonal = "add") 
plot(bank.HW2)
forcast.HW2 = forecast(bank.HW2, h = 60, level = 0.95)
rmse_es2 <- sqrt(mean((InvBoxCox(forcast.HW2$mean,lambda=lambda) - Bankruptcy_vali)^2))

# Tripple ES -- Multi
bank.HW3 <- HoltWinters(x = BoxCox(Bankruptcy_train,lambda =lambda),seasonal = "mult") 
plot(bank.HW3)
forcast.HW3 = forecast(bank.HW3, h = 60, level = 0.95)
rmse_es3 <- sqrt(mean((InvBoxCox(forcast.HW3$mean,lambda=lambda) - Bankruptcy_vali)^2))

# Compare and choose
print(data.frame(rmse_es2,rmse_es3))

# Plot
f <- forecast(HoltWinters(x = BoxCox(Bankruptcy,lambda =lambda),alpha = 0.3208689, beta = 0.178877, gamma=0.1732346,seasonal = "add"), h = 36, level = 0.95)
l <- ts(f$lower, start = c(2015, 1), frequency = 12) 
h <- ts(f$upper, start = c(2015, 1), frequency = 12) 
pred <- f$mean
fitted <- f$fitted

par(mfrow=c(1,1))
plot(Bankruptcy, xlim=c(1987,2018), ylim=c(-1,15),main = "Best Model in Exponential Smmothing \n (Tripple ES -- Additive)", ylab = " Monthly Bankruptcy Rate % ", xlab = "Year")
abline(v = 2015, lwd = 2, col = "black")
points(InvBoxCox(pred,lambda=lambda), type = "l", col = "blue")
points(InvBoxCox(l,lambda=lambda), type = "l", col = "red")
points(InvBoxCox(h,lambda=lambda), type = "l", col = "red")
points(InvBoxCox(fitted,lambda=lambda), type="l", col = "green")
legend("topleft", legend = c("Observed", "Fitted", "Predicted", "95% CI"), lty = 1, col = c("black", "green", "blue", "red"), cex = 0.8)
