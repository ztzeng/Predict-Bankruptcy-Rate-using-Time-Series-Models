library(forecast)
library(tseries)
library(vars)

data <- read.csv("/Users/anna/Classes/msds604/Final_Project/train.csv")
Bankruptcy <- ts(data$Bankruptcy_Rate, start = c(1987, 1), frequency = 12)
Unemployment<- ts(data$Unemployment_Rate, start = c(1987, 1), frequency = 12)
Population <- ts(data$Population, start = c(1987, 1), frequency = 12)
HousePrice <- ts(data$House_Price_Index, start = c(1987, 1), frequency = 12)
t <- time(Bankruptcy)

# Summary of the data
DF <- data.frame(t, Bankruptcy, Unemployment, Population, HousePrice)
# Look at the cross-correlations
cor(DF)

# splitting training data set to training and cross validation set 
train <- data[1:276,]
crossval <- data[277:336,]

train_Bankruptcy <- ts(train$Bankruptcy_Rate, start = c(1987, 1), frequency = 12)
train_Unemployment<- ts(train$Unemployment_Rate, start = c(1987, 1), frequency = 12)
train_Population <- ts(train$Population, start = c(1987, 1), frequency = 12)
train_HousePrice <- ts(train$House_Price_Index, start = c(1987, 1), frequency = 12)
cv_Population <- ts(crossval$Population, start = c(2010,1), frequency = 12)

# Plot the time series
par(mar=c(1,1,1,1))
plot(train_Bankruptcy)
plot(train_Unemployment)
plot(train_Population)
plot(train_HousePrice)


### VARX
## selecting p parameter 
select_p <- VARselect(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment))

## Most of the criterium selected p = 10 
rmse <- c()
for (pi in 1:10) {
  model_varx <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Unemployment), 
                    exogen = data.frame(pop=train_Population), p = pi)
  pred <- predict(model_varx, dumvar = data.frame(pop=cv_Population), n.ahead = 60, ci = 0.95)
  rmse[pi] <- sqrt(mean((pred$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))
}

var_models_df <- data.frame(p_parameter=seq(1,10), rmse_crossval_bankruptcy = rmse, train_aic = select_p$criteria[1,])

# arranged by lowest AIC 
var_models_df[order(var_models_df$train_aic),]

# arranged by lowest RMSE (when validated with cross validation set)
var_models_df[order(var_models_df$rmse_crossval_bankruptcy),]

best_vx <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Unemployment), 
               exogen = data.frame(pop=train_Population), p = 10)
pred <- predict(model_vx, dumvar = data.frame(pop=cv_Population), n.ahead = 60, ci = 0.95)
plot(pred)