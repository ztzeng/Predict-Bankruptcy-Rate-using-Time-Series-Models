library(forecast)
library(tseries)
library(ggplot2)
library(ModelMetrics)
library(vars)
library(tidyverse)

# reading in full training data set 
data = read.csv('/Users/anna/Classes/msds604/Final_Project/train.csv',header = TRUE)
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

## Using VAR(p) models - endogenous variables 
## population - population size
## unemployment - unemployment rate 
## house_price - housing price index - price changes of residential housing as a percentage change from some specific start date 
## bankrupcy rate 
## from looking at the cross correlation matrix, Bankruptcy seems mostly correlated with housing and population rather than unemployement 

### building model using training (after splitting for crossvalidation)
train_Bankruptcy <- ts(train$Bankruptcy_Rate, start = c(1987, 1), frequency = 12)
train_Unemployment<- ts(train$Unemployment_Rate, start = c(1987, 1), frequency = 12)
train_Population <- ts(train$Population, start = c(1987, 1), frequency = 12)
train_HousePrice <- ts(train$House_Price_Index, start = c(1987, 1), frequency = 12)

# Plot the time series
par(mfrow=c(1,1))
plot(train_Bankruptcy)
plot(train_Unemployment)
plot(train_Population)
plot(train_HousePrice)

## selecting p parameter 
select_p <- VARselect(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), lag.max = 20)

## Lowest AIC outputted by lag of 17. Try fitting 17 models 


## lets try iterating with simpler models (for parsimony)
## select one with lowest rmse? 
train_var_p1 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 1)
train_pred_p1 <- predict(train_var_p1, n.ahead = 60, ci = 0.95)
rmse.var_p1 <- sqrt(mean((train_pred_p1$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))

train_var_p2 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 2)
train_pred_p2 <- predict(train_var_p2, n.ahead = 60, ci = 0.95)
rmse.var_p2 <- sqrt(mean((train_pred_p2$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p3 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 3)
train_pred_p3 <- predict(train_var_p3, n.ahead = 60, ci = 0.95)
rmse.var_p3 <- sqrt(mean((train_pred_p3$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p4 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 4)
train_pred_p4 <- predict(train_var_p4, n.ahead = 60, ci = 0.95)
rmse.var_p4 <- sqrt(mean((train_pred_p4$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))

train_var_p5 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 5)
train_pred_p5 <- predict(train_var_p5, n.ahead = 60, ci = 0.95)
rmse.var_p5 <- sqrt(mean((train_pred_p5$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p6 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 6)
train_pred_p6 <- predict(train_var_p6, n.ahead = 60, ci = 0.95)
rmse.var_p6 <- sqrt(mean((train_pred_p6$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))

train_var_p7 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 7)
train_pred_p7 <- predict(train_var_p7, n.ahead = 60, ci = 0.95)
rmse.var_p7 <- sqrt(mean((train_pred_p7$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))

train_var_p8 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 8)
train_pred_p8 <- predict(train_var_p8, n.ahead = 60, ci = 0.95)
rmse.var_p8 <- sqrt(mean((train_pred_p8$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p9 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 9)
train_pred_p9 <- predict(train_var_p9, n.ahead = 60, ci = 0.95)
rmse.var_p9 <- sqrt(mean((train_pred_p9$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p10 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 10)
train_pred_p10 <- predict(train_var_p10, n.ahead = 60, ci = 0.95)
rmse.var_p10 <- sqrt(mean((train_pred_p10$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))

train_var_p11 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 11)
train_pred_p11 <- predict(train_var_p11, n.ahead = 60, ci = 0.95)
rmse.var_p11 <- sqrt(mean((train_pred_p11$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p12 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 12)
train_pred_p12 <- predict(train_var_p12, n.ahead = 60, ci = 0.95)
rmse.var_p12 <- sqrt(mean((train_pred_p12$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p13 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 13)
train_pred_p13 <- predict(train_var_p13, n.ahead = 60, ci = 0.95)
rmse.var_p13 <- sqrt(mean((train_pred_p13$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p14 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 14)
train_pred_p14 <- predict(train_var_p14, n.ahead = 60, ci = 0.95)
rmse.var_p14 <- sqrt(mean((train_pred_p14$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p15 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 15)
train_pred_p15 <- predict(train_var_p15, n.ahead = 60, ci = 0.95)
rmse.var_p15 <- sqrt(mean((train_pred_p15$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p16 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 16)
train_pred_p16 <- predict(train_var_p16, n.ahead = 60, ci = 0.95)
rmse.var_p16 <- sqrt(mean((train_pred_p16$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))


train_var_p17 <- VAR(y = data.frame(train_Bankruptcy, train_HousePrice, train_Population, train_Unemployment), p = 17)
train_pred_p17 <- predict(train_var_p17, n.ahead = 60, ci = 0.95)
rmse.var_p17 <- sqrt(mean((train_pred_p17$fcst$train_Bankruptcy[,1] - crossval$Bankruptcy_Rate)^2))

## comparing the models 
var_models_df <- data.frame(p_parameter=seq(1,17), rmse_crossval_bankruptcy = c(rmse.var_p1,rmse.var_p2, rmse.var_p3,rmse.var_p4, rmse.var_p5, rmse.var_p6, rmse.var_p7, rmse.var_p8, rmse.var_p9, rmse.var_p10, rmse.var_p11, rmse.var_p12, rmse.var_p13, rmse.var_p14, rmse.var_p15, rmse.var_p16, rmse.var_p17), train_aic = select_p$criteria[1,][1:17])

# arranged by lowest AIC 
var_models_df[order(var_models_df$train_aic),]

# arranged by lowest RMSE (when validated with cross validation set)
var_models_df[order(var_models_df$rmse_crossval_bankruptcy),]

# Notes: model with lowest RMSE chooses p = 9; model with lowest AIC chooses p = 17; maybe go for p = 9 for a simpler model? 

## using VAR(p=9) model to forecast into 2015 to 2017 for bankruptcy 
## n.ahead = 96 because 60 months from the cross validation set and 36 months from what we're actually trying to forecast 
pred_p9_2015_2017 <- predict(train_var_p9, n.ahead = 96, ci = 0.95)
plot(pred_p9_2015_2017)

## checking the acucracy between the test data set (unemployment, housing index, and population)
house_forecast_vals <- pred_p9_2015_2017$fcst$train_HousePrice[,1][61:96]
population_forecast_vals <- pred_p9_2015_2017$fcst$train_Population[,1][61:96]
unemployment_forecast_vals <- pred_p9_2015_2017$fcst$train_Unemployment[,1][61:96]

## reading in the test data 
test_data <- read.csv('./data/test.csv')
test_unemployment<- ts(test_data$Unemployment_Rate, start = c(2015, 1), frequency = 12)
test_population <- ts(test_data$Population, start = c(2015, 1), frequency = 12)
test_housePrice <- ts(test_data$House_Price_Index, start = c(2015, 1), frequency = 12)

## housing index 
accuracy(house_forecast_vals, as.numeric(test_housePrice))

## population 
accuracy(population_forecast_vals, as.numeric(test_population))

## unemployment 
accuracy(unemployment_forecast_vals, as.numeric(test_unemployment))

## notes:
## - low RMSE values for house and unemployement (6.891138 and 1.73542 respectively)
## - pretty large RMSE values for population (718321.4)


## accuracy between the point forecast and cross validation values for bankruptcy
## 60 months of the cross validation set 
pred_p9_crossval <- predict(train_var_p9, n.ahead = 60, ci = 0.95)
accuracy(pred_p9_crossval$fcst$train_Bankruptcy[,1], crossval$Bankruptcy_Rate)


## flaw with using this extra cross validation split, the forecasts into the cross validation set + what we're predicting (2015-2017) might be more inaccurate because we're using forecasts (ones made on cross validation set) to forecast future predictions (2015-2017)

### PLOT: overlay the actual bankruptcy rates from cross validation onto the train_bankruptcy forecast plot 
## plot the actual bankruptcy rates for the full training set (train+crossval)
## plot the forecast for bankruptcy of cross validation and testing years 

bankruptcy_actual_df <- data.frame(time = as.numeric(t), 
                       actual_bank = as.numeric(Bankruptcy))

## grab the time information for the predicted years from 2015 - 2017 
time_2015_2017 <- as.numeric(time(test_unemployment))
## grab the time information for the cross validation years 
time_crossval <- as.numeric(t)[277:336]
## combine these years together 
time_forecast <- c(time_crossval, time_2015_2017)

bankruptcy_forecast_df <- data.frame(time = time_forecast, forecast_bank = pred_p9_2015_2017$fcst$train_Bankruptcy[,1], upper = pred_p9_2015_2017$fcst$train_Bankruptcy[,3], lower = pred_p9_2015_2017$fcst$train_Bankruptcy[,2])

# bankruptcy_forecast_df_long <- data.frame(time = c(rep(NA, 240), time_forecast), forecast_bank = c(rep(NA, 240), pred_p9_2015_2017$fcst$train_Bankruptcy[,1]), upper = c(rep(NA, 240), pred_p9_2015_2017$fcst$train_Bankruptcy[,3]), lower = c(rep(NA, 240), pred_p9_2015_2017$fcst$train_Bankruptcy[,2]))

bankruptcy_forecast_df_full <- dplyr::full_join(bankruptcy_actual_df, bankruptcy_forecast_df)

bankruptcy_forecast_df_long <- tidyr::gather(bankruptcy_forecast_df_full, key=type, values=actual_bank:lower, factor_key=TRUE)

# ggplot() + 
#     geom_line(data = bankruptcy_actual_df, aes(time, actual_bank), stat = "identity", show.legend = TRUE) +
#     geom_line(data = bankruptcy_forecast_df, aes(time, forecast_bank), stat = "identity", color = "red", show.legend = TRUE) + geom_line(data = bankruptcy_forecast_df, aes(time, upper), stat = "identity", color = "blue", show.legend = TRUE)+ geom_line(data = bankruptcy_forecast_df, aes(time, lower), stat = "identity", color = "blue", show.legend = TRUE)

ggplot() + 
    geom_line(data = bankruptcy_forecast_df_long, aes(time, value, color = type), stat = "identity") + scale_color_manual(values = c("actual_bank" = "black", "forecast_bank" = "red", "upper" = "blue", "lower" = "blue")) + labs(x = "Time (years)", y = "Bankruptcy Rate (%)", title = "Predicted Bankruptcy Rate (2015-2017)") + scale_x_continuous(breaks=seq(1990,2017,5))  +theme(plot.title = element_text(hjust = 0.5))


############ Training on the entire training data set with VAR(9) model ############ 
full_train_var_p9 <- VAR(y = data.frame(Bankruptcy, HousePrice, Population, Unemployment), p = 9)

pred_p9 <- predict(full_train_var_p9, n.ahead = 36, ci =
                     0.95)

## plot Bankruptcy, houseprice, population, and unemployment 
plot(pred_p9)


bankruptcy_forecast_df <- data.frame(time = time_2015_2017, forecast_bank = pred_p9$fcst$Bankruptcy[,1], upper = pred_p9$fcst$Bankruptcy[,3], lower = pred_p9$fcst$Bankruptcy[,2])

bankruptcy_forecast_df_full <- dplyr::full_join(bankruptcy_actual_df, bankruptcy_forecast_df)

bankruptcy_forecast_df_long <- tidyr::gather(bankruptcy_forecast_df_full, key=type, values=actual_bank:lower, factor_key=TRUE)

dev.off()
ggplot() + 
    geom_line(data = bankruptcy_forecast_df_long, aes(time, value, color = type), stat = "identity") + scale_color_manual(values = c("actual_bank" = "black", "forecast_bank" = "red", "upper" = "blue", "lower" = "blue")) + labs(x = "Time (years)", y = "Bankruptcy Rate (%)", title = "Predicted Bankruptcy Rate (2015-2017)") + scale_x_continuous(breaks=seq(1990,2017,5)) +theme(plot.title = element_text(hjust = 0.5))
