# Predict Bankruptcy Rate in Canada using Time Series Models

Accurately forecasting national bankruptcy rates is of interest to national banks, 
insurance companies, credit lenders, politicians, etc.  

In Canada, bankruptcies account for insolvent corporations who cannot repay their debts
to creditors and carry on with their business.

The training dataset holds 4 monthly data time series data from 1987 to 2014 of:
1) bankruptcy rate
2) unemployment rate 
3) population size
4) housing price index

Housing Price Index (HPI) is a measurement of average price changes in repeat sales or refinancing 
on the same single-family houses.

The goal of this project is to forecast monthly bankruptcy rate for the years from 2015 to 2017 using time series models.

In this repository, you will see the following models: 
1) SARIMA (Seasonal Autoregressive Integrated Moving Average)
2) Exponential Smoothing (Holt-Winters)
3) VAR (Vector Autoregression)
4) VARX (Vector Autoregression + Explanatory Variable)

Due to the correlations between variables, we chose VAR model. It exploits the potential bidirectional between those variables as well as accounts for trend and seasonality.
