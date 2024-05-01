#install necessary packages for analysis
install.packages("quantmod")
install.packages("tseries")
install.packages("ggplot2")
install.packages("forecast")
install.packages("rugarch")
install.packages("rmgarch")
install.packages("anytime")

#include necessary packages for analysis
require(quantmod)
library(tseries)
library(ggplot2)
library(forecast)
library(rugarch)
library(rmgarch)
library(readxl)
library("anytime")

Sys.setenv(TZ = "UTC")

## Getting the data of NIFTY50 and VGUARD Equity ##

NSE_D <- getSymbols.yahoo("^NSEI", from = "2020-04-03", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "daily")
NSE_D <- na.omit(NSE_D)
head(NSE_D)
tail(NSE_D)

NSE_W <- getSymbols.yahoo("^NSEI", from = "2020-04-03", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "weekly")
NSE_W <- na.omit(NSE_W)
head(NSE_W)
tail(NSE_W)

NSE_M <- getSymbols.yahoo("^NSEI", from = "2020-04-03", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "monthly")
NSE_M <- na.omit(NSE_M)
head(NSE_M)
tail(NSE_M)


VGUARD_D <- getSymbols.yahoo("VGUARD.NS", from = "2020-04-03", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "daily")
VGUARD_D <- na.omit(VGUARD_D)
head(VGUARD_D)
tail(VGUARD_D)

VGUARD_W <- getSymbols.yahoo("VGUARD.NS", from = "2020-04-03", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "weekly")
VGUARD_W <- na.omit(VGUARD_W)
head(VGUARD_W)
tail(VGUARD_W)

VGUARD_M <- getSymbols.yahoo("VGUARD.NS", from = "2020-04-03", to = "2024-03-31", verbose = FALSE, auto.assign = FALSE, periodicity = "monthly")
VGUARD_M <- na.omit(VGUARD_M)
head(VGUARD_M)
tail(VGUARD_M)


## T-Bill Data ##

T_Bills_D_2023_I_Sem <- read_excel("T-Bills_2024.xlsx", sheet = "Daily")
T_Bills_D <- as.data.frame(T_Bills_D_2023_I_Sem)
head(T_Bills_D)
tail(T_Bills_D)
T_Bills_D$Date <- anytime(T_Bills_D$Date)
rownames(T_Bills_D) <- as.Date(T_Bills_D[,1]) 
D_TBills_xts <- xts(T_Bills_D[,-1], order.by=T_Bills_D[,1])
head(D_TBills_xts)

T_Bills_W_2023_I_Sem <- read_excel("T-Bills_2024.xlsx", sheet = "Weekly")
T_Bills_W <- as.data.frame(T_Bills_W_2023_I_Sem)
head(T_Bills_W)
tail(T_Bills_W)
#T_Bills_W$Week <- anytime(T_Bills_W$Week)
rownames(T_Bills_W) <- as.Date(T_Bills_W[,1]) 
W_TBills_xts <- xts(T_Bills_W[,-1], order.by=T_Bills_W[,1])
head(W_TBills_xts)

T_Bills_M_2023_I_Sem <- read_excel("T-Bills_2024.xlsx", sheet = "Monthly")
T_Bills_M <- as.data.frame(T_Bills_M_2023_I_Sem)
head(T_Bills_M)
tail(T_Bills_M)
#T_Bills_M$Month <- anytime(T_Bills_M$Month)
head(T_Bills_M)
rownames(T_Bills_M) <- as.Date(T_Bills_M[,1]) 
M_TBills_xts <- xts(T_Bills_M[,-1], order.by=T_Bills_M[,1])
head(M_TBills_xts)


### Part 1.1 - Estimating Beta using the CAPM model ###

#Making a dataframe of the closing prices
Close_D <- cbind(NSE_D$NSEI.Close, VGUARD_D$VGUARD.NS.Close)
head(Close_D, 5)

Close_W <- cbind(NSE_W$NSEI.Close, VGUARD_W$VGUARD.NS.Close)
head(Close_W, 5)

Close_M <- cbind(NSE_M$NSEI.Close, VGUARD_M$VGUARD.NS.Close)
head(Close_M, 5)

#Calculating the returns
plot(Close_D$NSEI.Close, mar=c(6,4.1,4.1,2.1))
title(main = "NIFTY Daily Price", xlab = "Time", ylab = "Price", 
      cex.main = 4,   font.main = 1, cex.lab = 1.5)
plot(Close_D$VGUARD.NS.Close, mar=c(6,4.1,4.1,2.1))
title(main = "VGUARD Daily Price", xlab = "Time", ylab = "Price", 
      cex.main = 2,   font.main = 1, cex.lab = 1.5)
Returns_D <- as.xts(tail(data.frame(Close_D),-1)/head(data.frame(Close_D),-1) - 1)
head(Returns_D, 5)
tail(Returns_D, 5)
D_Ret_VGUARD <- Returns_D$VGUARD.NS.Close
D_Ret_VGUARD <- na.omit(D_Ret_VGUARD)
plot(D_Ret_VGUARD, mar=c(6,4.1,4.1,2.1))
title(main = "VGUARD Daily Returns", xlab = "Time", ylab = "Returns", 
      cex.main = 2,   font.main = 1, cex.lab = 1.5)
(1 + mean(D_Ret_VGUARD))^252 - 1
sd(D_Ret_VGUARD)*sqrt(252)
D_Ret_NIFTY <- Returns_D$NSEI.Close
D_Ret_NIFTY <- na.omit(D_Ret_NIFTY)
plot(D_Ret_NIFTY, mar=c(6,4.1,4.1,2.1))
title(main = "NIFTY Daily Returns", xlab = "Time", ylab = "Returns", 
      cex.main = 4,   font.main = 1, cex.lab = 1.5)
(1 + mean(D_Ret_NIFTY))^252 - 1
sd(D_Ret_NIFTY)*sqrt(252)

plot(Close_W$NSEI.Close, mar=c(6,4.1,4.1,2.1))
title(main = "NIFTY Weekly Price", xlab = "Time", ylab = "Price", 
      cex.main = 4,   font.main = 1, cex.lab = 1.5)
plot(Close_W$VGUARD.NS.Close, mar=c(6,4.1,4.1,2.1))
title(main = "VGUARD Weekly Price", xlab = "Time", ylab = "Price", 
      cex.main = 2,   font.main = 1, cex.lab = 1.5)
Returns_W <- as.xts(tail(data.frame(Close_W),-1)/head(data.frame(Close_W),-1) - 1)
head(Returns_W, 5)
tail(Returns_W, 5)
W_Ret_VGUARD <- Returns_W$VGUARD.NS.Close
W_Ret_VGUARD <- na.omit(W_Ret_VGUARD)
plot(W_Ret_VGUARD, mar=c(6,4.1,4.1,2.1))
title(main = "VGUARD Weekly Returns", xlab = "Time", ylab = "Returns", 
      cex.main = 2,   font.main = 1, cex.lab = 1.5) 
(1 + mean(W_Ret_VGUARD))^52 - 1
sd(W_Ret_VGUARD)*sqrt(52)
W_Ret_NIFTY <- Returns_W$NSEI.Close
W_Ret_NIFTY <- na.omit(W_Ret_NIFTY)
plot(W_Ret_NIFTY, mar=c(6,4.1,4.1,2.1))
title(main = "NIFTY Weekly Returns", xlab = "Time", ylab = "Returns", 
      cex.main = 4,   font.main = 1, cex.lab = 1.5)
(1 + mean(W_Ret_NIFTY))^52 - 1
sd(W_Ret_NIFTY)*sqrt(52)

plot(Close_M$NSEI.Close, mar=c(6,4.1,4.1,2.1))
title(main = "NIFTY Monthly Price", xlab = "Time", ylab = "Price", 
      cex.main = 4,   font.main = 1, cex.lab = 1.5) 
plot(Close_M$VGUARD.NS.Close, mar=c(6,4.1,4.1,2.1))
title(main = "VGUARD Monthly Price", xlab = "Time", ylab = "Price", 
      cex.main = 2,   font.main = 1, cex.lab = 1.5) 
Returns_M <- as.xts(tail(data.frame(Close_M),-1)/head(data.frame(Close_M),-1) - 1)
head(Returns_M, 5)
tail(Returns_M, 5)
M_Ret_VGUARD <- Returns_M$VGUARD.NS.Close
M_Ret_VGUARD <- na.omit(M_Ret_VGUARD)
plot(M_Ret_VGUARD, mar=c(6,4.1,4.1,2.1))
title(main = "VGUARD Monthly Returns", xlab = "Time", ylab = "Returns", 
      cex.main = 2,   font.main = 1, cex.lab = 1.5) 
(1 + mean(M_Ret_VGUARD))^12 - 1
sd(M_Ret_VGUARD)*sqrt(12)
M_Ret_NIFTY <- Returns_M$NSEI.Close
M_Ret_NIFTY <- na.omit(M_Ret_NIFTY)
plot(M_Ret_NIFTY, mar=c(6,4.1,4.1,2.1))
title(main = "NIFTY Monthly Returns", xlab = "Time", ylab = "Returns", 
      cex.main = 4,   font.main = 1, cex.lab = 1.5)
(1 + mean(M_Ret_NIFTY))^12 - 1
sd(M_Ret_NIFTY)*sqrt(12)

#Calculating Excess Returns

exNSE_D <- Returns_D$NSEI.Close - D_TBills_xts
head(exNSE_D)
tail(exNSE_D)

exNSE_W <- Returns_W$NSEI.Close - W_TBills_xts 
head(exNSE_W)
tail(exNSE_W)

exNSE_M <- Returns_M$NSEI.Close - M_TBills_xts
head(exNSE_M)
tail(exNSE_M)


exVGUARD_D <- Returns_D$VGUARD.NS.Close - D_TBills_xts 
head(exVGUARD_D)
tail(exVGUARD_D)

exVGUARD_W <- Returns_W$VGUARD.NS.Close - W_TBills_xts 
head(exVGUARD_W)
tail(exVGUARD_W)

exVGUARD_M <- Returns_M$VGUARD.NS.Close - M_TBills_xts 
head(exVGUARD_M)
tail(exVGUARD_M)


#Running the regression model
D_regression <- lm(exVGUARD_D ~ exNSE_D)
W_regression <- lm(exVGUARD_W ~ exNSE_W)
M_regression <- lm(exVGUARD_M ~ exNSE_M)

#Slope parameter = beta in CAPM model
summary(D_regression) #Daily Beta of VGUARD equity is 0.842 and it is statistically significant (p-value < 1%)
summary(W_regression) #Weekly Beta of VGUARD equity is 0.848 and it is statistically significant (p-value < 1%)
summary(M_regression) #Monthly Beta of VGUARD equity is 1.178 and it is statistically significant (p-value < 1%)


### Part 1.2 - Estimating AR and MA coefficients using ARIMA ###

#Calculating the Returns
D_Returns_VGUARD <- Returns_D$VGUARD.NS.Close
D_Returns_VGUARD <-na.omit(D_Returns_VGUARD)
colnames(D_Returns_VGUARD) <- "Returns"
head(D_Returns_VGUARD, 5)
TS_D_Returns_VGUARD <- ts(D_Returns_VGUARD, frequency = 1)
head(TS_D_Returns_VGUARD)
plot(VGUARD_D$VGUARD.NS.Close)		#can be seen that data is non-stationary
plot(D_Returns_VGUARD$Returns)	#can seem more stationary with mean ~ 0 with very few outliers OR first difference of closing price
plot(TS_D_Returns_VGUARD)

W_Returns_VGUARD <- Returns_W$VGUARD.NS.Close
W_Returns_VGUARD <-na.omit(W_Returns_VGUARD)
colnames(W_Returns_VGUARD) <- "Returns"
head(W_Returns_VGUARD, 5)
TS_W_Returns_VGUARD <- ts(W_Returns_VGUARD, frequency = 1)
head(TS_W_Returns_VGUARD)
plot(VGUARD_W$VGUARD.NS.Close)		#can be seen that data is non-stationary
plot(W_Returns_VGUARD$Returns)	#can seem more stationary with mean ~ 0 with very few outliers OR first difference of closing price
plot(TS_W_Returns_VGUARD)

M_Returns_VGUARD <- Returns_M$VGUARD.NS.Close
M_Returns_VGUARD <-na.omit(M_Returns_VGUARD)
colnames(M_Returns_VGUARD) <- "Returns"
head(M_Returns_VGUARD, 5)
TS_M_Returns_VGUARD <- ts(M_Returns_VGUARD, frequency = 1)
head(TS_M_Returns_VGUARD)
plot(VGUARD_M$VGUARD.NS.Close)		#can be seen that data is non-stationary
plot(M_Returns_VGUARD$Returns)	#can seem more stationary with mean ~ 0 with very few outliers OR first difference of closing price
plot(TS_M_Returns_VGUARD)


#Stationarity Test

adf.test(D_Returns_VGUARD$Returns, alternative = "stationary")	#as p<0.05 ==> returns are stationary
adf.test(TS_D_Returns_VGUARD, alternative = "stationary")

adf.test(W_Returns_VGUARD$Returns, alternative = "stationary")	#as p<0.05 ==> returns are stationary
adf.test(TS_W_Returns_VGUARD, alternative = "stationary")

adf.test(M_Returns_VGUARD$Returns, alternative = "stationary")	#as p<0.05 ==> returns are stationary
adf.test(TS_M_Returns_VGUARD, alternative = "stationary")


#ACF and PACF plots for getting order of AR and MA terms
plot(acf(D_Returns_VGUARD$Returns, lag.max = 10))			#for MA
plot(acf(TS_D_Returns_VGUARD, lag.max = 10), main = "Returns", mar=c(5,5,4,1))
plot(pacf(D_Returns_VGUARD$Returns, lag.max = 10))			#for AR
plot(pacf(TS_D_Returns_VGUARD, lag.max = 10), main = "Returns", mar=c(5,5,4,1))

plot(acf(W_Returns_VGUARD$Returns, lag.max = 10))			#for MA
plot(acf(TS_W_Returns_VGUARD, lag.max = 10), main = "Returns", mar=c(5,5,4,1))
plot(pacf(W_Returns_VGUARD$Returns, lag.max = 10))			#for AR
plot(pacf(TS_W_Returns_VGUARD, lag.max = 10), main = "Returns", mar=c(5,5,4,1))

plot(acf(M_Returns_VGUARD$Returns, lag.max = 10))			#for MA
plot(acf(TS_M_Returns_VGUARD, lag.max = 10), main = "Returns", mar=c(5,5,4,1))
plot(pacf(M_Returns_VGUARD$Returns, lag.max = 10))			#for AR
plot(pacf(TS_M_Returns_VGUARD, lag.max = 10), main = "Returns", mar=c(5,5,4,1))


#Running ARIMA(p,d,q) models for various orders
auto.arima(D_Returns_VGUARD) #Gives best-fit ARIMA model ((0, 0, 0) in this case)
auto.arima(TS_D_Returns_VGUARD) #Gives best-fit ARIMA model ((0, 0, 0) in this case)
D_arima_final <- arima(D_Returns_VGUARD, order = c(0,0,0))	#Try for different ARIMA orders and choose the one with lower AIC value
D_arima_final
D_arima_final_TS <- arima(TS_D_Returns_VGUARD, order = c(0,0,0))	#Try for different ARIMA orders and choose the one with lower AIC value
D_arima_final_TS

auto.arima(W_Returns_VGUARD) #Gives best-fit ARIMA model ((1, 0, 0) in this case)
auto.arima(TS_W_Returns_VGUARD) #Gives best-fit ARIMA model ((1, 0, 0) in this case)
W_arima_final <- arima(W_Returns_VGUARD, order = c(1,0,0))	#Try for different ARIMA orders and choose the one with lower AIC value
W_arima_final
W_arima_final_TS <- arima(TS_W_Returns_VGUARD, order = c(1,0,0))	#Try for different ARIMA orders and choose the one with lower AIC value
W_arima_final_TS

auto.arima(M_Returns_VGUARD) #Gives best-fit ARIMA model ((0, 0, 0) in this case)
auto.arima(TS_M_Returns_VGUARD) #Gives best-fit ARIMA model ((0, 0, 0) in this case)
M_arima_final <- arima(M_Returns_VGUARD, order = c(0,0,0))	#Try for different ARIMA orders and choose the one with lower AIC value
M_arima_final
M_arima_final_TS <- arima(TS_M_Returns_VGUARD, order = c(0,0,0))	#Try for different ARIMA orders and choose the one with lower AIC value
M_arima_final_TS


#Predicting using the fitted ARIMA model
D_predicted <- predict(D_arima_final, n.ahead = 10)
D_predicted
D_predicted_TS <- predict(D_arima_final_TS, n.ahead = 10)
D_predicted_TS

W_predicted <- predict(W_arima_final, n.ahead = 10)
W_predicted
W_predicted_TS <- predict(W_arima_final_TS, n.ahead = 10)
W_predicted_TS

M_predicted <- predict(M_arima_final, n.ahead = 10)
M_predicted
M_predicted_TS <- predict(M_arima_final_TS, n.ahead = 10)
M_predicted_TS


#Diagnosis of the model
par(mar=c(3,3,3,3))  #Modifying graphic parameters/margins so as to get appropriate diagnosis plots
tsdiag(D_arima_final)
tsdiag(D_arima_final_TS)
par(mar=c(5.1,4.1,4.1,2.1)) #Setting graphic parameters/margins back to default

par(mar=c(3,3,3,3))  #Modifying graphic parameters/margins so as to get appropriate diagnosis plots
tsdiag(W_arima_final)
tsdiag(W_arima_final_TS)
par(mar=c(5.1,4.1,4.1,2.1)) #Setting graphic parameters/margins back to default

par(mar=c(3,3,3,3))  #Modifying graphic parameters/margins so as to get appropriate diagnosis plots
tsdiag(M_arima_final)
tsdiag(M_arima_final_TS)
par(mar=c(5.1,4.1,4.1,2.1)) #Setting graphic parameters/margins back to default



### Part 1.3 - GARCH and EGARCH models ###

#Getting returns
D_Ret_VGUARD <- Returns_D$VGUARD.NS.Close
D_Ret_VGUARD <- na.omit(D_Ret_VGUARD)
W_Ret_VGUARD <- Returns_W$VGUARD.NS.Close
W_Ret_VGUARD <- na.omit(W_Ret_VGUARD)
M_Ret_VGUARD <- Returns_M$VGUARD.NS.Close
M_Ret_VGUARD <- na.omit(M_Ret_VGUARD)

#Implementing univariate GARCH (default: GARCH(1,1))
ug_spec = ugarchspec()
ug_spec

#Implementing EGARCH
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit_D = ugarchfit(spec = ug_spec, data = D_Ret_VGUARD)
ugfit_D		#lower AIC value models are better
egfit_D = ugarchfit(spec = eg_spec, data = D_Ret_VGUARD)
egfit_D		#lower AIC value models are better

ugfit_W = ugarchfit(spec = ug_spec, data = W_Ret_VGUARD,solver.control = list(tol = 1e-12),solver = 'hybrid')
ugfit_W		#lower AIC value models are better
egfit_W = ugarchfit(spec = eg_spec, data = W_Ret_VGUARD,solver.control = list(tol = 1e-12),solver = 'hybrid')
egfit_W		#lower AIC value models are better

ugfit_M = ugarchfit(spec = ug_spec, data = M_Ret_VGUARD)
ugfit_M		#lower AIC value models are better
egfit_M = ugarchfit(spec = eg_spec, data = M_Ret_VGUARD)
egfit_M		#lower AIC value models are better


#Forecasting
ugforecast_D = ugarchforecast(ugfit_D, n.ahead = 10)
ugforecast_D
egforecast_D = ugarchforecast(egfit_D, n.ahead = 10)
egforecast_D

ugforecast_W = ugarchforecast(ugfit_W, n.ahead = 10)
ugforecast_W
egforecast_W = ugarchforecast(egfit_W, n.ahead = 10)
egforecast_W

ugforecast_M = ugarchforecast(ugfit_M, n.ahead = 10)
ugforecast_M
egforecast_M = ugarchforecast(egfit_M, n.ahead = 10)
egforecast_M