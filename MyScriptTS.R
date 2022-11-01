################################################################################
#########  Project Advances Time Series - Martina Chiesa and Matteo Zucca	######
################################################################################

#### Introduction:
# Financial Time Series Analysis of Intesa San Paolo Daily Prices
# Data are downloaded from Yahoo Finance


####### 1) Prepare Environment ####

#### 1.1) Load libraries and auxiliary functions

rm(list = ls())


library(rugarch)  # fits Arima and Garch models
library(forecast) # Rob Hyndman's package, contains Acf(.) function (it excludes first lag, acf(.) doesn't)
library(xts)      # xts object has some additional features rather than ts object (R's default)
library(quantmod) # with getSymbols function we can download data


# setwd(getwd()) # other users should upload functions and data by changing the path
data.path = "C:\\Users\\Matteo\\Desktop\\Advanced Time Series\\Project\\Project TSA"
setwd(data.path)
source("SupportFunctions.R")


#### 1.2) Download and store Data 
# Notice: section 1.2 should be executed only first time to download
#         data, otherwise results will change day by day

###
# symbol = "ISP.MI"
# x1 = getSymbols(Symbols = symbol,
#                 src = "yahoo",
#                 auto.assign = FALSE,
#                 from = "2003-01-01")
# 
# colnames(x1) = gsub(x = colnames(x1), 
#                     pattern = paste0(symbol, "."), 
#                     replacement = "" )
# 
# x1 = data.frame(Date = index(x1), x1, check.names = FALSE)
# 
# x1[rowSums(is.na(x1)) > 0, ]; x1 = na.omit(x1); summary(x1) # remove NA's records
# 
# write.table(x = x1, file = data.path, quote = FALSE, sep = "\t", na = ".",
# 		        row.names = FALSE, col.names = TRUE)
###

#### 1.3) Upload Data in the Work Environment, Compute Garman-Klass and Extracr Variables

# Upload Data in the Work Environment
file.name = paste(data.path, "RawDataISP.csv", sep = "\\")
RawData = read.csv(file = file.name, header = T, sep = ",", dec = ".")

head(RawData); str(RawData); anyNA(RawData); summary(RawData); dim(RawData)

# Compute Garman-Klass measures
RawDataFinal = data.frame(RawData, 
                          log.return = c(NA, diff(log(RawData$Adjusted))), 
                          # log-returns
                          check.names = TRUE) 

# Note: we have to include a NA in order to match the dimension of dataframe
#       and requirements set by garmanklass.measure function

# Remove NA
head(RawDataFinal)
RawDataFinal = na.omit(RawDataFinal) 
rownames(RawDataFinal) = 1 : nrow(RawDataFinal)
anyNA(RawDataFinal); dim(RawDataFinal)

# Extract Variables
time = as.Date(x = RawDataFinal$Date)
close.price = RawDataFinal$Close
adj.price = RawDataFinal$Adjusted
nobs = NROW(adj.price)
# Obs: adjusted prices are a transformation of close price that takes into
#      account splits and dividends of the firms


####### 2) Preliminary Analysis of Original Data ####
# Purpose of this section is to prove that, when dealing with stock prices,
# modelling the mean isn't possible since homoeskedasticity assumption doesn't hold

# Plots
par(mfrow = c(2,2))
plot(time, close.price, main = "Close Prices", xlab = "Time", ylab = "Close Price", type = "l")
plot(time, log(close.price), main = "LN of Close Prices", xlab = "Time", ylab = expression(log("ClosePrice")), type = "l")
plot(time, adj.price, main = "Close Adjusted Prices", xlab = "Time", ylab = "Adj Close Price", type = "l")
plot(time, log(adj.price),  main = "LN of Close Adj Prices", xlab = "Time", ylab = expression(log("AdjClosePrice")), type = "l")
par(mfrow = c(1,1))
# Notice:
# Serie of closing price isn't stationary in mean
# We have clusters of variability (stylized fact of financial ts)
# -> heteroschedasticty (either for log transform)
# -> Arima relies on homoschedasticity

Acf(log(adj.price), lag.max = 100, type = "correlation", main = expression(log("AdjClosePrice"))) 
# Comment: decay of ACF is too slow -> serie is clearly non-stationary
#          tests for Unit Root will lead to this conclusion (omitted)

## Final Comments: 
#  1. Both series of close price and its log transform are non stationary and heteroskedastic
#  2. Log scale allows to appreciate better the price changes.
#     However it isn't a variance-stabilizing transformation.
#  3. In next session, we'll use difference of logs to get a stationary (in mean) serie
#     In fact, this correspond to log-return



####### 3) Preliminary analyses of log-returns ####

y.ret = xts(x = 100 * RawDataFinal$log.return, order.by = time) 
head(y.ret); summary(y.ret) # values are logarithmic returns (in percentage) 

# Plot of the serie
plot(x = time, y = y.ret, 
     main = "Percentage Logarithmic  Returns", xlab = "Time", 
     ylab = paste(expression(log(Returns)),"%"), type = "l")

# ACF and PACF
par(mfrow = c(2,1))
Acf(x = y.ret, lag.max = 100, type = "correlation", main = paste("ACF of", expression(log(Returns)),"%"))
Acf(x = y.ret, lag.max = 100, type = "partial", main =  paste("PACF of", expression(log(Returns)),"%"))
par(mfrow = c(1,1))

# Unconditional distribution
par(mfrow = c(1,2))
hist(x = y.ret, xlim = c(-10, 10), breaks = 200, main = "Returns", freq = F)
norm_theo = seq(from = min(y.ret), to = max(y.ret)+1, length.out = 100) 
lines(norm_theo, y = dnorm(norm_theo, 0, sd(y.ret)), col = "red") 
qqnorm(y = scale(y.ret)); abline(a = 0, b = 1, col = "red") 
par(mfrow = c(1,1))


##  Comments: 
#   1. Daily log-returns move around a mean close to zero (similarly to a WN); 
#   2. There are periods with different variability around the mean (heteroskedasticity)
#      This is a consequence of clusters of variability (stylized facts of financial TS)
#   3. We have several outliers depending on exogenous shocks (e.g. economic crisis)
#   4. From ACF, we can notice some lags that exceed the significance bands.
#      However, in absolute value correlations aren't that high.
#   5. Normality is rejected: Distribution seems leptokurtic.


## Despite these evidence, one may still think that log-return is a N-WN.
# To exclude definitively this hypothesis: plot ACF of squared return.
# It can be considered a (very) proxy measure for conditional variance, then if
# there's serial autocorrelation it can not be a WN.
# Furthermore, in a WN every transformation leads to a WN.


par(mfrow = c(2,1))
Acf(y.ret^2, lag.max = 100, type = "correlation", main = expression(Returns^2))
Acf(abs(y.ret), lag.max = 100, type = "correlation", main = expression("|Returns|") )
par(mfrow = c(1,1))



####### 4) ARMA modelling (through rugarch package) ####


##### 4.1) Fit Models

# ARMA(0,0)-std  AIC 4.396 BIC 4.4
# ARMA(0,0)-norm AIC 4.716 BIC 4.718 -> Comment: worse, use standardized-t distrib for errors 
# ARMA(1,1)-std  AIC 4.397 BIC 4.404 -> As expected, ICs don't improve
# ARMA(2,1)-std  AIC 4.39  BIC 4.403

?arfimaspec
spec0 = arfimaspec(mean.model = list(armaOrder = c(2,1), 
                                     include.mean = TRUE), 
                   distribution.model = "std") 
fit0 = arfimafit(spec = spec0, data = y.ret, solver = "solnp") 

infocriteria(fit0)[1:2,]


##### 4.2) Diagnostics on Residuals

res = as.numeric(residuals(fit0)) 
par(mfrow = c(3,1))
Acf(x = res, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = abs(res), lag.max = 100, type = "correlation", main = "|res|")
Acf(x = res^2, lag.max = 100, type = "correlation", main = expression(res^2))
par(mfrow = c(1,1))

## Comments:
#   1. Changing order of Arma and distribution of error doesn't improve
#      the problem of correlation of residuals.
#   2. Residuals of an Arma model aren't WN.
#   3. Model for the mean isn't adeguate.



####### 5) GARCH modelling ####
# In this section, we'll explore various kinds of garch models
# Only pure-garch (no ARMA-Garch) since conditional mean is roughly constant (near zero)

#### 5.1) Simple GARCH

## 5.1.1) Fit and Statistics

# default is ARMA(1,1)+const -> exclude it (constant is in s-Garch too)
spec1 = ugarchspec(
          variance.model = list(model = "sGARCH", 
                                garchOrder = c(1,1)), # often (1,1) is "enough"
          mean.model = list(armaOrder = c(0, 0), 
                            include.mean = FALSE),
          distribution.model = "std")

fit1 = ugarchfit(spec = spec1, data = y.ret, solver = "solnp") # Model: constant + garch

infocriteria(fit1)[1:2,] 
# Notice: AIC 4.39 (Arima Homosch) moved to AIC 4.193 (const+sGarch)
fit1@fit$robust.matcoef 
# Constrain: alpha1 + beta1 < 1 = 0.09187441+0.90527226 < 1 = 0.9971467 < 1
# If the constrain doesn't hold, the variance is infinite


## 5.1.1) Diagnostic on Standardized Residuals

fit = fit1
res.s.garch = fit@fit$z

par(mfrow = c(3,1))
Acf(res.s.garch,      lag.max = 100, type = "correlation", main = "z")
Acf(abs(res.s.garch), lag.max = 100, type = "correlation", main = "|z|")
Acf(res.s.garch^2,    lag.max = 100, type = "correlation", main = expression(z^2))
par(mfrow = c(1,1))

par(mfrow = c(1,2))
hist(x = res.s.garch, xlim = c(-5, 5), breaks = 200, main = "Residuals", freq = FALSE)
norm_theo = seq(from = min(res.s.garch), to = max(res.s.garch)+1, length.out = 100) 
lines(norm_theo, y = dnorm(norm_theo, 0, 1), col = "red") 
qqnorm(y = res.s.garch, pch = 19); abline(a = 0, b = 1, col = "red") 
par(mfrow = c(1,1))


## Comments: 
#   1. Simple-Garch may not be the best model but it has satisfying results (residuals NWN)
#   2. Diagnostic on residuals doesn't show any several problems.
#      However we can notice the influence of outliers. 
#      This behaviour is common and well-studied in finance, e.g. the theory of Black Swan.


#### 5.2) GJR-GARCH
# We introduce a leverage parameter due to the stylized fact that conditional 
# volatility is higher when returns are negative.
# This can be seen as a sort of market turmoil.

## 5.2.1) Fit and Statistics

spec2 = ugarchspec(
  variance.model = list(model = "gjrGARCH", 
                        garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(0, 0), 
                    include.mean = FALSE), 
  distribution.model = "std")


fit2 = ugarchfit(spec = spec2, data = y.ret, solver = "solnp")

infocriteria(fit2)[1:2,] 
# Notice: AIC AIC 4.193 (cost + sGarch) moved to 4.17 (Cost + GJR-Garch)
fit2@fit$robust.matcoef 
# Constrain: alpha1 + beta1 + gamma/2 < 1 = 0.009507673 + 0.924768331 + 0.127930937/2 < 1 = 0.9982415 < 1


## 5.2.2) Diagnostic on Residuals
# Note: Remember to use standardized residuals

fit = fit2
res.gjr.garch = fit@fit$z
  
par(mfrow = c(3,1))
Acf(res.gjr.garch,      lag.max = 100, type = "correlation", main = "z")
Acf(abs(res.gjr.garch), lag.max = 100, type = "correlation", main = "|z|")
Acf(res.gjr.garch^2,    lag.max = 100, type = "correlation", main = expression(z^2))
par(mfrow = c(1,1))

par(mfrow = c(1,2))
hist(x = res.gjr.garch, xlim = c(-5, 5), breaks = 200, main = "Residuals", freq = FALSE)
norm_theo = seq(from = min(res.gjr.garch), to = max(res.gjr.garch)+1, length.out = 100) 
lines(norm_theo, y = dnorm(norm_theo, 0, 1), col = "red") 
qqnorm(y = res.gjr.garch, pch = 19); abline(a = 0, b = 1, col = "red") 
par(mfrow = c(1,1))

## Comments:
#   1. GJR-Garch has slightly better diagnostic than s-Garch
#   2. Does it worths an higher complexity (leverage parameter)?
#      Answer has to be based on error measures.


#### 5.3) Further developments

# Literature proposed decades of garch models starting from 80's.
# For example: T-Garch, E-Garch, i-Garch and the entire family of f-Garch.
# They differ by several aspects. However often the simple-Garch(1,1) 
# has satisfying results and increasing complexity doesn't worth it.


###### 6) Forecasting (Ex-Post) ####
# Notice:
# 1.  We assume Garman-Klass measures as a less noisy proxy than squared return for
#     volatility at time t.
# 2.  We don't perform any kind of external validation. As a consequence, the error
#     may be underestimated.


#### 6.1) Calculate G-K volatility and Analyze them

y.gk = garmanklass.measure(data = RawDataFinal, volatility = TRUE) * 100 # variance, not volatility
head(y.gk)
y.gk[1] = sd(y.ret)

y.s.garch = fit1@fit$sigma
y.gjr.garch = fit2@fit$sigma

ylim.temp = range(max(y.s.garch), max(y.gjr.garch), max(y.gk),
                  min(y.s.garch), min(y.gjr.garch),  min(y.gk))

plot(x = time, y = y.gk, 
     ylim = ylim.temp, pch = 20, type = "p",
     ylab = "Garman-Klass volatility measure", xlab = "Time",
     main="Garch Forecasts Over Time")


lines(time, y.s.garch, col = "red", lwd = 2)
lines(time, y.gjr.garch, col = "cyan", lwd = 2)
legend("top", legend=c("S-Garch","GJR-Garch"),
       col = c("red","cyan"), lty = c(1,1), cex = 0.8,ncol = 1,lwd = 2) 



#### 6.2) Calculate Error Measures

## 6.2.1) Set Naive Model
# We assume as naive the unconditional variance or the unconditional st.dev (for volatility)

naive.vol = sd(y.ret)
naive.var = naive.vol^2 


## 6.2.2) Computer Error Measures

errors = data.frame(
  measure = rep(c("Volatility", "Variance"), each = 2),
  model = rep(c("S-Garch", "GJR-Garch"), times = 2), 
  rbind( 
        error.measures(y = y.gk,   fit = y.s.garch,   naive = naive.vol), 
        error.measures(y = y.gk,   fit = y.gjr.garch,   naive = naive.vol), 
        error.measures(y = y.gk^2, fit = y.s.garch^2, naive = naive.var), 
        error.measures(y = y.gk^2, fit = y.gjr.garch^2, naive = naive.var)
        ) 
  )

errors

## Comments:
#   1. Both models, compared to naive, performs well on predicting the volatility.
#   2. GJR-Garch allows a reduction of 7 percentage point w.r.t. S-GARCH in Scaled RMSE.
#   3. S-Garch doesn't works well on predicting the conditional variance if compared
#      to naive model.
#      However with the leverage parameter we can improve this inefficiency.




################################################################################
#### End of Project Advances Time Series - Martina Chiesa and Matteo Zucca #####
################################################################################