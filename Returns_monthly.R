#install.packages("readxl") # doe dit eenmailg
library(readxl)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(stargazer)
library(TTR)
library(stats)
install.packages("sandwich")
install.packages("EconometricsUGent")
library(EconometricsUGent)
data_funds <- read_xlsx("data_funds.xlsx", sheet = "Sheet 1")
SocGen <- read_xlsx("data_funds.xlsx", sheet = "SocGen")
# View(data_funds)
# View(SocGen)

tickers <- c("MSCI Index", "SP500 index", "VTSAX.O", "SWPPX.O", "VFIAX.O", "FXIAX.O", "VTIAX.O","VBTLX.O","AGTHX.O","GFACX.O","VWNAX.O","FSPSX.O", "SocGenCTA", "SocGenCTATrend")
length(tickers)
mutualfund.index <- c("VTSAX.O", "SWPPX.O", "VFIAX.O", "FXIAX.O", "VTIAX.O","VBTLX.O","AGTHX.O","GFACX.O","VWNAX.O","FSPSX.O")
length(mutualfund.index)

# Set the dates for which we want to compare the returns
date.start <- as.Date("2012-12-31")
date.end   <- as.Date("2023-12-29")

# ----------------------------------------------------------QUESTION 1---------------------------------------------------------------------------
# Calculate the returns of an equally weighted mutual fund index, consisting of the ten mutual funds. 
# Compare the performance of the S&P500, the self-constructed mutual fund index and the two CTA indices.

#Calculate the returns 
returns <- CalculateReturns(data_funds) # compounded daily returns of all assets
returns <- returns[(-1),] # removal of the first row 
returns <- as.xts(returns)
length(returns)
# 1: Returns of the equally weighted mutual fund index
mutualfundreturns <- returns[,3:12]
w.mutualfundindex <- rep(1/length(mutualfund.index),length(mutualfund.index))
returns.mutualfundindex <- Return.portfolio(mutualfundreturns, w.mutualfundindex)
# 2: Returns of the S&P500
returns.sp500 <- returns[,2]
# 3: Returns of general index for CTA/managed futures
returns.CTA <- returns[,13]
# 4: Returns of trend following CTAs 
returns.trend <- returns[,14]

# Combining the four different strategies
overall.returns <- merge(returns.sp500, returns.mutualfundindex, returns.CTA, returns.trend)
colnames(overall.returns) <- c("S&P500", "Equally Weighted Mutual Fund Index", "CTA Managed Futures", "CTA Trend")
performance <- table.AnnualizedReturns(overall.returns)
skew <- apply(overall.returns, 2, skewness)
kurt <- apply(overall.returns, 2, kurtosis)
evaluation <- rbind(performance, Skewness = skew, Kurtosis = kurt)
stargazerTable(evaluation, fileDirectory = getwd(), fileName = "Strategies Evaluation")
stargazer(evaluation, type = "text", summary = FALSE)
chart.CumReturns(overall.returns, wealth.index = TRUE, legend.loc = "topleft", colorset = c("darkgreen", "red", "blue", "gold"))

# # Plot monthly returns
# plot.xts(returns, legend.loc = "topleft")
# # Plot monthly closing prices 
# plot.xts(as.xts(data_funds), legend.loc = "topleft" )
# plot.xts(as.xts(data_funds),legend.loc = "bottomleft" , ylim=c(0,500))



# ----------------------------------------------------------QUESTION 2---------------------------------------------------------------------------
# Treynor-Mazuy and Henrikkson-Merton test for market timing ability 

#risk free based https://www.worldgovernmentbonds.com/bond-historical-data/germany/10-years/#title-historical-data, where we've taken the yield from 
risk_free1    <-(0.01486+0.01941)/2  #average risk free rate from 31 dec. 2012 till 31 dec. 2013
risk_free2    <-(0.01941+0.00541)/2  #average risk free rate from 31 dec. 2013 till 31 dec. 2014
risk_free3    <-(0.00541+0.00635)/2  #average risk free rate from 31 dec. 2014 till 31 dec. 2015
risk_free4    <-(0.00635+0.00207)/2  #average risk free rate from 31 dec. 2015 till 31 dec. 2016
risk_free5    <-(0.00207+0.00426)/2  #average risk free rate from 31 dec. 2016 till 31 dec. 2017
risk_free6    <-(0.00426+0.00246)/2  #average risk free rate from 31 dec. 2017 till 31 dec. 2018
risk_free7    <-(0.00246-0.00188)/2  #average risk free rate from 31 dec. 2018 till 31 dec. 2019
risk_free8    <-(0.00188-0.00576)/2  #average risk free rate from 31 dec. 2019 till 31 dec. 2020
risk_free9    <-(0.00576-0.00182)/2  #average risk free rate from 31 dec. 2020 till 31 dec. 2021
risk_free10   <-(0.00182+0.02565)/2  #average risk free rate from 31 dec. 2021 till 31 dec. 2022
risk_free11   <-(0.02565+0.02031)/2  #average risk free rate from 31 dec. 2022 till 31 dec. 2023

risk_free<-sum(risk_free1,risk_free2,risk_free3,risk_free4,risk_free5,risk_free6,risk_free7,risk_free8, risk_free9,risk_free10, risk_free11) /11 #average risk free rate from 2012-12-31 till 2023-12-31
#Market timing abilities

# with MarketTiming
TM <- MarketTiming(as.xts(returns),as.xts(returns)[,2], risk_free, method = "TM")
View(TM)
HM <- MarketTiming(as.xts(returns),as.xts(returns)[,2], risk_free, method = "HM")
View(HM)

#manually
#Portfolio excess return
excess.return.mutualfunds    <- returns.mutualfundindex - risk_free
excess.return.market         <- returns.sp500 - risk_free
excess.return.market.squared <- excess.return.market^2                 # Need the squared excess market return for Treynor and Mazuy
excess.return.CTA            <- returns.CTA - risk_free
excess.return.trend          <- returns.trend - risk_free
HM.second.term               <- pmax(0, excess.return.market)          # Gain an extra return when the market is going down

TM.regression.mutualfund <- lm(excess.return.mutualfunds ~ excess.return.market + excess.return.market.squared, data = returns)
TM.regression.CTA        <- lm(excess.return.CTA ~ excess.return.market + excess.return.market.squared, data = returns)
TM.regression.trend      <- lm(excess.return.trend ~ excess.return.market + excess.return.market.squared, data = returns)
# summary(TM.regression.mutualfund)
# summary(TM.regression.CTA)
# summary(TM.regression.trend)
file.path <- "Treynor-Mazuy_Test.html"
stargazer(TM.regression.mutualfund, TM.regression.CTA, TM.regression.trend, title = "Treynor-Mazuy", 
          column.labels = c("HM Mutual Fund", "HM CTA", "HM Trend"), 
          out = file.path, header = FALSE, style = "default")

HM.regression.mutualfund <- lm(excess.return.mutualfunds ~ excess.return.market + HM.second.term, data = returns)
HM.regression.CTA        <- lm(excess.return.CTA ~ excess.return.market + HM.second.term, data = returns)
HM.regression.trend      <- lm(excess.return.trend ~ excess.return.market + HM.second.term, data = returns)
# summary(HM.regression.mutualfund)
# summary(HM.regression.CTA)
# summary(HM.regression.trend)
file.path <- "Merton-Henriksson_Test.html"
stargazer(TM.regression.mutualfund, TM.regression.CTA, TM.regression.trend, title = "Merton-Henriksson", 
          column.labels = c("HM Mutual Fund", "HM CTA", "HM Trend"), 
          out = file.path, header = FALSE, style = "default")

# Define the intervals for the largest price declines/crashes on the S&P500 index
market.crashes <- list(
  crash1 = as.Date(c("2015-08-19", "2015-08-26")),
  crash2 = as.Date(c("2015-12-30", "2016-02-11")),
  crash3 = as.Date(c("2018-01-29", "2018-04-02")),
  crash4 = as.Date(c("2018-10-04", "2018-12-24")),
  crash5 = as.Date(c("2020-02-20", "2020-03-23")),
  crash6 = as.Date(c("2021-12-28", "2022-10-12"))
)
# Function to check if a date falls within any of the defined intervals
date.in.crashes <- function(date, intervals) {
  any(sapply(intervals, function(interval) date >= interval[1] & date <= interval[2]))
}
# Create a dummy variable based on the crashes intervals
# Initialize an empty vector to store dummy variable values
dummy <- numeric(length(returns.sp500))
# Loop through each observation in the S&P500 returns
for (i in 1:length(returns.sp500)) {
  # Check if the date of the ith observation falls within any crash interval
  if (date.in.crashes(index(returns.sp500)[i], market.crashes)) {
    dummy.variable[i] <- 0  # If it does, set dummy variable to 0
  } else {
    dummy.variable[i] <- 1  # Otherwise, set dummy variable to 1
  }
}
View(as.matrix(dummy))
HM.regression.mutualfund.dummy <- lm(excess.return.mutualfunds ~ excess.return.market + HM.second.term + dummy, data = returns)
HM.regression.CTA.dummy        <- lm(excess.return.CTA ~ excess.return.market + HM.second.term + dummy, data = returns)
HM.regression.trend.dummy      <- lm(excess.return.trend ~ excess.return.market + HM.second.term + dummy, data = returns)
regressions.HM.dummy           <- list(HM.regression.mutualfund.dummy, HM.regression.CTA.dummy, HM.regression.trend.dummy)
stargazerRegression(regressions.HM.dummy, fileDirectory = getwd(), fileName = "Henriksson and Merton Test - Dummy")

# ----------------------------------------------------------QUESTION 3---------------------------------------------------------------------------
# Treynor-Mazuy and Henrikkson-Merton test for market timing ability when using dummy variable that is 0 for the six largest price declines during the sample period



MarketTimingAdapted <- function (Ra, Rb, Rf = 0, method = c("TM", "HM"))

{ # @author Andrii Babii, Peter Carl
  
  # FUNCTION
  
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  if (!is.null(dim(Rf))) 
    Rf = checkData(Rf)
  Ra.ncols = NCOL(Ra)
  Rb.ncols = NCOL(Rb)
  pairs = expand.grid(1:Ra.ncols, 1)
  method = method[1]
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  
  mt <- function (xRa, xRb)
  {
    switch(method,
           "HM" = { 
             intervals <- list(
               c("2015-08-19", "2015-08-26"),
               c("2015-12-30", "2016-02-11"),
               c("2018-01-29", "2018-04-02"),
               c("2018-10-04", "2018-12-24"),
               c("2020-02-20", "2020-03-23"),
               c("2021-12-28", "2022-10-12")
             )
             
             for (i in 1:length(intervals)) {
               start_date <- intervals[[i]][1]
               end_date <- intervals[[i]][2]
               if (any(xRb[,1] >= start_date & xRb[,1] <= end_date)) {
                 xRb[xRb[,1] >= start_date & xRb[,1] <= end_date, -1] <- 0
               }
             }
             S = xRb > 0
           },
           "TM" = { S = xRb }
    )
    
    
    R = merge(xRa, xRb, xRb*S)
    R.df = as.data.frame(R)
    model = lm(R.df[, 1] ~ 1 + ., data = R.df[, -1])
    return(coef(model))
  }
  
  result = apply(pairs, 1, FUN = function(n, xRa, xRb) 
    mt(xRa[, n[1]], xRb[, 1]), xRa = xRa, xRb = xRb)
  result = t(result)
  
  if (ncol(Rb) > 1){
    for (i in 2:ncol(xRb)){
      res = apply(pairs, 1, FUN = function(n, xRa, xRb) 
        mt(xRa[, n[1]], xRb[, i]), xRa = xRa, xRb = xRb)
      res = t(res)
      result = rbind(result, res)
    }
  }
  
  rownames(result) = paste(rep(colnames(Ra), ncol(Rb)), "to",  rep(colnames(Rb), each = ncol(Ra)))
  colnames(result) = c("Alpha", "Beta", "Gamma")
  return(result)
}
merged.returns <- merge(returns.trend,returns.CTA,returns.mutualfundindex)
TM.adapted <- MarketTimingAdapted(merged.returns, as.xts(returns)[,2], risk_free, method = "TM")
# View(TM.adapted)
HM.adapted <- MarketTimingAdapted(merged.returns,as.xts(returns)[,2], risk_free, method = "HM")
View(HM.adapted)

# ----------------------------------------------------------QUESTION 4---------------------------------------------------------------------------
# Dual Moving Average Crossover Strategy, MA of 20 and 100 days 
# Calculate the moving averages
#MA.20 <- SMA(returns, n = 20)
#MA.100 <- SMA(returns, n = 100)
#signals <- ifelse(MA.20 > MA.100, 1, -1) # Buy signal when MA-20 > MA-100, sell signal otherwise 
# Dual Moving Average Crossover Strategy, MA of 20 and 100 days 
# Calculate the moving averages
sp500.index <- as.matrix(data_funds[,2])
MA.20 <- SMA(sp500.index, n = 20)
MA.100 <- SMA(sp500.index, n = 100)
signals <- ifelse(MA.20 > MA.100, 1, -1)
signals <- signals[-1]
clean_data <- returns.sp500[-c(1:98), ]
strategy.returns <- signals * returns.sp500
na.omit(strategy.returns)
strategy.performance <- table.AnnualizedReturns(strategy.returns)
print(strategy.performance)
performanceSP500 <- table.AnnualizedReturns(clean_data)
print(performanceSP500)




