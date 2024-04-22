#install.packages("readxl") # doe dit eenmailg
library(readxl)
data_funds <- read_xlsx("data_funds.xlsx", sheet = "Sheet 1")
SocGen <- read_xlsx("data_funds.xlsx", sheet = "SocGen")
View(data_funds)
View(SocGen)

tickers <- c("MSCI Index", "SP500 index", "VTSAX.O", "SWPPX.O", "VFIAX.O", "FXIAX.O", "VTIAX.O","VBTLX.O","AGTHX.O","GFACX.O","VWNAX.O","FSPSX.O", "SocGenCTA", "SocGenCTATrend")
length(tickers)

# Set the dates for which we want to compare the returns
date.start <- as.Date("2012-12-31")
date.end   <- as.Date("2023-12-29")

library(PerformanceAnalytics)
#Calculate de returns 
returns <- CalculateReturns(data_funds) #compounded daily returns of all assets
returns <- returns[(-1),] # removal of the first row 
returns <-as.xts(returns)
# Plot monthly returns
plot.xts(returns, legend.loc = "topleft")
# Plot monthly closing prices 
plot.xts(as.xts(data_funds), legend.loc = "topleft" )
plot.xts(as.xts(data_funds),legend.loc = "bottomleft" , ylim=c(0,500))



#risk free based https://www.worldgovernmentbonds.com/bond-historical-data/germany/10-years/#title-historical-data, where we've taken the yield from 
risk_free1 <-(0.01486+0.01941)/2 #average risk free rate from 31 dec. 2012 till 31 dec. 2013
risk_free2 <- (0.01941+0.00541)/2#average risk free rate from 31 dec. 2013 till 31 dec. 2014
risk_free3<- (0.00541+0.00635)/2#average risk free rate from 31 dec. 2014 till 31 dec. 2015
risk_free4<- (0.00635+0.00207)/2#average risk free rate from 31 dec. 2015 till 31 dec. 2016
risk_free5 <- (0.00207+0.00426)/2#average risk free rate from 31 dec. 2016 till 31 dec. 2017
risk_free6 <- (0.00426+0.00246)/2#average risk free rate from 31 dec. 2017 till 31 dec. 2018
risk_free7<- (0.00246-0.00188)/2#average risk free rate from 31 dec. 2018 till 31 dec. 2019
risk_free8<-(0.00188-0.00576)/2#average risk free rate from 31 dec. 2019 till 31 dec. 2020
risk_free9<-(0.00576-0.00182)/2#average risk free rate from 31 dec. 2020 till 31 dec. 2021
risk_free10<-(0.00182+0.02565)/2#average risk free rate from 31 dec. 2021 till 31 dec. 2022
risk_free11 <- (0.02565+0.02031)/2#average risk free rate from 31 dec. 2022 till 31 dec. 2023

risk_free<-sum(risk_free1,risk_free2,risk_free3,risk_free4,risk_free5,risk_free6,risk_free7,risk_free8, risk_free9,risk_free10, risk_free11) /11 #average risk free rate from 2012-12-31 till 2023-12-31
#Market timing abilities

TM <-MarketTiming(as.xts(returns),as.xts(returns)[,2],risk_free, method = "TM")
View(TM)
HM <-MarketTiming(as.xts(returns),as.xts(returns)[,2],risk_free, method = "HM")
View(HM)
