tickers <- c("MSCI Index", "SP500 index", "VTSAX.O", "SWPPX.O", "VFIAX.O", "FXIAX.O", "VTIAX.O","VBTLX.O","AGTHX.O","GFACX.O","VWNAX.O","FSPSX.O", "SocGenCTA", "SocGenCTATrend")
length(tickers)

# VUG: US Equity - Large Growth - Vanguard Growth Index Fund ETF 
# VTV: US Equity - Large Value - Vanguard Value Index Fund ETF 
# IWM: US Equity - Small - iShares Russell 2000 ETF 
# EFA: International Equity - Developed Markets - iShares MSCI EAFE ETF 
# EEM: International Equity - Emerging Markets - iShares MSCI Emerging Markets ETF 
# IEF: US Government Bonds - iShares 7-10 Year Treasury Bond ETF 
# IGOV: International Government Bonds - iShares International Treasury Bond ETF
# LQD: US/International Corporate Bonds - iShares iBoxx $ Invmt Grade Corp Bd ETF 
# VNQ: US Real Estate - Vanguard Real Estate Index Fund ETF Shares
# IXC: Global Energy - iShares Global Energy ETF
# ICLN: Global Energy - iShares Global Clean Energy ETF
# IAU: Gold - iShares Gold Trust

# Set the dates for which we want to compare the returns
date.start <- as.Date("2012-12-31")
date.end   <- as.Date("2023-12-29")

# Loop over tickers and download data

library("xts")

returns <- CalculateReturns(data_funds) 

# Plot monthly closing prices
library(ggplot2)
ggplot(data_funds, x = data_funds$Timestamp, y= c(data_funds$`MSCI Index`, data_funds$`SP500 Index`, data_funds$VTSAX.O,data_funds$SWPPX.O, data_funds$VFIAX.O, data_funds$FXAIX.O,data_funds$VTIAX.O, data_funds$VBTLX.O,data_funds$AGTHX.O, data_funds$GFACX.O,data_funds$VWNAX.O,data_funds$FSPSX.O, data_funds$SocGenCTA,data_funds$SocGenCTATrend)) + geom_line()
