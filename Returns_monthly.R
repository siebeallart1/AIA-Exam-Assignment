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

returns <- CalculateReturns(data_funds) 

# Plot monthly closing prices ( werkt nog niet)
library(ggplot2)
ggplot(data_funds, x = data_funds$Timestamp, y= c(data_funds$`MSCI Index`, data_funds$`SP500 Index`, data_funds$VTSAX.O,data_funds$SWPPX.O, data_funds$VFIAX.O, data_funds$FXAIX.O,data_funds$VTIAX.O, data_funds$VBTLX.O,data_funds$AGTHX.O, data_funds$GFACX.O,data_funds$VWNAX.O,data_funds$FSPSX.O, data_funds$SocGenCTA,data_funds$SocGenCTATrend)) + geom_line()