require("quantmod")
require("blotter")
# require("dtw")

setwd("C:/New folder/s")
initDate = "2009-01-01"
initEq = 100000

stock_300 <- read.csv("000300cons.csv", colClasses = c(rep("factor", 1)))
codes <- as.vector(stock_300$code)
# code_names <- paste("X", toupper(codes), sep ="")

histories <- new.env()
getSymbols(codes[1:3], from = initDate, env = histories)
# code <- ""
# for (i in 1:10)
# {
#   code <- stock_300$code[i]
#   histories[[i]] <- getSymbols(code, from = "2008-01-01", to = Sys.Date(), src = "yahoo", auto.assign = FALSE)
# }
# getHistory <- function(x){return(getSymbols(stock_300$code[[x]], from = "2008-01-01", to = Sys.Date(), src = "yahoo", auto.assign = FALSE))}
# histories <- lapply(1:10, getHistory)

currency("RMB")
stock(code_names, currency = "RMB", multiplier = 1)
# head(histories[[toupper(codes[[3]])]])

updateStrat <- function(Portfolio, Symbol, TxnDate, PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
{ 
#   http://www.programtrader.net/a/turtle_system/2012/1123/975.html
  pname=Portfolio
  NewTxn = xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.POSIXct(TxnDate))
  colnames(NewTxn) = c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
  Portfolio<-getPortfolio(Portfolio)
  Portfolio[[Symbol]]$strat <- rbind(Portfolio[[Symbol]]$strat, NewTxn)
  assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}

portfolio = "turtles"
initPortf(name = portfolio, codes, initDate = initDate)
account = "turtles"
initAcct(name=account,portfolios="turtles", initDate=initDate, initEq=initEq)