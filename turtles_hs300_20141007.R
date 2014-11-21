require(quantmod)
require(TTR)
require(blotter)
setwd("C:\\New folder\\s\\histories")
Sys.setenv(TZ="UTC")

try(rm("account.turtles","portfolio.turtles",pos=.blotter),silent=TRUE)
try(rm("portfolio","account","N","symbol","symbols","ClosePrice","CurrentDate","equity","Units","maxUnits","size","Stop","equity","TxnPrice","initDate","initEq","Posn","verbose"),silent=TRUE)

initDate = "2009-01-01"
endDate = "2014-10-01"
initEq = 100000
print("Initializing portfolio and account structure")
# Assemble a small portfolio of three stocks
stock_300 <- read.csv("C:/New folder/s/histories/000300cons.csv", colClasses = c(rep("factor", 1)))
symbols <- as.vector(stock_300$code)
symbols <- paste(rep("X",300), symbols, sep = "")
symbols <- toupper(symbols)
# symbols <- c("X601018.SS")
# symbols = c("XLF", "XLP", "XLE")#, "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
currency("RMB")
for(symbol in symbols)
  {
    stock(symbol, currency="RMB", multiplier=1)
  }

#set function for storing intermediate values
updateStrat <- function(Portfolio, Symbol, TxnDate, PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
{ # @author Peter Carl
	
	# DESCRIPTION:
	# Adds transactions-related data to the STRATEGY timeseries.
	
	# Inputs
	# TxnDate: transaction date as ISO 8106, e.g., '2008-09-01'
	# PosUnitsQty: total units (shares) of the transaction
	# StopPrice: price at which the transaction was done
	# TxnPrice: last trade price
	# TxnN: calculated N for last transaction
	
	# Outputs:
	# No output.  Modifies STRATEGY in local namespace.
	
	# FUNCTION
	# Store the transaction and calculations
	# Called for its side-effects of updating the 'strat' table in the portfolio
	NewTxn <- xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by = as.POSIXct(TxnDate), dimnames = list(NULL, c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')))
	# .getPortfolio returns the portfolio environment, which allows you to write to it, since
	# environments are pass-by-reference.
	# NOTE: To be safe, use getPortfolio for a read-only copy of the portfolio. getPortfolio copies
	# the portfolio environment to a list.
	Portfolio <- .getPortfolio(Portfolio)
	# This table stores transaction-related information relative to the strategy
	Portfolio$symbols[[Symbol]]$strat <- rbind(Portfolio$symbols[[Symbol]]$strat, NewTxn)
}

# getSymbols(symbols, index.class="POSIXct", from=initDate, src="yahoo")
for (symbol in symbols)
  {
    .GlobalEnv[[symbol]] <- as.xts(read.zoo(tolower(paste(substring(symbol, 2), ".adjusted.csv", sep="")), header = TRUE, sep = ","))
#     if (initDate < min(index(.GlobalEnv[[symbol]])))
#     {
# #       firstOpen <- .GlobalEnv[[symbol]][[1]]
#       days <- timeBasedSeq(paste(initDate, min(index(.GlobalEnv[[symbol]])), sep="/"))
#       weekdays <- subset(days, !weekdays(days) %in% c("Saturday", "Sunday"))
#       .GlobalEnv[[symbol]] <- merge(.GlobalEnv[[symbol]], weekdays, fill = NA)
#     }
#     reg.time <- timeBasedSeq(paste(initDate, endDate, sep="/"))
#     .GlobalEnv[[symbol]] <- merge(.GlobalEnv[[symbol]], reg.time, fill = NA)
    days <- timeBasedSeq(paste(initDate, endDate, sep = "/"))
    weekdays <- subset(days, !weekdays(days) %in% c("Saturday", "Sunday"))
    .GlobalEnv[[symbol]] <- merge(.GlobalEnv[[symbol]], weekdays, fill = 0)
    Open <-0
    High <- 0
    Low <- 0
    Close <- 0
    Adjusted <- 0
    for(i in 1:NROW(.GlobalEnv[[symbol]]))
    {
      
      if (( .GlobalEnv[[symbol]][i ,1] == 0)&&(Close !=0))
      {
#         print(paste(i,Close))
        .GlobalEnv[[symbol]][i, 1] <- Open
        .GlobalEnv[[symbol]][i, 2] <- High
        .GlobalEnv[[symbol]][i, 3] <- Low
        .GlobalEnv[[symbol]][i, 4] <- Close
        .GlobalEnv[[symbol]][i, 6] <- Adjusted
      }
      Open <-.GlobalEnv[[symbol]][i, 1]
      High <-.GlobalEnv[[symbol]][i, 2]
      Low <-.GlobalEnv[[symbol]][i, 3]
      Close <-.GlobalEnv[[symbol]][i, 4]
      Adjusted <-.GlobalEnv[[symbol]][i, 6]
    }
    
#     Open <- 0
#     High <- 0
#     Low <- 0
#     Close <- 0
#     Adjusted <- 0
#     for (day in weekdays)
#     {
#       if (length(.GlobalEnv[[symbol]][as.Date(day)]) == 0)
#       {
#         insertRow <- xts(Open, as.Date(day))
#         colnames(insertRow) = "Open"
#         insertRow$h = High
#         insertRow$l  = Low
#         insertRow$c = Close
#         insertRow$v = 0
#         insertRow$a = Adjusted
#         colnames(insertRow) = paste(symbol, c("Open", "High", "Low", "Close", "Volume", "Adjusted"), sep = ".")
# #         insertRow[, paste(symbol,".High",sep="")] = High
# #         insertRow[, paste(symbol,".Low",sep="")]  = Low
# #         insertRow[, paste(symbol,".Close",sep="")] = Close
# #         insertRow[, paste(symbol,".Volume",sep="")] = 0
# #         insertRow[, paste(symbol,".Adjusted",sep="")] = Adjusted
#         .GlobalEnv[[symbol]][as.Date(day)] = insertRow
# #         .GlobalEnv[[symbol]] <- merge(.GlobalEnv[[symbol]], insertRow)
# #         print(as.Date(day))
# #         .GlobalEnv[[symbol]][as.Date(day)][,1] <- Open
# #         .GlobalEnv[[symbol]][as.Date(day)][,2] <- High
# #         .GlobalEnv[[symbol]][as.Date(day)][,3] <- Low
# #         .GlobalEnv[[symbol]][as.Date(day)][,4] <- Close
# #         .GlobalEnv[[symbol]][as.Date(day)][,5] <- Adjusted
#       }
#       else
#       {
#         Open <- .GlobalEnv[[symbol]][as.Date(day)][,1]
#         High <- .GlobalEnv[[symbol]][as.Date(day)][,2]
#         Low <- .GlobalEnv[[symbol]][as.Date(day)][,3]
#         Close <- .GlobalEnv[[symbol]][as.Date(day)][,4]
#         Adjusted <- .GlobalEnv[[symbol]][as.Date(day)][,5]
#       }
#     }
    print(paste(symbol, length(.GlobalEnv[[symbol]])))
  }
# getSymbols now defaults (as originally) to "Date" indexing.  We can change to use POSIXct here.
# getSymbols(symbols, index.class=c("POSIXt","POSIXct"), from=initDate, source="yahoo")

# Set up a portfolio object and an account object
portfolio = "turtles" 
initPortf(name=portfolio, symbols, initDate=initDate)
account = "turtles"
initAcct(name=account,portfolios="turtles", initDate=initDate, initEq = initEq)

# @todo: decrease the size of the notional account by 20% each time lose 10% of original account (10% drawdown).  E.g., if trading a $1M account and down 100K, trade as if $800K account until out of drawdown.  If lose another 10% from 800K, or 80K loss, then reduce account size another 20% for notional size of 640K.

# Set up indicators
print("Setting up indicators")
for(symbol in symbols)
  {
    # System 1
    #
    # 20-day breakouts are ignored if the last breakout
    # would have resulted in a winning trade
    #
    # These values will also be used as System 2 exits
    x = get(symbol)
    # Entries (& System 2 exits)
    x$Min20 <- runMin(x[,grep('Low',colnames(x))], 20)
    x$Max20 <- runMax(x[,grep('High',colnames(x))],20)

    # Exits
    x$Min10 <- runMin(x[,grep('Low',colnames(x))], 10)
    x$Max10 <- runMax(x[,grep('High',colnames(x))],10)

    # System 2
    #
    # 55-day breakouts are always taken

    # Entries
    x$Min55 <- runMin(x[,grep('Low',colnames(x))], 55)
    x$Max55 <- runMax(x[,grep('High',colnames(x))],55)

    # Position Size Parameter c('High','Low','Close')
    x$N <- ATR(x[,c(2,3,4)], n=20, maType=EMA, wilder=TRUE)[,'atr']
    assign(symbol,x)
}
# Portfolio Parameters
size = 0.01
maxUnits = 4
Units=0
verbose=TRUE

# Create trades
for( i in 57:NROW(x) ) 
  { 
  # Assumes all dates are the same
    CurrentDate=time(x)[i]
    equity = getEndEq(account, CurrentDate)
    print(equity)
#     print (CurrentDate)
    for(symbol in symbols)
      {
        x = get(symbol)
        ClosePrice = as.numeric(Cl(x[i,]))

        Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
        s = tail(getPortfolio(portfolio)$symbols[[symbol]]$strat,1)

    	  Units = as.numeric(s[,'Pos.Units'])
        TxnPrice = as.numeric(s[,'Txn.Price'])
        N = as.numeric(s[,'Txn.N'])
        Stop = as.numeric(s[,'Stop.Price'])
    
#         print(paste(CurrentDate,", equity = ", equity, ", symbol = ", symbol, sep = "" ))
        UnitSize = as.numeric(trunc((size * equity)/(x[i-1,'N']*ClosePrice)))
        if ((!is.na(Lo(x[i-56,]))) && (!is.na(Vo(x[i,]))) &&
              (as.numeric(Lo(x[i-56,])) > 0) && 
              (as.numeric(Vo(x[i,])) > 100))
        {
          if( Posn == 0 && ( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max55']) )) 
            { 
              addTxn(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, TxnPrice = as.numeric(x[i-2,'Max55']), TxnQty = UnitSize , TxnFees = as.numeric(x[i-2,'Max55'])*UnitSize*-0.00003, verbose = verbose)
              N = as.numeric(x[i-1,'N'])
              updateStrat(Portfolio = portfolio, 
                          Symbol = symbol, 
                          TxnDate = CurrentDate, 
                          PosUnitsQty = 1, 
                          UnitSize = UnitSize, 
                          StopPrice = as.numeric(x[i-2,'Max55'])-2*N, 
                          TxnPrice = as.numeric(x[i-2,'Max55']), 
                          TxnN = N)
            } 
          else if( Posn > 0 && ( as.numeric(Lo(x[i-1,]))  <  as.numeric(x[i-2,'Min20'])  ) ) 
            {
  #             print(paste("Stoploss, Lo(x[i-1,] = ", as.character(Lo(x[i-1,])), ", Min20 = ", as.character(x[i-2,'Min20']), ", stop = ",as.character(Stop), sep = ""))
              addTxn(Portfolio = portfolio, 
                     Symbol = symbol, 
                     TxnDate = CurrentDate, 
                     TxnPrice = as.numeric(x[i-2,'Min20']), 
                     TxnQty = -Posn , 
                     TxnFees = 0, 
                     verbose = verbose)
              N = as.numeric(x[i-1,'N'])
              updateStrat(Portfolio = portfolio, 
                          Symbol = symbol, 
                          TxnDate = CurrentDate, 
                          PosUnitsQty = 0, 
                          UnitSize = UnitSize, 
                          StopPrice = NA, 
                          TxnPrice = as.numeric(x[i-2,'Min20']), 
                          TxnN = N)
            } 
          else if(  Posn > 0 && ( Lo(x[i-1,])  < Stop ))
            {
  #             print(paste("Stoploss, Lo(x[i-1,] = ", as.character(Lo(x[i-1,])), ", Min20 = ", as.character(x[i-2,'Min20']), ", stop = ",as.character(Stop), sep = ""))
              addTxn(Portfolio = portfolio, 
                     Symbol = symbol, 
                     TxnDate = CurrentDate, 
                     TxnPrice = Stop, 
                     TxnQty = -Posn , 
                     TxnFees = 0, 
                     verbose = verbose)
              N = as.numeric(x[i-1,'N'])
              updateStrat(Portfolio = portfolio, 
                          Symbol = symbol, 
                          TxnDate = CurrentDate, 
                          PosUnitsQty = 0, 
                          UnitSize = UnitSize, 
                          StopPrice = NA, 
                          TxnPrice = Stop, 
                          TxnN = N)
            }
          else if( Posn > 0  && Units < maxUnits && Hi(x[i-1,]) > ( TxnPrice + N * 0.5 ) ) 
            {
              addTxn(Portfolio = portfolio, 
                     Symbol = symbol, 
                     TxnDate = CurrentDate, 
                     TxnPrice = TxnPrice + N * 0.5, 
                     TxnQty = UnitSize , 
                     TxnFees = (TxnPrice + N * 0.5)*UnitSize*-0.00003, 
                     verbose = verbose)
              N = as.numeric(x[i-1,'N'])
              updateStrat(Portfolio = portfolio, 
                          Symbol = symbol, 
                          TxnDate = CurrentDate, 
                          PosUnitsQty = Units + 1, 
                          UnitSize = UnitSize, 
                          StopPrice = (TxnPrice + N * 0.5 - 2*N), 
                          TxnPrice = TxnPrice + N * 0.5, 
                          TxnN = N)
            } 
        }
    }
  # Now that we've updated all of our trades, its time to mark the book
  updatePortf(Portfolio = portfolio)
  updateAcct(account, Dates=CurrentDate)
  updateEndEq(account, Dates=CurrentDate)
} # End dates loop

# Final values
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')

if (require(quantmod)) {
	for(symbol in symbols){
		dev.new()
		chart.Posn(Portfolio='turtles',Symbol=symbol)
	}
}

if(require(PerformanceAnalytics)){
    return = Delt(getAccount(account)$summary$End.Eq)
	dev.new()
    charts.PerformanceSummary(as.zoo(return),main="Turtle Demo Performance")   
	dev.new()
	charts.PerformanceSummary(PortfReturns('turtles'),main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}

getEndEq(account,Sys.time())
