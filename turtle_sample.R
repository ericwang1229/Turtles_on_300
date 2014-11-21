require(quantmod)
require(TTR)
require(blotter)

try(rm("account.turtles","portfolio.turtles",pos=.blotter),silent=TRUE)
try(rm("portfolio","account","N",
       "symbol","symbols","ClosePrice","CurrentDate",
       "equity","Units","maxUnits","size","Stop","equity",
       "TxnPrice","initDate","initEq","Posn","verbose"),silent=TRUE)

initDate="2008-01-01"
initEq=100000
print("Initializing portfolio and account structure")

symbols = c("XLF", "XLP", "XLE")#, "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")
currency("USD")
for(symbol in symbols){
  stock(symbol, currency="USD",multiplier=1)
}

updateStrat <- function(Portfolio, Symbol, TxnDate,
                        PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
{
  pname=Portfolio
  NewTxn = xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.POSIXct(TxnDate))
  colnames(NewTxn) = c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
  Portfolio<-getPortfolio(Portfolio)
  Portfolio[[Symbol]]$strat <- rbind(Portfolio[[Symbol]]$strat, NewTxn)
  assign( paste("portfolio",pname,sep='.'), Portfolio, envir=.blotter )
}

getSymbols(symbols, index.class="POSIXct", from=initDate, source="yahoo")

portfolio = "turtles"
initPortf(name=portfolio,symbols, initDate=initDate)
account = "turtles"
initAcct(name=account,portfolios="turtles", initDate=initDate, initEq=initEq)

Portfolio<-getPortfolio(portfolio)
for(symbol in symbols){
  Portfolio[[symbol]]$strat <- xts( as.matrix(t(c(0,0,0,0,0))), order.by=as.POSIXct(initDate) )
  colnames(Portfolio[[symbol]]$strat) <- c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')
}

assign( "portfolio.turtles", Portfolio , envir=.blotter )
rm("Portfolio")

print("Setting up indicators")
for(symbol in symbols){
  
  x=get(symbol)
  
  x$Min20 <- runMin(x[,grep('Low',colnames(x))], 20)
  x$Max20 <- runMax(x[,grep('High',colnames(x))],20)
  
  x$Min10 <- runMin(x[,grep('Low',colnames(x))], 10)
  x$Max10 <- runMax(x[,grep('High',colnames(x))],10)
  
  x$Min55 <- runMin(x[,grep('Low',colnames(x))], 55)
  x$Max55 <- runMax(x[,grep('High',colnames(x))],55)
  
  x$N <- ATR(x[,c(2,3,4)], n=20, maType=EMA, wilder=TRUE)[,'atr']
  assign(symbol,x)
}

size = 0.01
maxUnits = 4
Units=0
verbose=TRUE

for( i in 57:NROW(x) ) { 
  CurrentDate=time(x)[i]
  #print(CurrentDate)
  equity = getEndEq(account, CurrentDate)
  
  for(symbol in symbols){
    x=get(symbol)
    ClosePrice = as.numeric(Cl(x[i,]))
    
    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    s = tail(getPortfolio(portfolio)[[symbol]]$strat,1)
    
    Units = as.numeric(s[,'Pos.Units'])
    TxnPrice = as.numeric(s[,'Txn.Price'])
    N = as.numeric(s[,'Txn.N'])
    Stop = as.numeric(s[,'Stop.Price'])
    
    UnitSize = as.numeric(trunc((size * equity)/(x[i-1,'N']*ClosePrice)))
    
    if( Posn == 0 ) {
      if( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max55']) ) {
        addTxn(Portfolio=portfolio, Symbol=symbol,
               TxnDate=CurrentDate, TxnPrice=ClosePrice,
               TxnQty = UnitSize , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio=portfolio, Symbol=symbol,
                    TxnDate = CurrentDate, PosUnitsQty = 1,
                    UnitSize = UnitSize, StopPrice = (ClosePrice-2*N),
                    TxnPrice = ClosePrice, TxnN = N)
      } else
        if( as.numeric(Lo(x[i-1,]))  < as.numeric(x[i-2,'Min55']) ) {
          addTxn(Portfolio=portfolio, Symbol=symbol,
                 TxnDate=CurrentDate, TxnPrice=ClosePrice,
                 TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
          N = as.numeric(x[i-1,'N'])
          updateStrat(Portfolio=portfolio, Symbol = symbol,
                      TxnDate = CurrentDate, PosUnitsQty = Units, UnitSize = UnitSize,
                      StopPrice = (ClosePrice +2*N), TxnPrice = ClosePrice, TxnN = N)
        }
    } else
      if( ( Posn > 0 && ( as.numeric(Lo(x[i-1,]))  <  as.numeric(x[i-2,'Min20']) || Lo(x[i-1,])  < Stop ) ) ||
            ( Posn < 0 && ( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max20']) || Hi(x[i-1,]) > Stop ) ) ) {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
               TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio = portfolio, Symbol = symbol,
                    TxnDate = CurrentDate, PosUnitsQty = 0, UnitSize = UnitSize,
                    StopPrice = NA, TxnPrice = ClosePrice, TxnN = N)
      } else
        if( Posn > 0  && Units < maxUnits && Hi(x[i-1,]) > ( TxnPrice + N * 0.5 ) ) {
          addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
                 TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
          N = as.numeric(x[i-1,'N'])
          updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate,
                      PosUnitsQty = Units+1, UnitSize = UnitSize,
                      StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
        } else
          if( Posn < 0 && Units < maxUnits && Lo(x[i-1,])  < ( TxnPrice - N * 0.5 ) ) {
            addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate,
                   TxnPrice=Cl(x[i,]), TxnQty = -UnitSize , TxnFees=0, verbose=verbose)
            N = as.numeric(x[i-1,'N'])
            updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate,
                        PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice+2*N),
                        TxnPrice = ClosePrice, TxnN = N)
          } #else
  }
  updatePortf(Portfolio = portfolio, Dates = CurrentDate)
  updateAcct(account, Dates = CurrentDate)
  updateEndEq(account, Dates = CurrentDate)
} 

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
  charts.PerformanceSummary(PortfReturns('turtles'),
                            main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}

getEndEq(account,Sys.time())