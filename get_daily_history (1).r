require("quantmod")

setwd("C:/New folder/s/histories")
initDate = "2009-01-01"

stock_300 <- read.csv("000300cons.csv", colClasses = c(rep("factor", 1)))
codes <- as.vector(stock_300$code)

# getSymbols(codes, from = initDate)
for (code in codes[200:300])
{
  print(code)
#   code = "600008.ss"
  getSymbols(code, from = initDate)
  write.zoo(.GlobalEnv[[toupper(code)]], file = paste(code, ".csv", sep = ""), index.name = "Date", sep = ",")
  write.zoo(adjustOHLC(.GlobalEnv[[toupper(code)]], use.Adjusted = TRUE), file = paste(code, ".adjusted.csv", sep = ""), index.name = "Date", sep = ",")
}


