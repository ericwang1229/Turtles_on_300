## Turtle strategy for ShangHai and ShenZhen 300 stocks.
## using adjusted prices which is the relative value with CSI300
## Last update Oct 15th, 2014

from __future__ import division
import csv
from collections import OrderedDict
from datetime import *
import matplotlib.pyplot as plt
    
class backtester():
    def __init__(self, filepaths, init_date = datetime(2009,1,1), end_date = datetime(2014, 9, 30)):
        self.equity = 0
        self.cash = 0
        self.history = OrderedDict()
        self.positions = {}
        self.bars = {}
        self.init_date = init_date
        self.end_date = end_date
        for symbol in filepaths:
            print(symbol)
            self.bars[symbol] = self.load_data(filepaths[symbol])
            reader = csv.reader(open(filepaths[symbol], 'rb'))
            print(symbol + ": " + str(len(self.bars[symbol])))
            self.positions[symbol] = {"Position":0, "AvgCost":0, "LastPrice":0}

    def load_data(self, path, filter_zero_vol = True, init_date=None, end_date=None):
        if init_date == None:
            init_date = self.init_date
        if end_date == None:
            end_date = self.end_date
        reader = csv.reader(open(path, 'rb'))
        return_values = []
        headers = reader.next()
        for title in headers:
            if "Date" in title:
                date_index = headers.index(title)
            elif "Open" in title:
                open_index = headers.index(title)
            elif "High" in title:
                high_index = headers.index(title)
            elif "Low" in title:
                low_index = headers.index(title)
            elif "Close" in title:
                close_index = headers.index(title)
            elif "Volume" in title:
                volume_index = headers.index(title)
        for row in reader:
            tempdate = datetime.strptime(row[date_index], '%Y-%m-%d')
            if (( not filter_zero_vol or int(float(row[volume_index])) > 0)
                and tempdate >= init_date
                and tempdate <= end_date):
                return_values.append({"D":tempdate,
                                      "O":float(row[open_index]),
                                      "H":float(row[high_index]),
                                      "L":float(row[low_index]),
                                      "C":float(row[close_index]),
                                      "V":int(float(row[volume_index]))})
        return return_values
            
    def run(self, strategy, init_equity = 1000000, init_date = None, end_date = None):
        self.equity = init_equity
        self.cash = self.equity
        self.best = self.equity
        self.max_drawdown = 0
        if init_date == None:
            init_date = self.init_date
        if end_date == None:
            end_date = self.end_date
        current_date = init_date
        symbol_indices = {}
        for symbol in self.bars:
            symbol_indices[symbol] = 0
        while(current_date <= end_date):
            for symbol in self.bars:
                if (symbol_indices[symbol] < len(self.bars[symbol])
                    and current_date == self.bars[symbol][symbol_indices[symbol]]["D"]):
                    new_position = strategy(current_date,
                                            symbol,
                                            symbol_indices[symbol],
                                            self.equity,
                                            self.cash,
                                            self.positions,
                                            self.bars,
                                            self)
                    self.trade(new_position, current_date)
                    self.positions[symbol]["LastPrice"] = self.bars[symbol][symbol_indices[symbol]]["C"]
                    symbol_indices[symbol] += 1
            if self.equity > self.best:
                self.best = self.equity
            if self.max_drawdown < 1 - self.equity/self.best:
                self.max_drawdown = 1 - self.equity/self.best
            self.history[current_date] = {"date":current_date,
                                          "equity":self.equity,
                                          "cash":self.cash,
                                          "positions":self.positions,
                                          "drawdown":1-self.equity/self.best}
            print(current_date.__str__()+", equity = "+self.equity.__str__()+", cash = "+self.cash.__str__())
            current_date = current_date + timedelta(days=1)
            
    def trade(self, position, date):
        equity = 0
        for action in position:
            action["Position"] = round(action["Position"])
            action["Price"] = round(action["Price"],3)
            print(date.__str__() + " "
                  + action["Symbol"] + ": "
                  + action["Position"].__str__() + "@"
                  + action["Price"].__str__())
            symbol = action["Symbol"]
            self.cash -= action["Position"]*action["Price"]
            self.cash -= abs(action["Position"]*action["Price"]) * 3/10000
            if self.positions[symbol]["Position"] + action["Position"] == 0:
                self.positions[symbol]["Position"] = 0
                self.positions[symbol]["AvgCost"] = 0
            else:
                self.positions[symbol]["AvgCost"] = \
                                                  (self.positions[symbol]["AvgCost"]*self.positions[symbol]["Position"] +\
                                                  action["Position"]*action["Price"])/(self.positions[symbol]["Position"] + action["Position"])
                self.positions[symbol]["Position"] += action["Position"]
        for symbol in self.positions:
            equity += self.positions[symbol]["Position"] * self.positions[symbol]["LastPrice"]
        self.equity = self.cash + equity
##        print(date.__str__()+", cash = "+self.cash.__str__()+", equity = "+self.equity.__str__())

class turtle():
    def __init__(self,
                 donchian_exit_period = 10,
                 donchian_short_period = 20,
                 donchian_long_period = 55,
                 turtle_atr_period = 20,
                 turtle_risk_ratio = 0.01):
        self.exit_period = donchian_exit_period
        self.short_period = donchian_short_period
        self.long_period = donchian_long_period
        self.atr_period = turtle_atr_period
        self.risk_ratio = turtle_risk_ratio
        self.pre_entry_success = {}
        self.pre_entry_price = {}
        self.entry_number = {}
        
    def turtle(self, date, symbol, index, equity, cash, positions, data, backtester):
##        print(symbol + " @ " + date.__str__())
        actions = []
        if index < 56:
            return actions
        day_high = data[symbol][index]["H"]
        day_low = data[symbol][index]["L"]
        day_open = data[symbol][index]["O"]
        day_highr = data[symbol][index]["HH"]
        day_lowr = data[symbol][index]["LL"]
        day_openr = data[symbol][index]["OO"]
        day_max55 = data[symbol][index]["Max55"]
        day_max20 = data[symbol][index]["Max20"]
        day_min10 = data[symbol][index]["Min10"]
        day_max55r = data[symbol][index]["Max55r"]
        day_max20r = data[symbol][index]["Max20r"]
        day_min10r = data[symbol][index]["Min10r"]
        day_atr = data[symbol][index]["ATR"]
        turtle_size_1 = self.risk_ratio*cash/day_atr
        turtle_size_2 = cash/5/day_open
        turtle_size = round(min(turtle_size_1, turtle_size_2)/100)*100
## Turtle size modification based on current drawdown
## For example, if current drawdown = 10%, then the turtle size will be 90% of original normal size.
##        turtle_size = round(turtle_size * (backtester.equity/backtester.best)**2/100)*100
        if (day_high == day_low
            and day_high > data[symbol][index-1]["C"] * 1.098):
            return actions
        if (day_highr >= day_max55r
            and positions[symbol]["Position"] == 0
            and turtle_size >= 100):
            price = day_max55 if day_open < day_max55 else day_open
            actions.append({"Symbol":symbol,
                            "Price": price,
                            "Position":turtle_size})
            self.entry_number[symbol] = 1
            self.pre_entry_price[symbol] = price
        elif (day_highr >= day_max20r
              and symbol in self.pre_entry_success
              and self.pre_entry_success[symbol]
              and positions[symbol]["Position"] == 0
              and turtle_size >= 100):
            price = day_max20 if day_open < day_max20 else day_open
            actions.append({"Symbol":symbol,
                            "Price":price,
                            "Position":turtle_size})
            self.entry_number[symbol] = 1
            self.pre_entry_price[symbol] = price
        elif (symbol in self.pre_entry_price
              and day_high > self.pre_entry_price[symbol] + 0.5*day_atr
              and turtle_size >= 100):
            price = self.pre_entry_price[symbol] + 0.5*day_atr
            while (price < day_high
                   and self.entry_number[symbol] < 4
                   and turtle_size >= 100):
                actions.append({"Symbol":symbol,
                                "Price":price if day_open < price else day_open,
                                "Position":turtle_size})
                cash -= price * turtle_size
                turtle_size = turtle_size if turtle_size < self.risk_ratio*cash/day_atr else round(self.risk_ratio*cash/day_atr/100)*100
                self.entry_number[symbol] += 1
                self.pre_entry_price[symbol] = price
                price += 0.5*day_atr
        elif (symbol in positions
              and symbol in self.pre_entry_price
              and positions[symbol]["AvgCost"] > day_low + 2*day_atr):
            actions.append({"Symbol":symbol,
                            "Price": day_low + 2*day_atr,
                            "Position": -positions[symbol]["Position"]})
            self.entry_number[symbol] = 0
            del self.pre_entry_price[symbol]
            self.pre_entry_success[symbol] = False
        elif (symbol in positions and symbol in self.pre_entry_price
              and day_low < day_min10):
            actions.append({"Symbol":symbol,
                            "Price": day_min10,
                            "Position": -positions[symbol]["Position"]})
            self.entry_number[symbol] = 0
            del self.pre_entry_price[symbol]
            if (positions[symbol]["AvgCost"] < day_min10):
                self.pre_entry_success[symbol] = True
            else:
                self.pre_entry_success[symbol] = False
        return actions
    
    def prepare_indicators(self, data, backtester):
        self.csi300 = backtester.load_data(r"C:\New folder\s\histories\000300.ss.adjusted.csv", False)
        csi300 = self.csi300
        for symbol in data:
            csi300_index = 0
            for index in range(0, len(data[symbol])):
                date = data[symbol][index]['D']
                while(csi300_index < len(csi300)):
                    if csi300[csi300_index]['D'] == data[symbol][index]['D']:
                        break
                    else:
                        csi300_index += 1
                if csi300_index > 1:
                    data[symbol][index]['OO'] = data[symbol][index]['O']/csi300[csi300_index-1]['C']*2000
                    data[symbol][index]['HH'] = data[symbol][index]['H']/csi300[csi300_index-1]['C']*2000
                    data[symbol][index]['LL'] = data[symbol][index]['L']/csi300[csi300_index-1]['C']*2000
                    data[symbol][index]['CC'] = data[symbol][index]['C']/csi300[csi300_index-1]['C']*2000
                else:
                    data[symbol][index]['OO'] = data[symbol][index]['O']/csi300[csi300_index]['C']*2000
                    data[symbol][index]['HH'] = data[symbol][index]['H']/csi300[csi300_index]['C']*2000
                    data[symbol][index]['LL'] = data[symbol][index]['L']/csi300[csi300_index]['C']*2000
                    data[symbol][index]['CC'] = data[symbol][index]['C']/csi300[csi300_index]['C']*2000
                    
                if index >= self.exit_period:
                    data[symbol][index]['Min10r'] = min([x["LL"] for x in data[symbol][(index - self.exit_period):index]])
                else:
                    data[symbol][index]['Min10r'] = None
                if index >= self.short_period:
                    data[symbol][index]['Max20r'] = max([x["HH"] for x in data[symbol][(index - self.short_period):index]])
                else:
                    data[symbol][index]['Max20r'] = None
                if index >= self.long_period:
                    data[symbol][index]['Max55r'] = max([x["HH"] for x in data[symbol][(index - self.long_period):index]])
                else:
                    data[symbol][index]['Max55r'] = None
                        
                if index >= self.exit_period:
                    data[symbol][index]['Min10'] = min([x["L"] for x in data[symbol][(index - self.exit_period):index]])
                else:
                    data[symbol][index]['Min10'] = None
                if index >= self.short_period:
                    data[symbol][index]['Max20'] = max([x["H"] for x in data[symbol][(index - self.short_period):index]])
                else:
                    data[symbol][index]['Max20'] = None
                if index >= self.long_period:
                    data[symbol][index]['Max55'] = max([x["H"] for x in data[symbol][(index - self.long_period):index]])
                else:
                    data[symbol][index]['Max55'] = None
                if index >= self.atr_period:
                    temp = []
                    for i in range(index-self.atr_period, index-1):
                        temp.append(max(data[symbol][i]["H"] - data[symbol][i]["L"],
                                        abs(data[symbol][i]["H"] - data[symbol][i]["O"]),
                                        abs(data[symbol][i]["L"] - data[symbol][i]["O"])))
                    data[symbol][index]['ATR'] = sum(temp)/len(temp)
                else:
                    data[symbol][index]['ATR'] = None

filepath = {"000001.SZ":r"C:\New folder\s\histories\000001.sz.adjusted.csv",
            "000858.SZ":r"C:\New folder\s\histories\000858.sz.adjusted.csv"}
##filepath = {}
##codes = csv.reader(open(r"C:\New folder\s\histories\000300cons.csv",'rb'))
##for code in codes:
##    filepath[code[0]] = "C:\\New folder\\s\\histories\\" + code[0] + ".adjusted.csv"
b = backtester(filepath)
t = turtle()
t.prepare_indicators(b.bars, b)
b.run(t.turtle)
equity_history = [(date, record["equity"], record["drawdown"]) for date, record in b.history.items()]
dates, values, drawdowns = zip(*equity_history)
fig, ax1 = plt.subplots()
ax1.plot(dates, values, 'b-')
ax2 = ax1.twinx()
ax2.plot(dates, drawdowns, 'r-')
plt.show()
    
                
        
            
                  
                  
            
        
            

    
        
    

        
    
