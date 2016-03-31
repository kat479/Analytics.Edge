#Analytics Edge
#June 8, 2015

setwd('D:\\Downloads\\The Analytics Edge\\Week 1')
library(ggplot2)

#Stock Dynamics
IBM <- read.csv('IBMStock.csv',sep = ',',header = T , stringsAsFactors = F)
GE <- read.csv('GEStock.csv',sep = ',',header = T , stringsAsFactors = F)
ProcterGamble <- read.csv('ProcterGambleStock.csv',sep = ',',header = T , stringsAsFactors = F)
CocaCola <- read.csv('CocaColaStock.csv',sep = ',',header = T , stringsAsFactors = F)
Boeing <- read.csv('BoeingStock.csv',sep = ',',header = T , stringsAsFactors = F)

str(GE$Date)

IBM$Date <- as.Date(IBM$Date , '%m/%d/%y')
GE$Date <- as.Date(GE$Date , '%m/%d/%y')
ProcterGamble$Date <- as.Date(ProcterGamble$Date , '%m/%d/%y')
CocaCola$Date <- as.Date(CocaCola$Date , '%m/%d/%y')
Boeing$Date <- as.Date(Boeing$Date , '%m/%d/%y')

#Summary Statistics

nrow(IBM)

min(IBM$Date)

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

#Plots

CocaCola$Year <- format(CocaCola$Date , '%Y')
qplot(Date,StockPrice,data = CocaCola, ,geom ='line')

qplot(Date,StockPrice,data = ProcterGamble , geom_line(color = blue))

p <- ggplot(NULL, aes(x=Date,y=StockPrice)) +geom_line(data = ProcterGamble , colour = 'blue')+geom_line(data = CocaCola , colour = 'red')

p + geom_vline(xintercept = as.numeric(as.Date('1983-01-01')))

#Visualising the trends

p <- ggplot(NULL, aes(x=Date,y=StockPrice))+geom_line(data = ProcterGamble , colour = 'blue')+geom_line(data = IBM , colour = 'yellow')+geom_line(data = CocaCola , colour = 'red')+geom_line(data = GE , colour = 'purple') + geom_line(data = Boeing , colour = 'black')

p + geom_vline(xintercept = as.numeric(as.Date('1997-10-01')))

p + coord_cartesian(xlim = c(as.Date('2003-11-01','%Y-%m-%d'), as.Date('2006-02-1','%Y-%m-%d')),ylim = c(0,150))

#Monthly Trends

IBM.month.mean <- as.data.frame(sort(tapply(IBM$StockPrice,months(IBM$Date),mean),decreasing = T))

IBM.month.mean$month <- row.names(IBM.month.mean)

names(IBM.month.mean)[1] <- 'month.mean'

above.mean <- function(x){
  x - mean(IBM$StockPrice)
}

IBM.above.mean <- data.frame(sort(tapply(IBM.month.mean$month.mean,IBM.month.mean$month,above.mean),decreasing = T))

GE.month.mean <- data.frame(sort(tapply(GE$StockPrice,months(GE$Date),mean),decreasing = T))
CocaCola.month.mean <- data.frame(sort(tapply(CocaCola$StockPrice,months(CocaCola$Date),mean),decreasing = T))
