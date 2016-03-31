#Analytics Edge
#June 6, 2015

setwd('D:\\Downloads\\The Analytics Edge\\Week 1')
library(ggplot2)

#Crime Chicago

crime.ch <- read.csv('mvtWeek1.csv',sep = ',' , header = T, stringsAsFactors = F)

#EDA
nrow(crime.ch)
ncol(crime.ch)
str(crime.ch)
summary(crime.ch)
max(crime.ch$ID)
min(crime.ch$Beat)
summary(crime.ch$Arrest)
tapply(crime.ch$ID,crime.ch$LocationDescription == 'ALLEY',length)

#Converting Date format

DateConvert <- as.Date(strptime(crime.ch$Date,'%m/%d/%y %H:%M'))
median(DateConvert)

qplot(DateConvert,binwidth = 25)

crime.ch$Date <- DateConvert

crime.ch$Month <- months(crime.ch$Date)
crime.ch$Weekday <- weekdays(crime.ch$Date)

#In Which month has the fewest motor vehicle thefts occur?

t <- as.data.frame(tapply(crime.ch$ID , crime.ch$Month,length))
t <- which(tapply(crime.ch$ID , crime.ch$Month,length) == min(tapply(crime.ch$ID , crime.ch$Month,length)), arr.ind = T)

#In Which month has the fewest motor vehicle thefts occur?
t <- which(tapply(crime.ch$ID , crime.ch$Weekday,length) == max(tapply(crime.ch$ID , crime.ch$Weekday,length)), arr.ind = T)

#Which month has the largest number of motor vehicle thefts for which an arrest was made?

t <- tapply(crime.ch$ID, crime.ch$Month,length)

s <- crime.ch[which(crime.ch$Arrest == 'TRUE'),c(1,12)]
s <- tapply(crime.ch[which(crime.ch$Arrest == 'TRUE'),1], crime.ch[which(crime.ch$Arrest == 'TRUE'),12],length)

r <- which(s == max(s),arr.ind = T)

#Plots

qplot(crime.ch$Date , geom = 'Histogram',binwidth = 100 )
hist(crime.ch$Date , breaks = 100)

#Creating a boxplot of the variable "Date", sorted by the variable "Arrest" 

qplot(crime.ch$Arrest,crime.ch$Date, xlab = 'Arrest',ylab = 'Date',main = 'Spread of Arrests along Date',geom = 'boxplot')

#For what proportion of motor vehicle thefts in 2001 was an arrest made?

yearconvert <- format(crime.ch$Date, '%Y')
crime.ch$Year <- yearconvert


crime.ch.2001 <- crime.ch[which(crime.ch$Year == '2001'),c(1,4,9)]

overall.sum.rows <- nrow(crime.ch.2001)
proportion.arrest.sum <- tapply(crime.ch.2001$ID , crime.ch.2001$Arrest , length)
proportation.arrest.prop <- proportion.arrest.sum/overall.sum.rows
proportation.arrest.prop 

#Plotting the trend of arrests across years

overall.sum <- as.data.frame(tapply(crime.ch$ID , crime.ch$Year, length))
overall.sum$Year <- row.names(overall.sum)
names(overall.sum)[1] <- 'Overall Sum'

proportion <- aggregate(ID ~ Arrest+Year,crime.ch,length)

prop.overall.Year <- merge(proportion,overall.sum, by = 'Year')

prop.overall.Year$prop <- prop.overall.Year$ID/prop.overall.Year$'Overall Sum'

qplot(Year,prop,data =prop.overall.Year,colour = Arrest,geom = 'line',binwidth = 1 )

#Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?

sorted.Location.Desc <- sort(table(crime.ch$LocationDescription),decreasing = T)
t <- as.data.frame(sorted.Location.Desc[1:6])
t$LocationDescription <- row.names(t)
names(t)[1]<- 'Theft'
t <- t[-which(t$LocationDescription == 'OTHER'),]
class(t$Theft)
sum(t$Theft)

#Subsetting the data for the five locations

top5 <- crime.ch[crime.ch$LocationDescription %in% t$LocationDescription,]

agg.top5 <- aggregate(ID~Arrest+LocationDescription,top5,length)
agg.top5.overall <- aggregate(ID~LocationDescription,top5,length)

agg.top5.arrestprop <- merge(agg.top5,agg.top5.overall,by='LocationDescription')
agg.top5.arrestprop$prop <- agg.top5.arrestprop$ID.x/agg.top5.arrestprop$ID.y

ok <- agg.top5.arrestprop[which(agg.top5.arrestprop$Arrest == 'TRUE'),]

#On which day of the week do the most motor vehicle thefts at gas stations happen?
tapply(top5[top5$LocationDescription == 'GAS STATION',1],top5[top5$LocationDescription == 'GAS STATION',13],length)
tapply(top5[top5$LocationDescription == 'DRIVEWAY - RESIDENTIAL',1],top5[top5$LocationDescription == 'DRIVEWAY - RESIDENTIAL',13],length)
