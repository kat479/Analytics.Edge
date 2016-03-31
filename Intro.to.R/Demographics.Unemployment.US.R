#Analytics Edge
#June 13, 2015

setwd('D:\\Downloads\\The Analytics Edge\\Week 1')
library(ggplot2)

#DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES

CPS <- read.csv('CPSData.csv',sep = ',',header = T,stringsAsFactors = F)

is.na(CPS)

sort(table(CPS$Industry),decreasing = T)

#Which state has the fewest interviewees?
sort(table(CPS$State),decreasing =T)

table(CPS$Citizenship == 'Citizen, Native')
table(CPS$Citizenship)/nrow(CPS)

table(CPS[which(CPS$Hispanic == 1),9])

#Checking the NULLS in all columns 
sapply(CPS, function(x)any(is.na(x)))

#PROBLEM 2.2 - EVALUATING MISSING VALUES

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$State,is.na(CPS$MetroAreaCode))

t <- as.data.frame(table(CPS$Region,is.na(CPS$MetroAreaCode)))
s <- as.data.frame(table(CPS$Region))
r <- merge( t,s, by = 'Var1')
r$prop <- r$Freq.x /r$Freq.y

q <- tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)
q
p <- q[which(q %in% 0:1 )]

p


q <- tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)
q -0.3
p <- q[-which(q-1 ==0 )]

which.max(p)

#PROBLEM 3.1 - INTEGRATING METROPOLITAN AREA DATA  

MetroAreaMap <- read.csv('MetroAreaCodes.csv',sep = ',',header = T, stringsAsFactors = F)
CountryMap <-  read.csv('CountryCodes.csv',sep = ',',header = T, stringsAsFactors = F)

#PROBLEM 3.2 - INTEGRATING METROPOLITAN AREA DATA  

CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)



table(is.na(CPS$MetroArea))
head(sort(table((CPS$MetroArea)),decreasing = T),n = 20L)

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? 


head(sort(tapply(CPS$Hispanic,CPS$MetroArea,mean),decreasing = T))

#Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector 
#of whether an interviewee is Asian, determine the number of metropolitan areas
#in the United States from which at least 20% of interviewees are Asian.

which(tapply((CPS$Race == 'Asian'),CPS$MetroArea,mean) >= 0.2)

#Passing na.rm=TRUE to the tapply function, determine which metropolitan area 
#has the smallest proportion of interviewees who have received no high school diploma.

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm = T))

#PROBLEM 4.1 - INTEGRATING COUNTRY OF BIRTH DATA

#What is the name of the variable added to the CPS data frame by this merge operation?

CPS <- merge(CPS,CountryMap , by.x = 'CountryOfBirthCode',by.y = 'Code',all.x = T)

#How many interviewees have a missing value for the new country of birth variable?

table(is.na(CPS$Country))

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area 
#have a country of birth that is not the United States?
v <- CPS[which(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA"),]
w <- tapply( v$Country == 'United States' ,v$MetroArea,mean,na.rm = T)
w
1-w
#-----

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
  

#Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?

table(CPS$Country == 'India',CPS$MetroArea)
sort(tapply(CPS$Country == 'India',CPS$MetroArea,mean,na.rm = T),decreasing = T)
sort(tapply(CPS$Country == 'Brazil',CPS$MetroArea,mean,na.rm = T),decreasing = T)
sort(tapply(CPS$Country == 'Somalia',CPS$MetroArea,mean,na.rm = T),decreasing = T)
