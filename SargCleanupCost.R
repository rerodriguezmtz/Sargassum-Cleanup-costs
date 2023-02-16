# Code used for manuscript "Pelagic Sargassum cleanup cost in Mexico"
#By Rodríguez-Martínez RE, Torres-Conde EG, and Jordán-Dahlgren E.

rm(list=ls(all=TRUE))
Sys.setenv(TZ='GMT')
setwd("D:/GitHub/SargCleanupCost")
Vol<-read.csv("SarVolPM.csv")

#Load libraries
library(tidyr)
library(plyr)# para hacer tablas con ddply
library(ggpubr)

str(Vol)

# Transform data
Vol$Year = factor(Vol$Year,levels = c("2018", "2019", "2021", "2022"))
Vol$Month = as.factor(Vol$Month)
Vol$YearMonth = as.factor(Vol$YearMonth)
Vol$Date = as.Date(Vol$Date)

#Reshape
require(reshape2)
Vol2<-melt(Vol, id = c("Year", "Month", "YearMonth", "Date"))
str (Vol2)
summary (Vol2)

#Remove NA
Vol2<-na.omit(Vol2)

## Summary statistics
VolMonth <- ddply(Vol2, .(Month), summarise, 
                 N    = length(value),
                 tot    = sum(value),
                 mean = round(mean (value), 1),
                 sd   = round (sd(value),1),
                 se   = sd(value)/sqrt(length(value)))

VolMonth

write.table(VolMonth, "VolMean.csv", quote=F, sep = ",", row.names=F)

#Calculate mean per-unit cost for Puerto Morelos (US$)
+(41+27+28+19+20)/5 #(mean = 27)

## Add column with cost (US$)
Vol2$Cost<-(Vol2$value*27)
Vol2$Cost

#Calculate yearly cost (US$)
VolYearCost <- ddply(Vol2, .(Year), summarise, 
                      N    = sum(Cost),
                      mean = round(mean (Cost), 1),
                      sd   = round (sd(Cost),1),
                      se   = sd(Cost)/sqrt(length(Cost)))

VolYearCost

#Calculate monthly cost (US$)
VolMonthCost <- ddply(Vol2, .(Month), summarise, 
                          N    = length(Cost),
                          mean = round(mean (Cost), 1),
                          sd   = round (sd(Cost),1),
                          se   = sd(Cost)/sqrt(length(Cost)))

VolMonthCost

write.table(VolMonthCost, "CostMean.csv", quote=F, sep = ",", row.names=F)

# Plot monthly Sargassum landing volumes (using data from 14 hotels for 2018-2022; mean and SE)
ggbarplot(Vol2, x = "Month", y = "value", color = "Month", fill = "Month", 
          add = "mean_ci", size=1, ylim = c(0, 6000))+ 
  xlab("Month")+
  ylab(expression(paste("Volume of Sargassum ", " ("*~ m^3 ~Km^-1*")")))+
  theme_grey(base_size=16) + theme(axis.text.x = element_text(angle=0), legend.position = "none")

