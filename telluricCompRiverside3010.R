library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(plotly)
library(tidyverse)


SDL <- function(file){
  
  setwd("C:/Users/dgallen/Desktop/Transition/Riverside/SDL")
  datDC <- read.csv(dir()[6])

  DC <- select(datDC,DC.Reading,Record.Type,colnames(datDC[2]), Record.Type) 
  
  Int <- paste(c(rep("OFF",10),rep("ON",30)))
  IntSec <- paste(c(rep(1:10),c(1:30)))
  
  day <- as.data.frame( seq(as.POSIXct("2017-10-26"),as.POSIXct("2017-11-2"),by=1))
  n <- dim(day)[1]
  day <- as.data.frame(day[1:(n-1),])
  day$DateTime <- day$`day[1:(n - 1), ]`
  day$Int <- rep_len(Int,length.out = n-1)
  day$IntSec <- as.numeric(rep_len(IntSec,length.out = n-1))
  day$Cycle <- paste(day$Int,day$IntSec,sep="_")
  
  DC$DateTime <- as.POSIXct(as.character(DC[[3]]), 
                            format = "%m-%d-%Y %H:%M:%OS", 
                            origin="1899-12-30",
                            tz="UTC")##UTC ##US/Central
  DC$DateTimeCDT <- with_tz(DC$DateTime,tzone = "US/Central")
  
  DC <- merge(day, DC, by.x = "DateTime") %>%
    filter(Record.Type == "DC Reading") %>%
    select(Record.Type,Int,IntSec,Cycle,DateTime,DateTimeCDT,DC.Reading)
  DC$Cycle <- as.factor(DC$Cycle)
  DC$Int <- as.factor(DC$Int)


  ###StartOnOff <- as.numeric(row.names( day[day$Time == DC[1,"DateTime"],]))
  ###EndOnOff <- StartOnOff + as.numeric(dim(DC)[1])-1
  ###OnOff <- day$Cycle[StartOnOff:EndOnOff]
  ###DC$Cycle <- OnOff
  
  DCplot <- filter(DC,(Cycle == "ON_30") | (Cycle== "OFF_10"))
  
  d <- filter(DC, DateTimeCDT > "2017-10-30 14:00:00",DateTimeCDT < "2017-10-30 15:00:00")
  
  g <- ggplot(DCplot[1:(dim(DCplot)[1]-1),], aes(DateTimeCDT, DC.Reading,color=Int))+
    geom_line()+
    labs(x="Time", y="Pipe to Soil Potential (V)")+
    scale_y_continuous(trans = 'reverse',breaks = c(seq(0,-2,-0.1)), limits = c( 0,-1.75))+
    theme(panel.grid.major = element_line(colour = "grey"), 
          panel.grid.minor = element_line(colour = "grey"),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))+
    geom_hline(aes(yintercept=-0.850),color="red")+
    scale_x_datetime(breaks = date_breaks("3 hour"), 
                     minor_breaks=date_breaks("1 hour"), 
                     labels=date_format("%m-%d %H:%M",tz = "US/Central"))+
    ggtitle(1)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90,vjust=0.25))
  plot(g)
  
  ggplotly(g)
  
}


DC7<-DC


  mean(filter(DC6,(Int=="OFF")&(IntSec==10))[,7])
mean <- mean(x)
         


meanSDL <- function(up,down,on,off){
meanupon <- mean(filter(up,(Int=="ON")&(IntSec>=on))[,7])

meanupoff <- mean(filter(up,(Int=="OFF")&(IntSec>=off))[,7])

meandownon <- mean(filter(down,(Int=="ON")&(IntSec>=on))[,7])

meandownoff <- mean(filter(down,(Int=="OFF")&(IntSec>=off))[,7])

dat <- as.data.frame(row.names = c("meanupon","meanupoff","meandownon","meandownoff"),
              c(meanupon,meanupoff,meandownon,meandownoff))
colnames(dat) <- "mean"
return(dat)
}

mean <- meanSDL(DC6,DC7,25,10)


setwd("C:/Users/dgallen/Desktop/Transition/Riverside")

CIS <- readxl::read_xlsx("CloseIntervalInspectionsSVYV.xlsx")

CIS$DateTime <- as.POSIXct(CIS$`Inspection Date/Time`, 
                         format = "%m/%d/%Y %I:%M:%OS %p", 
                         origin="1899-12-30",
                         tz="US/Central")

CIS$ONDateTime <- as.POSIXct(CIS$`CIS Structure P/S Reading Date/Time`, 
                          format = "%m/%d/%Y %I:%M:%OS %p", 
                          origin="1899-12-30",
                          tz="US/Central")

CIS$OFFDateTime <- as.POSIXct(CIS$`CIS Structure IRF Reading Date/Time`, 
                           format = "%m/%d/%Y %I:%M:%OS %p", 
                           origin="1899-12-30",
                           tz="US/Central")

Ci <- CIS %>%
  filter(CISURVEYNAME =="")%>%
  arrange(DateTime)

upon <- sapply(1:length(Ci$ROW),function(i) DC6[DC6$DateTime==Ci$ONDateTime[i],7])
upon <-(onup-mean[1,])

upoff <- sapply(1:length(Ci$ROW),function(i) DC6[DC6$DateTime==Ci$OFFDateTime[i],7])
upoff <- (upoff-mean[2,])

downon <- sapply(1:length(Ci$ROW),function(i) DC7[DC7$DateTime==Ci$ONDateTime[i],7])
downon <-(downon-mean[3,])

downoff <- sapply(1:length(Ci$ROW),function(i) DC7[DC7$DateTime==Ci$OFFDateTime[i],7])
downoff <-(downoff-mean[4,])

upMeasure <- -9630
downMeasure <- 63757

upmean <- mean


updiff <- abs(upMeasure-Ci$milepost)
downdiff <- abs(downMeasure-Ci$milepost)

Ci$OnComp <- as.numeric(Ci$structure_ps__cis)+
  upon*updiff/(updiff+downdiff)+
  downon*downdiff/(updiff+downdiff)

Ci$OffComp <- as.numeric(Ci$structure_irf__cis)+
  updiff*upoff/(updiff+downdiff)+
  downdiff*downoff/(updiff+downdiff)


Cimelt <- Ci %>%
  melt(id.vars= -c(5,6,19,20),variable.name = "Survey", value.name = "Potential")
  Cimelt$Potential <- as.numeric(Cimelt$Potential)

p1 <- ggplot(Cimelt, aes(x=milepost,y=Potential,color=Survey))+
  geom_line()+
  geom_hline(aes(yintercept=-0.85),color="red")+
  scale_y_continuous(trans = 'reverse',breaks = c(seq(0,-2,-0.1)), limits = c( 0,-1.75))
ggplotly(p1)








  