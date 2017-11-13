library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(plotly)
library(tidyverse)
library(stringr)


SDL <- function(file){
  

  setwd("Y:/CenterPoint Energy/06217200678 Riverside Pipeline-Indirect Inspections/Working Docs/Data/SDL")
  
  dir <- dir()[13]
  file <- toString(dir)
  filename <- paste(file,".pdf",sep="")
  datDC <- read.csv(dir)

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
                            tz="US/Central")##UTC ##US/Central
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
  
  DCplot <- filter(DC,(Cycle == "ON_29") | (Cycle== "OFF_9"))
  
  DCplot <- filter(DC,(Int == "ON" & IntSec>1)|(Int == "OFF" & IntSec>1))
  
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
    ggtitle(file)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90,vjust=0.25))
  plot(g)
  
  ggplotly(g)
  
  ggsave(filename,width=11, height=7)

    
}










  