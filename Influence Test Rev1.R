
library(tidyverse)
library(magrittr)
library(stringr)
library(data.table)
library(rgdal)
library(lubridate)

Influence <- function(RectifierName,Time,plusminusseconds,file,name,
                    filename,ylim=c(0,-3.8), ymin=0.1,yaxis=-0.1){
  
  
  ###plotSDL("Rect","2017-04-22 06:00",600,"20170424_SunocoPRCGulfRD_udl1_sn001189.csv",
  ###"20170424_SunocoPRCGulfRD_udl1_sn001189-1-06.00.pdf",ylim=c(-0.5,-1.16),ymin=-1.11) 
  
  
  ###this SDL reader does is for uninterruped data readings. 
  ###SDLs are placed all over the place and you cycled rectifiers. 
  ### it simply reads the SDL and lets you quickly go the specific area in the SDL

  dat <- read.csv(file)
  DC <- filter(dat, Record.Type == "DC Reading")
  
  DC$DateTime <- if(colnames(DC)[2] == "Date.Time.Central.Daylight.Time.UTC.5.00."){
        mdy_hms(DC[[2]], tz="US/Central")
    } else{
      mdy_hms(DC[[2]], tz="UTC")
    }
  
    DC$DateTimeEST <- with_tz(DC$DateTime, tz= "EST")+3600
    
    ###Excel.Time.UTC.
  ###Excel.Time.Central.Daylight.Time.UTC.5.00.
  
  l <- ymd_hm(Time, tz="EST")
  z <-plusminusseconds
  lab <- paste(as.character(l),"EST")

  g <- ggplot(DC, aes(DateTimeEST,DC.Reading))+
        geom_line()+
    coord_cartesian(xlim=c(l-z,l+z ),y=ylim)+
    scale_y_continuous(trans = 'reverse', breaks = seq(ylim[1],ylim[2],yaxis))+
    labs(x="Time", y="Pipe to Soil Potential (V)")+
    geom_vline(aes(xintercept = as.numeric(l)))+
    annotate("text",x = l-z*0.04,y=ymin, hjust=0,label =RectifierName, angle=90)+
    annotate("text",x = l-z*-0.025,y=ymin,hjust=0, label =lab, angle=90)+
    theme(plot.margin = unit(c(0.25,0.75,0.25,0.25), "cm"))+
    ggtitle(name)+
    theme(plot.title = element_text(hjust = 0.5))
  
  plot(g)
  ggsave(filename, width=11, height=7)
        
}
ggsave(filename, width=11, height=7)

name <- as.data.frame(dir()[4:13])
AllData <- lapply(name,read.csv)
PulledGPS <- sapply(AllData,function(DF)
  DF[1,4])










