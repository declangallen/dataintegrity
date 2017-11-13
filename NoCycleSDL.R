library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(plotly)


SDL <- function(file){
  
  datDC <- read.csv(dir()[7])
  

  DC <- select(datDC,DC.Reading,Record.Type,colnames(datDC[2]), Record.Type) 
  
  DC$DateTime <- as.POSIXct(as.character(DC[[3]]), 
                            format = "%m-%d-%Y %H:%M:%OS", 
                            origin="1899-12-30",
                            tz="UTC")##UTC ##US/Central
  DC$DateTimeCDT <- with_tz(DC$DateTime,tzone = "US/Central")
  DC$DateTimeEDT <- with_tz(DC$DateTime,tzone = "US/Eastern")

  DC <- DC%>%
    filter(Record.Type == "DC Reading") %>%
    select(Record.Type,DateTime,DateTimeEDT,DateTimeCDT,DC.Reading)%>%
    droplevels()
          
  
  g <- ggplot(DC[1:(dim(DC)[1]-1),], aes(DateTimeEDT, DC.Reading))+
    geom_line()+
    labs(x="Time", y="Pipe to Soil Potential (V)")+
    scale_y_continuous(trans = 'reverse',breaks = c(seq(0,-3.1,-0.1)), limits = c(0,-3.1))+
    theme(panel.grid.major = element_line(colour = "grey"), 
          panel.grid.minor = element_line(colour = "grey"),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))+
    geom_hline(aes(yintercept=-0.850),color="red")+
    scale_x_datetime(breaks = date_breaks("1 hour"), 
                     minor_breaks=date_breaks("1 hour"), 
                     labels=date_format("%m-%d %H:%M",tz = "US/Eastern"))+
    ggtitle(1)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90,vjust=0.25))
  plot(g)
  
  ggplotly(g)
  

  
}

Cycle <- function(On,Off){
  paste(c(rep("Off",Off),rep("On",On)))
}



  