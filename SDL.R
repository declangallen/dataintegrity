library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(plotly)


SDL <- function(file){
  
  dir()
  datDC <- read.csv(dir()[31])
  head(datDC)

  DC <- select(datDC,DC.Reading,Record.Type,colnames(datDC[2]), Record.Type) 
  
  ##Int <- paste(c(rep("OFF",1),rep("ON",3)))
  
  Int <- paste(c(rep("OFF",9),rep("off",1),rep("ON",29),rep("on",1)))
  IntSec <- paste(c(rep(1:10),c(1:30)))
  ###IntSec <- paste(c(rep(1),c(1:3)))
  
  day <- as.data.frame( seq(as.POSIXct("2017-11-01"),as.POSIXct("2017-11-25"),by=1))
  n <- dim(day)[1]
  day <- as.data.frame(day[1:(n-1),])
  day$DateTime <- day$`day[1:(n - 1), ]`
  day$Int <- rep_len(Int,length.out = n-1)
  day$IntSec <- rep_len(IntSec,length.out = n-1)
  day$Cycle <- paste(day$Int,day$IntSec,sep="_")
  
  DC$DateTime <- as.POSIXct(as.character(DC[[3]]), 
                            format = "%m-%d-%Y %H:%M:%OS", 
                            origin="1899-12-30",
                            tz="US/Central")##UTC ##US/Central
  DC$DateTimeCDT <- with_tz(DC$DateTime,tzone = "US/Central")
  
  DC <- merge(day, DC,by.x = "DateTime") %>%
    filter(Record.Type == "DC Reading") %>%
    select(Record.Type,Int,Cycle,DateTimeCDT,DC.Reading)
  DC$Cycle <- as.factor(DC$Cycle)
  DC$Int <- as.factor(DC$Int)

  
  DCplot <- filter(DC,Int != "ON",Int != "OFF")
  ##DCplot <- DC
  
  
  d <- filter(DC, DateTimeCDT > "2017-10-30 14:00:00",DateTimeCDT < "2017-10-30 15:00:00")
  
  g <- ggplot(DCplot[1:(dim(DCplot)[1]-1),], aes(DateTimeCDT, DC.Reading,color=Int))+
    geom_line()+
    labs(x="Time", y="Pipe to Soil Potential (V)")+
    scale_y_continuous(trans = 'reverse',breaks = c(seq(0,-2,-0.1)), limits = c( 0,-2))+
    theme(panel.grid.major = element_line(colour = "grey"), 
          panel.grid.minor = element_line(colour = "grey"),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))+
    geom_hline(aes(yintercept=-0.850),color="red")+
    scale_x_datetime(breaks = date_breaks("6 hour"), 
                     minor_breaks=date_breaks("3 hour"), 
                     labels=date_format("%m-%d %H:%M",tz = "US/Central"))+
    ggtitle("20171112_42nd St_sn002872")+    
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90,vjust=0.25))
  plot(g)
  
  ggsave("20171112_42nd St_sn002872.pdf",width=17,height=11)
  
  
  ggplotly(g)
  

  
}

Cycle <- function(On,Off){
  paste(c(rep("Off",Off),rep("On",On)))
}


ggsave(filenameDC, width=11, height=7)


no <- dim(DCplot)[1]
day <- as.data.frame(DCplot[1:(dim(DCplot)[1]-1),])

mean(DC[DC$Int=="ON"|DC$Int=="on",5])

mean(DC[DC$Cycle=="ON_1",5])


mean(DC[DC$Int=="OFF"|DC$Int=="off",5])

mean(DC[DC$Cycle=="OFF_1",5])






  