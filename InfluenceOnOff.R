###plot the interrupted SDL.
###focus to a specific time in the SDL that the rectifier was cycled.
###this needs more work for larger cycle times



InfluenceOnOff <- function(RectifierName,Time,plusminusseconds,On,Off,file,name,
                             filename,ylim=c(0,-3.8), ymin=0.1,yaxis=-0.1){

datDC <- read.csv(file)

DC <- filter(datDC, Record.Type == "DC Reading")

DC$DateTime <- as.POSIXct(as.character(DC[[2]]),
                          format = "%m-%d-%Y %H:%M:%OS",
                          origin="1899-12-30",
                          tz="UTC")
DC$DateTimeEST <- with_tz(DC$DateTime,tzone = "EST")

Cycle <- paste(c(rep("Off",Off),rep("On",On))) 

day <- as.data.frame( seq(as.POSIXct("2017-10-26"),as.POSIXct("2017-11-2"),by=1))
n <- dim(day)[1]
day <- as.data.frame(day[1:(n-1),])
day$DateTime <- day$`day[1:(n - 1), ]`
day$Int <- rep_len(Int,length.out = n-1)
day$IntSec <- rep_len(IntSec,length.out = n-1)
day$Cycle <- paste(day$Int,day$IntSec,sep="_")

DC <- merge(day, DC,by.x = "DateTime") %>%
  filter(Record.Type == "DC Reading") %>%
  select(Record.Type,Int,Cycle,DateTimeCDT,DC.Reading)
DC$Cycle <- as.factor(DC$Cycle)
DC$Int <- as.factor(DC$Int)

l <- ymd_hm(Time, tz="EST")
z <-plusminusseconds
lab <- paste(as.character(l),"EST")


g <- ggplot(DC, aes(DateTimeEST, Unit,color=Cycle))+
  geom_line()+
  coord_cartesian(xlim=c(l-0.5*z,l+z ),y=ylim)+
  labs(x="Time", y="Current (ADC)")+
  scale_y_continuous(breaks = seq(ylim[1],ylim[2],yaxis))+
  geom_vline(aes(xintercept = as.numeric(l)))+
  annotate("text",x = l-z*0.04,y=ymin, hjust=0,label =RectifierName, angle=90)+
  annotate("text",x = l-z*-0.025,y=ymin,hjust=0, label =lab, angle=90)+
  theme(panel.grid.major = element_line(colour = "grey"), 
        panel.grid.minor = element_line(colour = "grey"),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  ggtitle(name)+
  theme(plot.title = element_text(hjust = 0.5))
plot(g)

ggsave(filename, width=11, height=7)

}

