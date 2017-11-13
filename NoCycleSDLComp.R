library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(plotly)


SDL <- function(file){
  
  datDC <- read.csv("20171101_TCPL7200744_150th Ave_udl1_sn002049.csv")
  datDC <- read.csv("20171101_TCPL7200744_120TH AVE_udl1_sn001788.csv")
  datDC <- read.csv("20171101_TCPL7200744_12 mile rd depol_udl1_sn001788.csv")
  
  
  datDC <- rbind(datDC2,datDC1)
  
  filter(datDC,!is.na(GPS.Latitude))

  DC <- select(datDC,DC.Reading,Record.Type,colnames(datDC[2]), Record.Type) 
  
  DC$DateTime <- as.POSIXct(as.character(DC[[3]]), 
                            format = "%m-%d-%Y %H:%M:%OS", 
                            origin="1899-12-30",
                            tz="UTC")
  DC$DateTimeCDT <- with_tz(DC$DateTime,tzone = "US/Central")

  DC <- DC%>%
    filter(Record.Type == "DC Reading") %>%
    select(Record.Type,DateTimeCDT,DC.Reading)
  
  DC <- filter(DC, DateTimeCDT > "2017-10-30 14:00:00")
          
  
  g <- ggplot(DC[1:(dim(DC)[1]-1),], aes(DateTimeCDT, DC.Reading))+
    geom_line()+
    labs(x="Time", y="Pipe to Soil Potential (V)")+
    scale_y_continuous(trans = 'reverse',breaks = c(seq(0,-2,-0.1)), limits = c( 0,-1.75))+
    theme(panel.grid.major = element_line(colour = "grey"), 
          panel.grid.minor = element_line(colour = "grey"),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))+
    geom_hline(aes(yintercept=-0.850),color="red")+
    scale_x_datetime(breaks = date_breaks("1 hour"), 
                     minor_breaks=date_breaks("1 hour"), 
                     labels=date_format("%m-%d %H:%M",tz = "US/Central"))+
    ggtitle(1)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90,vjust=0.25))
  plot(g)
  
  ggplotly(g)
  

  
}

Cycle <- function(On,Off){
  paste(c(rep("Off",Off),rep("On",On)))
}

mileRD <- DC
Ave120th <- DC
Ave150th <- DC

mean(Ave120th$DC.Reading)

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
######
Ci <- CIS %>%
  filter(CISURVEYNAME =="20171101_TCPL7200744_LINE 409_CIS_ONOFF_MP3.10_N_J")%>%
  arrange(CADStationing)

upMeasure <- -9630
downMeasure <- 8000

upMeasure <- -9630
downMeasure <- 8000

upon <- unlist(sapply(1:length(Ci[[1]]),
               function(i) mileRD[mileRD$DateTimeCDT==Ci$ONDateTime[i],3]))
upon <-upon-mean(mileRD$DC.Reading)

downon <- unlist(sapply(1:length(Ci[[1]]),
               function(i) Ave120th[Ave120th$DateTimeCDT==Ci$ONDateTime[i],3]))
downon <-downon-mean(mileRD$DC.Reading)

updiff <- abs(upMeasure-Ci$CADStationing)
downdiff <- abs(downMeasure-Ci$CADStationing)

Ci$OnComp <- as.numeric(Ci$`CIS Native P/S`)+
  upon*updiff/(updiff+downdiff)+
  downon*downdiff/(updiff+downdiff)


########
Ci <- CIS %>%
  filter(CISURVEYNAME =="20171101_TCPL7200744_LINE 409_CIS_DEPOL_MP 0_N_JES")%>%
  arrange(CADStationing)

upMeasure <- 862
downMeasure <- 6780

upon <- unlist(sapply(1:length(Ci[[1]]),
               function(i) Ave150th[Ave150th$DateTimeCDT==Ci$ONDateTime[i],3]))
upon <-upon-mean(Ave150th$DC.Reading)

downon <- unlist(sapply(1:length(Ci[[1]]),
                 function(i) Ave120th[Ave120th$DateTimeCDT==Ci$ONDateTime[i],3]))
downon <-downon-mean(mileRD$DC.Reading)


updiff <- abs(upMeasure-Ci$CADStationing)
downdiff <- abs(downMeasure-Ci$CADStationing)


Ci$OnComp <- as.numeric(Ci$`CIS Native P/S`)+
  upon*updiff/(updiff+downdiff)+
  downon*downdiff/(updiff+downdiff)

Cimelt <- Ci %>%
  melt(id.vars= -c(13,20),variable.name = "Survey", value.name = "Potential")
Cimelt$Potential <- as.numeric(Cimelt$Potential)

p1 <- ggplot(Cimelt, aes(x=Cimelt$CADStationing,y=Potential,color=Survey))+
  geom_line()+
  geom_hline(aes(yintercept=-0.85),color="red")+
  scale_y_continuous(trans = 'reverse',breaks = c(seq(0,-2,-0.1)), limits = c( 0,-1.75))
ggplotly(p1)



  