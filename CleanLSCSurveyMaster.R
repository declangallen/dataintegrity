library(tidyverse)
library(magrittr)
library(stringr)
library(data.table)
library(rgdal)
library(readxl)

setwd(paste("Y:/Enbridge/00917200348 Superior Region- Mainlines-CIS",
            " into ISM Data Integration/Working Docs/PCS/GPS Gaps/SuperiorW1",sep = ""))
dir()

readCIS <- function(filename,Pipe){
  ##read the CIS data from the PCS export
  dat <- read.csv(filename)
  ##PCS pushed out the +. Really annoying.
  dat$Latitude[dat$Latitude == 0] <- NA
  dat$Longitude[dat$Longitude == 0] <- NA
dat %>% 
    mutate(STN = as.numeric(gsub("[+]","",dat$Station.Number)),
           STARTDATE = as.POSIXct(as.character(dat$Start.Date),format = "%m/%d/%Y"),
           Pipe = Pipe,
           CISurveyName = as.factor(paste(CI.Survey.Name,Start.Date)))%>%
    ##arrange the data based on STN
    arrange(STN) %>%
    return()
}

duplicates <- function(x,variable1,variable2){
table(duplicated(x$variable1)&duplicated(x$variable2))
}

##some exploratory dplyr functions to see what surveys are missing GPS and how many.... 

naGPS <- function(x){
x %>%
    filter(is.na(Latitude)) %>%
    droplevels()%>%
    return()
}

ggplot(supW13isGPS,aes(x=Longitude,y=Latitude))+
  geom_point()
 

isGPS <- function(x){
  x %>%
    filter(!is.na(Latitude)) %>%
    droplevels()%>%
    return()
}

 
sumCIS <- function(x){
  x %>%
    group_by(CI_Survey_Name)%>%
    summarise(n=n(),min=min(MEAS),max=max(MEAS),Date=Start_Date[1])%>%
    mutate(Records=n)%>%
    select(-n)%>%
    arrange(min)
}


bookendGPSGap <- function(df){
  ##filter all dataset to find all missing Latitude values
  dat <- filter(df,is.na(Latitude)) %>% droplevels() 
  ##find the survey names associated with the missing GPS
  levels <- levels(dat$CISurveyName)
  ##filter the dataset to only the surveys with missing GPS
  Cal <- filter(df,CISurveyName %in% levels) %>% droplevels()
  ##add a column that offsets the row number by -1. This is needed to find the start of the GPS Gap
  Cal <- arrange(Cal,CISurveyName)%>%
    mutate(CAL = as.numeric(rownames(Cal))-1)
    
  ##The first row number can't be a - number or zero so coerce it to be 1. 
  ##This would be a problem if the very first entry was a bookend. unlikely
  
  Cal$CAL[1] <- 1
  ##Create dataset
 Cal %>% 
    ##Is the Lat from the row NA and the next one in the list non NA? if so then 1 if not 0
    mutate(CAL1 = ifelse(is.na(Latitude)& !is.na(Latitude)[as.numeric(rownames(Cal))+1],1,0),
           ###Is the Lat from the row NA and the previous one in the list non NA? if so then 1 if not 0
           CAL2 = ifelse(is.na(Latitude)& !is.na(Latitude)[as.numeric(CAL)],1,0),
           ##Combine the bookends into one field. 
           ##Locations where there is only 1 missing GPS throw up interesting case. 
           ##CAL1-3 will all be 1
           CAL3 = ifelse((CAL1[as.numeric(CAL)]==1 | CAL2[as.numeric(rownames(Cal))+1]==1)|(CAL1==1&CAL2==1),1,0)) %>%
    ##filter the GPS bookends but also the locations without GPS due to it being 1 missing record by itself
    filter(CAL3==1& !is.na(Latitude)) %>% 
    ##drop the levels from the other survey names
    droplevels()%>%
    
    ##return the filtered dataset
    return()
}

printCISurveys <- function(x,fileext){
  myList <- split(x,f=x$CISurveyName)
  
  names(myList) <- str_replace_all( levels(x$CISurveyName),"[^[:alnum:]]","_")
  lapply(1:length(myList),function(i) write.csv(myList[[i]],
                                                file=paste0(names(myList[i]),fileext),
                                                row.names = FALSE,
                                                na = ""))
}

write.csv(supW65isGPS,"supW65isGPS.csv",row.names = FALSE,na="")

p <- ggplot(supW4,aes(x=Longitude,y=Latitude))+
         geom_point()
  ggplotly(p)
  


  
fgdb <- "C:/Users/dgallen/Desktop/Geos/ENB/SUPERIOR.gdb"  
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)


readARCMap <- function(x) {
  dat <- readOGR(dsn=fgdb,layer=x)
  dat <- as.data.frame(dat)
  return(dat)
  }


calP%>%group_by(LINE,STN)%>%summarise(n=n())%>%arrange(desc(n))


supW2measure %>% 
  filter(is.na(coords.x1))%>%
  summarise(n=n())

b <- duplicated(supW2measure[,c(2)])&
         !duplicated(supW2measure[,c(25,26,2)])

s <- duplicated(supW2measure[,c(25,26)])&
  !duplicated(supW2measure[,c(7)])

j <- duplicated(n[,c(5,6)])&
  !duplicated(n[,c(1)])

c <-  duplicated(supW67measure[,c(2)],fromLast = TRUE)&
      !duplicated(supW67measure[,c(24,25,2)],fromLast = TRUE)

t <- supW2measure[s,]%>%
  group_by(CI_Survey_Name,MEAS,coords.x2,coords.x1)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  ungroup()
  
  t%>%group_by(n)%>%summarise(count=n())


SurveyAppend <- function(Survey1,Survey2){
  
  myList <- Survey1$MEAS
  min <- min(Survey2$MEAS)
  max <- max(Survey2$MEAS)
  
  Survey1$add <- as.numeric(sapply(1:length(myList),
                                   function(i)ifelse((Survey1$MEAS[i]>min)&(Survey1$MEAS[i]<max),0,1)))
  Survey2$add <- 1
  z <- rbind(Survey1[Survey1$add==1,],Survey2)%>%
    arrange(MEAS)%>%
    droplevels()
return(z)
}


