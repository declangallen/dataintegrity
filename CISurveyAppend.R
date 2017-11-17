library(tidyverse)
library(magrittr)
library(stringr)
library(data.table)
library(rgdal)
library(readxl)
library(dplyr)
options(digits = 15)


sup13%>% 
  filter(is.na(coords.x1))%>%
  summarise(n=n())



b <- duplicated(supW2measure[,c(2)])&
         !duplicated(supW2measure[,c(25,26,2)])

###
s <- duplicated(sup13[,c(21,20,4)])&
  duplicated(sup13[,c(4)])

###
j <- duplicated(dat[,c(5,6)])&
  !duplicated(dat[,c(2)])

###duplicate stn vs with no duplicate lat long stn
c <-  duplicated(sup13[,c(4)],fromLast = TRUE)&
      !duplicated(sup13[,c(21,22,4)],fromLast = TRUE)

t <- sup13[s,]%>%
  group_by(CI_Survey_Name,Station_Number,coords.x2,coords.x1)%>%
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
print(z)
}

readheader <- function(x){
  
  patterns <- c(".CSV",".csv")
  
  myList <- list.files(pattern = paste(patterns,collapse = "|"))
  sapply(1:length(myList), function(i) names(read.csv(myList[[i]])))
  
}

patterns <- c(".CSV",".csv")
myList <- list.files(pattern = paste(patterns,collapse = "|"))

myList <- list.files(pattern = (".xlsx"))

filenames <- paste("GPS6A",1:length(myList),sep = "")

###to get multiple files into multiple datasets
for(i in 1:length(myList)){
    assign(filenames[i], 
           read_xls(myList[i]))
  }

##read files into list
LSCEast13xlsx <- lapply(1:length(myList),function(i) read_xlsx(myList[[i]]))
LSCEast61csv <- lapply(1:length(myList),function(i) read.csv(myList[[i]]))

##give all tables a start date of....
LSCEast13xlsx <- lapply(LSCEast13xlsx,cbind, ORGSTARTDATE="2012-10-01")
LSCEast61csv <- lapply(LSCEast61csv,cbind, ORGSTARTDATE="2012-10-01")

##break out file names from a 1x7 list to a 7x1 list. then we can use it in lapply.
files <- mapply(list,myList)

files <- lapply(mapply(list,myList),paste,"LINE_61_EAST_csv",sep="_")

###assing the surveyname to the filename
LSCEast13xlsx <- mapply(cbind,
                        LSCEast13xlsx,
                        "ORGCISURVEYNAME"=files,
                        SIMPLIFY = F)
LSCEast61csv <- mapply(cbind,
                       LSCEast61csv,
                       "ORGCISURVEYNAME"=files,
                       SIMPLIFY = F)


sup61eastcsv <- rbindlist(LSCEast61csv,fill = TRUE)



sup61eastcsv <- mutate(sup61eastcsv, 
                       CISURVEYNAME = "Line_61_SUPERIOR_EAST_LSC_VERI_COMBINE.csv")

write.csv(sup61eastcsv,"Line_61_SUPERIOR_EAST_LSC_VERI_COMBINE.csv",na="")

sup13eastcsv <- n

sup13eastcsv%>%
  group_by(as.factor(ORGCISURVEYNAME))%>%
  summarise(n=n(),mean=mean(STRUCTURE_PS__CIS))%>%
  print(n=nrow(.))

##remove the element from the list and turn it into a data frame
list <- LSCEast6Acsv[[2]]
##rename the colheaders
colnames(list2)[10] <- "LATITUDE"
colnames(list2)[11] <- "LONGITUDE"

##push the dataframe back into the right location in the list
LSCEast6Acsv[2] <- list(list)



L6A <- read.csv(dir()[14])
dat <- L6A[!is.na(L6A$LATITUDE),]
datNoGPS <- L6A[is.na(L6A$LATITUDE),]

j <- duplicated(dat[,c(5,6)])&!duplicated(dat[,c(2)])
dat <- arrange(dat,STATION)
head(dat[j,2:7],n=15)
head(dat[,2:7],n=15)

datnoDup <- dat[!j,]
t <- dat[j,]%>%
  group_by(ORGCISURVEYNAME,STATION,LATITUDE,LONGITUDE)%>%
  summarise(n=n())%>%
  arrange(STATION)%>%
  ungroup()

t%>%group_by(n)%>%summarise(count=n())









