install.packages("zoo")

setwd("C:/Users/dgallen/Desktop/R Work")

library(magrittr)
library(tidyverse)
library(plotly)
dat <- as.tibble(read.csv("elevation_profile.csv"))


Rupture <- 3621

dat %<>%
  mutate(Rup = 'N')%>%
  mutate(rowno = as.numeric(rownames(dat)))
dat[match(min(abs(dat$stn-Rupture)),abs(dat$stn-Rupture)),which(colnames(dat)=="Rup")] = 'Y'


r <- which(dat$Rup=="Y")
length <- length(dat$stn)
str(dat)



datup <- arrange(dat[1:r,],desc(stn))%>%
  mutate(rowno = as.numeric(rownames(dat[1:r,])))
datup <- datup %>%
  mutate(upslope = as.numeric(ifelse(datup$elevation[1:length(datup$stn)]<
                                       datup$elevation[1:length(datup$stn)+1],datup$elevation,"")))
datup <- datup %>%
  mutate(max=cummax(datup$elevation))
datup <- datup %>%
  mutate(upslope1 = ifelse(datup$upslope[1:length(datup$stn)]<
                             datup$max[1:length(datup$stn)],
                           NA,datup$max[1:length(datup$stn)]))



datdown <- dat[r:length,] %>%
  mutate(rowno = as.numeric(rownames(dat[r:length,])))

datdown <- datdown %>%
  mutate(upslope = as.numeric(ifelse(datdown$elevation[1:length(datdown$stn)]<
                                       datdown$elevation[1:length(datdown$stn)+1],
                                     datdown$elevation,"")))
datdown <- datdown %>%
  mutate(max=cummax(datdown$elevation))
datdown <- datdown %>%
  mutate(upslope1 = ifelse(datdown$upslope[1:length(datdown$stn)]<
                             datdown$max[1:length(datdown$stn)],
                           NA,datdown$max[1:length(datdown$stn)]))






datF <- arrange(rbind(datup,datdown),stn)

x=as.numeric(datF[datF$Rup=="Y",1][1,])
y=as.numeric(datF[datF$Rup=="Y",2][1,])

p <- ggplot(datF)+
  geom_line(aes(x=stn,y=elevation))+
  geom_point(aes(x=stn,y=elevation))+
  geom_line(data=datdown[1:datdown$rowno[datdown$elevation==max(datdown$elevation)],],
            aes(x=stn,y=max,color="red"))+
  geom_line(data=datup[1:datup$rowno[datup$elevation==max(datup$elevation)],],
            aes(x=stn,y=upslope1,color="red"))+
  geom_point(aes(x=x,y=y,color="blue"))
ggplotly(p)

View(datF)



p <- ggplot(datdown)+
  geom_line(aes(x=stn,y=elevation))+
  geom_line(data=datdown[1:datdown$rowno[datdown$elevation==max(datdown$elevation)],],
            aes(x=stn,y=max,color="red"))+
  geom_line(data=datup[1:datup$rowno[datup$elevation==max(datup$elevation)],],
            aes(x=stn,y=max,color="red"))+
  colScale
ggplotly(p)


q <- ggplot(datup)+
  geom_line(aes(x=stn,y=elevation))+
  geom_line(data=datup[1:datup$rowno[datup$elevation==max(datup$elevation)],],
            aes(x=stn,y=max,color="red"))+
  colScale
ggplotly(q)

