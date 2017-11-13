
setwd("Y:/Enbridge/00917200348 Superior Region- Mainlines-CIS into ISM Data Integration/Working Docs/PCS/Superior West/Bemidji")
dir()

a <- read.csv("LSC 2014 Combine Rev1.csv")

a$PIPE <- as.factor(sub('\\s*L.*','', a$Line))


LSC2014SupWCombine <-LSC2014SupWCombine %>% 
  mutate(STN = as.numeric(gsub("[+]","",LSC2014SupWCombine$MILEPOST)))

LSC2014SupWCombine <- droplevels(filter(a, grepl("LSC",a$SURVEY)))

LSC2014SupWCombine$STARTDATE <- gsub(",.*","",LSC2014SupWCombine$INSPECTION_DATETIME)

LSC2014SupWCombine$Vendor <- "LSC"
LSC2014SupWCombine$ROWCode <- "BEMIDJI"

write.csv(LSC2014SupWCombine,"LSC2014 SupW Combine.csv",row.names = FALSE, na="")             



table(LSC2014SupWCombine$STARTDATE,LSC2014SupWCombine$CISurveyName)

View(filter(LSC2014SupWCombine, CISurveyName=="LSC DEPOL 1LSC9"))




LSC2016Combine <- read.csv("LSC2016Combine.csv")

LSC2016Combine$CISurveyName <- as.factor(paste(LSC2016Combine$SURVEY,LSC2016Combine$Line))


write.csv(LSC2016Combine,"LSC2016 SupW Combine.csv",row.names = FALSE, na="")             




