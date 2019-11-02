library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(RSQLite)
library(DBI)

contrib_all <- read_xlsx("Top MA Donors 2016-2020.xlsx", sheet = "Direct Contributions & JFC Dist")

# Fix contrib
FirstName<-rep(NA,length(contrib_all$contrib))
for(r in 1:nrow(contrib_all)){ 
  FirstName[r]<-strsplit(contrib_all$contrib[r]," ")[[1]][2]  
}
contrib_all$contrib<-paste(sub(" .*", "", contrib_all$contrib), FirstName, sep=" ")
contrib_all$contrib[contrib_all$contrib=="SABAN, HAIM,,"]<-"SABAN, HAIM"
contrib_all$contrib[contrib_all$contrib=="FISH,JOHN,,,"]<-"FISH, JOHN"
contrib_all$contrib[contrib_all$contrib=="RASKY,LAWRENCE,"]<-"RASKY, LAWRENCE"
contrib_all$contrib[contrib_all$contrib=="FISH,JOHN,F"]<-"FISH,  JOHN"
contrib_all$contrib[contrib_all$contrib=="FISH,JOHN"]<-"FISH,  JOHN"
contrib_all$contrib[contrib_all$contrib=="SABAN,HAIM NA"]<-"SABAN, HAIM"
contrib_all$contrib[contrib_all$contrib=="SABERTY, NAOMI"]<-"ABERLY, NAOMI"
contrib_all$contrib[contrib_all$contrib=="ANITABEKENSTEIN NA"]<-"BEKENSTEIN, ANITA"
contrib_all$contrib[contrib_all$contrib=="ATKIN S,"]<-"ATKINS, CHESTER"
contrib_all$contrib[contrib_all$contrib=="FISH, JOHN,"]<-"FISH, JOHN"
contrib_all$contrib[contrib_all$contrib=="HOSTETTER, JR,"]<-"HOSTETTER, JR"
contrib_all$contrib[contrib_all$contrib=="PRITZKER SIMMONS,"]<-"SIMMONS, PRITZKER"
contrib_all$contrib[contrib_all$contrib=="STEWART &"]<-"SIMMONS, PRITZKER" 

# Fix Fecoccemp
# fix "." problem and case-sensitive problem
contrib_all$Fecoccemp <- gsub(contrib_all$Fecoccemp, pattern = "[:.:]\\s", replacement = "")  
contrib_all$Fecoccemp <- gsub(contrib_all$Fecoccemp, pattern = "[:.:]", replacement = "")  
contrib_all$Fecoccemp <- na.omit(contrib_all$Fecoccemp)  
contrib_all$Fecoccemp <- toupper(contrib_all$Fecoccemp)  
# get a 6-letter occupation/employer names list from Fecoccemp
Fecoccemp_list <- substring(contrib_all$Fecoccemp, 1, 6) 
Fecoccemp_list <- toupper(Fecoccemp_list)
Fecoccemp_list <- unique(Fecoccemp_list)
Fecoccemp_list <- Fecoccemp_list[-112] # fix blank value
# match with Fecoccemp
for (i in Fecoccemp_list){  
  index_Fecoccemp <- grep(i, contrib_all$Fecoccemp) # get the location of the matched data
  replacename_Fecoccemp <- contrib_all$Fecoccemp[index_Fecoccemp[1]] # get the replace name
  for (j in index_Fecoccemp){  
    contrib_all$Fecoccemp[j] <- replacename_Fecoccemp # replace Fecoccemp with the replace name
  }  
}  
# fix wrongly written characters
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="0"] <- NA
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="ADSVENTURES"] <- "ADS VENTURES"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="ALERE, INC"] <- "ALERE"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="ALERE INC"] <- "ALERE"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="AT-HOME"] <- "AT HOME"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="BLUE HAVEN INSTITUTE"] <- "BLUE HAVEN INITIATIVE"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="BLUEHAVEN INITIATIVE"] <- "BLUE HAVEN INITIATIVE"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="COMM OF MASS"] <- "COMMONWEALTH OF MASSACHUSETTS"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="E-SCRIPTION"] <- "ESCRIPTION"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="GLOBL PETROLEUM CORP"] <- "GLOBAL PARTNERS LP"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="GLOPAL P CORP"] <- "GLOBAL PARTNERS LP"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="GOBAL PETROLEUM CORP"] <- "GLOBAL PARTNERS LP"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="LUIRADX"] <- "LUMIRADX"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="MCCLEAN HOSPITAL"] <- "MCLEAN HOSPITAL"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="N / A"] <- NA
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="NOT-EMPLOYED"] <- "NOT EMPLOYED"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="PHIANTHROPIST"] <- "PHILANTHROPIST"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="PILOTHOUSE ASSOCIATES"] <- "PILOT HOUSE ASSOCIATES"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="PILOT HOUSE ASSOCIATION"] <- "PILOT HOUSE ASSOCIATES"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="SELF"] <- "SELF EMPLOYED"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="SELF-EMPLOYED"] <- "SELF EMPLOYED"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="SULFFOLK CONSTRUCTION COMPANY"] <- "SUFFOLK CONSTRUCTION CO, INC"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="TAAL  CAPITAL"] <- "TAAL CAPITAL"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="TUTFS UNIVERSITY"] <- "TUFTS UNIVERSITY"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="TUTS UNIVERSITY"] <- "TUFTS UNIVERSITY"
contrib_all$Fecoccemp[contrib_all$Fecoccemp=="UNEMPLOYED"] <- "NOT EMPLOYED"
contrib_all$Fecoccemp[contrib_all$Fecoccemp==""] <- NA


# Second Normal Form
contributiors <- select(contrib_all, contribid, fam, contrib, City, State, Zip, Fecoccemp, orgname)
orgs <- select(contrib_all, orgname, ultorg)
contribution <- select(contrib_all, fectransid, cycle, contribid, date, amount, type, recipient)
recipients <- select(contrib_all, recipient, recipid, recipcode, cmteid)

# Third Normal Form
contributiors <- select(contrib_all, contribid, fam, contrib, Zip, Fecoccemp, orgname)
zipcode <- select(contrib_all, Zip, City)
city <- select(contrib_all, City, State)
orgs <- select(contrib_all, orgname, ultorg)
contribution <- select(contrib_all, fectransid, contribid, date, amount, type, recipient)
dates <- select(contrib_all, date, cycle)
recipients <- select(contrib_all, recipient, recipid, recipcode, cmteid)

# Fourth Normal Form
contributiors <- select(contrib_all, contribid, contrib)
family <- select(contrib_all, contrib, fam)
employer <- select(contrib_all, contribid, Fecoccemp)
organization <- select(contrib_all, contribid, orgname)
zipcode <- select(contrib_all, contribid, Zip)
city <- select(contrib_all, Zip, City)
state <- select(contrib_all, City, State)
orgs <- select(contrib_all, orgname, ultorg)
contribution <- select(contrib_all, fectransid, contribid, date, amount, type, recipient)
dates <- select(contrib_all, date, cycle)
recipients <- select(contrib_all, recipient, recipid, recipcode, cmteid)

# Eliminate duplicative rows
contributiors <- base::unique(contributiors) 
family <- base::unique(family) 
employer <- base::unique(employer) 
organization <- base::unique(organization) 
zipcode <- base::unique(zipcode) 
city <- base::unique(city) 
state <- base::unique(state) 
orgs <- base::unique(orgs) 
contribution <- base::unique(contribution) 
dates <- base::unique(dates) 
recipients <- base::unique(recipients) 

# Fix weird value and remove NA
family <- na.omit(family)
organization <- na.omit(organization)
orgs <- na.omit(orgs)
employer <- filter(employer , employer$Fecoccemp != "." )

# Output database
mydb <- dbConnect(SQLite(), "shixin.sqlite")

dbWriteTable(mydb, "contributiors", contributiors)
dbWriteTable(mydb, "family", family)
dbWriteTable(mydb, "employer", employer)
dbWriteTable(mydb, "organization", organization)
dbWriteTable(mydb, "zipcode", zipcode)
dbWriteTable(mydb, "city", city)
dbWriteTable(mydb, "state", state)
dbWriteTable(mydb, "orgs", orgs)
dbWriteTable(mydb, "contribution", contribution)
dbWriteTable(mydb, "dates", dates)
dbWriteTable(mydb, "recipients", recipients)

dbConnect(mydb)
