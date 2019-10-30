library(tidyverse)
library(readxl)

contrib_all <- read_xlsx("Top MA Donors 2016-2020.xlsx", sheet = "Direct Contributions & JFC Dist")
JFC <- read_xlsx("Top MA Donors 2016-2020.xlsx", sheet = "JFC Contributions (DO NOT SUM W")

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

# View tables
View(contributiors)
View(family)
View(employer)
View(organization)
View(zipcode)
View(city)
View(state)
View(orgs)
View(contribution)
View(dates)
View(recipients)

