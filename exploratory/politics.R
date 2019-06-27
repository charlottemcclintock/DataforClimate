
# Cleaning Script
# Political Engagement Data

# -------------------------------- set up ----------------------------------------

# load libraries
library(tidyverse)
library(maps)
library(tools)

setwd("data")

# read in data
load("elections/countypres_2000-2016.RData")

res <- subset(x, year=="2016") %>% select(-c(candidate, office))

res <- spread(res, key=party, value=candidatevotes)

res <- rename(res, "subregion"="county", "region"="state", "other"="NA")

res <- left_join(res, damages, by=c("region", "subregion"))

mod <- select(res, democrat, republican, other, totalvotes, 
              countypopulationin2012, countyincomein2012, 
              worried, happening, personal, diff)

mod$rep_support <- 100*mod$republican/mod$totalvotes

mod$turnout <- 100*with(mod, (democrat+republican+other)/countypopulationin2012)

mod1 <- lm(diff ~ turnout + rep_support + countyincomein2012 + countypopulationin2012, data=mod)
summary(mod1)
margins(mod1)


mod2 <- lm(personal ~ turnout + rep_support + countyincomein2012 + countypopulationin2012, data=mod)
summary(mod2)
margins(mod2)
