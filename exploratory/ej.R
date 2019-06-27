
# Environmental Justice
# YEJSCREEN Data

# -------------------------------- set up ----------------------------------------

load("ej/ejscreen.RData")
load("data/climate-opinion.RData")

library(tidyverse)
library(highcharter)


ej$minpop <- ej$pop*ej$pctmin
ej$lipop <- ej$pop*ej$pctlowinc

ej <- ej %>% group_by(ST, statename, county) %>% 
  summarise(pop=sum(pop), 
            minpop=sum(minpop), 
            lipop=sum(lipop), 
            avg.pctile.cancer=mean(pctile.cancer,na.rm = T), 
            avg.pctile.dpm=sum(pctile.dpm*pop)/sum(pop), 
            avg.pctile.resp=sum(pctile.resp*pop)/sum(pop), 
            avg.pctile.traffic=sum(pctile.traffic.score*pop)/sum(pop), 
            avg.pctile.npdes=sum(pctile.proximity.npdes*pop)/sum(pop), 
            avg.pctile.npl=sum(pctile.proximity.npl*pop)/sum(pop), 
            avg.pctile.rmp=sum(pctile.proximity.rmp*pop)/sum(pop), 
            avg.pctile.tsdf=sum(pctile.proximity.tsdf*pop)/sum(pop), 
            avg.pctile.o3=sum(pctile.o3*pop)/sum(pop), 
            avg.pctile.pm=sum(pctile.pm*pop)/sum(pop))

ej$us <- "us"
ej$ST <- str_to_lower(ej$ST)
ej$county <- substring(ej$county, 3,5)

ej <- unite(ej, "code", us, ST, county, sep="-")
ej <- left_join(hgeo, ej, by="code")

ej$pctmin <- 100*ej$minpop/ej$pop
ej$pctlowinc <- 100*ej$lipop/ej$pop

ej <- rename(ej, "county_full"="name")
ej <- left_join(ej, damages, by="county_full")

saveRDS(ej, "ej.rds")

