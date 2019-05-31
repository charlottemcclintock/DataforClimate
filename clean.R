
# Cleaning Script
# Yale Climate Change Opinion Maps

# -------------------------------- set up ----------------------------------------

# load libraries
library(tidyverse)
library(maps)
library(tools)

# read in data
ycom <- read_csv("data/opinion/YCOM_2018_Data.csv")
county_df <- map_data("county")
damages <- read_csv("data/impacts/county_damages_by_sector.csv")


# ------------------------- clean yale climate data -------------------------------

# names to lower case
names(ycom) <- str_to_lower(names(ycom))

diff <- ycom

# calculate the difference between % happening and % worried 
diff$diff <- with(diff, happening-worried) # all positive differences

# subset to county geography
diff <- subset(diff, geotype=="County")

# separate county names for join
diff <- separate(diff, geoname, into=c("county","state"), sep=", ")
diff <- separate(diff, county, into=c("county"), sep=" County", extra="drop")
diff <- separate(diff, county, into=c("county"), sep=" Parish", extra="drop")
diff <- separate(diff, county, into=c("county"), sep=" Borough", extra="drop")
diff <- separate(diff, county, into=c("county"), sep=" Census Area", extra="drop")
diff <- separate(diff, county, into=c("county"), sep=" city", extra="drop")

# string to lower for join
diff$county <- str_to_lower(diff$county)
diff$state <- str_to_lower(diff$state)

# rename for join to geo
diff <- rename(diff, "region"="state", "subregion"="county")

# ------------------------- clean climate impact data ---------------------------

# variable names to lower 
names(damages) <- str_to_lower(str_replace_all(names(damages), "[^[:alnum:]]", ""))

# rename variable name 
damages <- rename(damages, "county"="countyname")

# drop geo names for merge
damages <- separate(damages, county, into=c("county"), sep=" County", extra="drop")
damages <- separate(damages, county, into=c("county"), sep=" Parish", extra="drop")
damages <- separate(damages, county, into=c("county"), sep=" Borough", extra="drop")
damages <- separate(damages, county, into=c("county"), sep=" Census Area", extra="drop")
damages <- separate(damages, county, into=c("county"), sep=" city", extra="drop")
damages <- separate(damages, county, into=c("county"), sep=" City", extra="drop")
damages <- separate(damages, county, into=c("county"), sep=" and", extra="drop")

# to lower for merge
damages$county <- str_to_lower(damages$county)

# rename for merge
damages <- rename(damages, "subregion"="county")

# read in state codes for merge
xwalk <- read_csv("data/geo/states.csv")
names(xwalk) <- c("region", "statecode")

# join state codes to states
damages <- left_join(damages, xwalk, by="statecode")
damages$region <- str_to_lower(damages$region) # state names to lower

# join damages to diff
damages <- left_join(diff, damages, by=c("subregion", "region"))




# ------------------------------ join to geo ---------------------------------------

# join to geo
map <- left_join(county_df, damages, by=c("region", "subregion"))

# rename super long names 
map <- rename(map, 
              "damages"="totaldamagescountyincome", 
              "coastal_10"="coastaldamagelog10countyincome")


# county names to upper 
map$subregion <- toTitleCase(map$subregion)
map$region <- toTitleCase(map$region)
damages$subregion <- toTitleCase(damages$subregion)
damages$region <- toTitleCase(damages$region)

# drop geotype, all counties
map <- select(map, -geotype)

# -------------------------------- save ---------------------------------------

save.image("climate-opinion.RData")



