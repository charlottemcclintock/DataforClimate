
# C. McClintock
# Brightest Research
# Yale Climate Change Opinion Maps

# -------------------------------- set up ----------------------------------------

# load libraries
library(tidyverse)
library(maps)
library(tools)
library(urbnmapr)

# read in data
ycom <- read_csv("YCOM_2018_Data.csv")
al <- read_csv("../alliance/alliance.csv")

# ------------------------- clean yale climate data -------------------------------

# names to lower case
names(ycom) <- str_to_lower(names(ycom))
names(al) <- str_to_lower(names(al))

# subset to state geography
ycom <- subset(ycom, geotype=="State")

# drop and change names
ycom <- select(ycom, -geotype) %>% rename("state"="geoname") %>% 
  filter(!state=="District of Columbia")

# join to climate alliance data to filter
ycom <- left_join(ycom, al, by="state")

# filter to states not in climate alliance to minimize distractions
ycom <- filter(ycom, alliance=="N")

# join to state geographies
data(states)
states <- rename(states, "abbreviation"="state_abbv", 
                 "state"="state_name")
ycom_states <- left_join(states, ycom, by=c("state","abbreviation"))


# ------------------------- build opinion maps -------------------------------

# happening
ggplot() + 
  geom_polygon(data = ycom_states, mapping = aes(x = long, y = lat, group = group, fill = happening),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")

# human-caused
ggplot() + 
  geom_polygon(data = ycom_states, mapping = aes(x = long, y = lat, group = group, fill = human),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")

# worried
ggplot() + 
  geom_polygon(data = ycom_states, mapping = aes(x = long, y = lat, group = group, fill = worried),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")

# will be personally affected
ggplot() + 
  geom_polygon(data = ycom_states, mapping = aes(x = long, y = lat, group = group, fill = personal),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")

# average of states not in climate alliance
mean(ycom$happening)
mean(ycom$human)
mean(ycom$worried)
mean(ycom$personal)


# --------------------- build policy support maps -----------------------------


# governor should do more about climate
ggplot() + 
  geom_polygon(data = ycom_states, 
               mapping = aes(x = long, y = lat, group = group, fill = governor),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")



# fund renewable energy
ggplot() + 
  geom_polygon(data = ycom_states, 
               mapping = aes(x = long, y = lat, group = group, fill = fundrenewables),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")

# regulate CO2 as a pollutant
ggplot() + 
  geom_polygon(data = ycom_states, 
               mapping = aes(x = long, y = lat, group = group, fill = regulate),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")

# support setting strict CO2 limits on existing coal-fired power plants
ggplot() + 
  geom_polygon(data = ycom_states, 
               mapping = aes(x = long, y = lat, group = group, fill = co2limits),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#de425b", mid = "#fbca73",
                       high = "#248f3b", na.value="grey", space ="Lab", 
                       midpoint = 50, limits=c(0,100)) +
  theme_void() + labs(fill="")

# average of states not in climate alliance
mean(ycom$governor)
mean(ycom$regulate)
mean(ycom$co2limits)
mean(ycom$fundrenewables)
