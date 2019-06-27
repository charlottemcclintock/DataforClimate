

# C. McClintock
# Brightest Research
# Yale Climate Change Opinion Maps

# -------------------------------- set up ----------------------------------------

# load libraries
library(tidyverse)
library(usmap)
library(urbnmapr)

# read in the data
damages <- read_csv("damages-counties.csv")
data(states)


# -------------------------------- map ----------------------------------------

# county-level economic damages
damages %>%
  ggplot(aes(long, lat, group = group, fill = 100*damages_perc)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#488f31", mid = "#e5e5e5",
                       high = "#de1f38", na.value="transparent", space ="Lab" ) +
  labs(fill="")+
  theme_void() + theme(legend.position = "right")  +
  geom_polygon(data = states, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff")


# create variable for weighted average calculation
damages$wa <- with(damages, countypopulationin2012*(totaldamagescountyincome/100))

# calculate weighted averages
state_damages <- damages %>% group_by(abbr) %>% 
  summarize(avg_damages=100*sum(wa)/sum(countypopulationin2012))

# join to state geography
states <- rename(states, "abbr"="state_abbv")
state_damages <- left_join(state_damages, states, by="abbr")

# state level economic damages (population-weighted average)
ggplot() + 
  geom_polygon(data = state_damages, mapping = aes(x = long, y = lat, group = group, fill = avg_damages),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient2(low = "#488f31", mid = "#e5e5e5",
                       high = "#de1f38", na.value="transparent", space ="Lab" ) +
  theme_void() + labs(fill="")

# calculate weighted average again, but weighted by county income
damages2 <- damages
damages2$wa <- with(damages2, countyincomein2012*(totaldamagescountyincome/100))
state_damages2 <- damages2 %>% group_by(abbr) %>% 
  summarize(avg_damages2=100*sum(wa)/sum(countyincomein2012))

# create data frame for comparison
comp <- left_join(state_damages, state_damages2, by="abbr")
comp$diff <- comp$avg_damages-comp$avg_damages2

# check differences
summary(comp$diff) # 25th -0.78 # 75th 0.90
ggplot(comp, aes(diff)) + geom_density() # most between 

