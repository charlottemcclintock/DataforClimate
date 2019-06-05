
# Mapping
# Yale Climate Change Opinion Maps

# -------------------------------- set up ----------------------------------------

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiY2hhcmxvdHRlbWNjbGludG9jayIsImEiOiJjancyNmpja3EwdGdrNDlwZzk2NThwNWFyIn0.VrHeDXCIvkgAIDJNtgOOQw')

# set working directory
setwd("data")

# load libraries
library(plotly)
library(tidyverse)
library(maps)
library(psycho)

# read in data
load("climate-opinion.RData")

# ----------------------- happening, worried, diff ---------------------------------

arb <- subset(map, region=="Kansas")
cent_long <- median(arb$long)
cent_lat <- median(arb$lat)

# BELIEVE CLIMATE CHANGE IS HAPPENING
p <- map %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~happening, 
              colors = c('yellow','blue'),
              text = ~subregion, hoverinfo = 'text', 
              showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5),
               showlegend = FALSE, hoverinfo = 'none'
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 3,
      center = list(lat = ~cent_lat, lon = ~cent_long)),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p

# WORRIED ABOUT CLIMATE CHANGE
p <- map %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~as.factor(round(worried)), 
              colors = c('yellow','blue'),
              text = ~subregion, hoverinfo = 'text', 
              showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5),
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 3,
      center = list(lat = ~cent_lat, lon = ~cent_long)),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p


# HAPPENING BUT NOT WORRIED
p <- map %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~as.factor(round(diff)), 
              colors = c('yellow','blue'),
              text = ~paste0(subregion, ", ", region, "\n", 
                             round(happening), "% believe climate change is happening", "\n",
                            round(worried), "% worried about climate change", "\n", 
                            round(diff), "% happening but not worried"),
              hoverinfo = 'text',
              showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5),
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 3,
      center = list(lat = ~cent_lat, lon = ~cent_long)),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p

# --------------------- economic damages & impacts --------------------------------

p <- map %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~as.factor(round(damages)), 
              colors = c('yellow','blue'),
              text = ~subregion, hoverinfo = 'text', 
              showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5)
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 3,
      center = list(lat = ~cent_lat, lon = ~cent_long)),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p

p <- map %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~as.factor(round(laborlowrisk,2)), 
              colors = c('yellow','blue'),
              text = ~subregion, hoverinfo = 'text', 
              showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5)
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 3,
      center = list(lat = ~cent_lat, lon = ~cent_long)),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p

p <- map %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~as.factor(round(laborhighrisk,1)), 
              colors = c('yellow','blue'),
              text = ~subregion, hoverinfo = 'text', 
              showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5)
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 3,
      center = list(lat = ~cent_lat, lon = ~cent_long)),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p

map$damages_standard <- standardize(map$damages)
map$worried_standard <- standardize(map$worried)


# does is change the distribution?
ggplot(map, aes(damages_standard)) + geom_density()
ggplot(map, aes(damages)) + geom_density() # nope


# COMPARING DAMAGES TO WORRIES
p <- map %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~factor(round(damages_standard-worried_standard,1)), 
              colors = c('yellow','blue'),
              text = ~paste0(subregion, ", ", region, "\n",
                             round(damages_standard,1), "\n", round(worried_standard,1)), 
              hoverinfo = 'text', 
              showlegend = FALSE) %>%
  add_polygons(
    line = list(width = 0.4)
  ) %>%
  add_polygons(fillcolor = 'transparent',
               line = list(color = 'black', width = 0.5),
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
    mapbox = list(
      style = 'light',
      zoom = 3,
      center = list(lat = ~cent_lat, lon = ~cent_long)),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p

ggplot(map, aes(damages_standard, worried_standard)) + geom_point()


# ------------------------------ to do --------------------------------------------

# possible correlates of climate change denial:
# density (urban/rural)
# trump votes
# economic risk from yale study
