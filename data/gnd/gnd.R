
library(readxl)
library(tidyverse)

setwd("~/Projects/Active/DataforClimate/data/gnd")

gnd <- read_excel("gnd.xlsx", sheet=2)

names(gnd) <- str_to_lower(names(gnd))
names(gnd)[2] <- "type"

gnd$fill <- "1"

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(gnd, locationmode = 'USA-states') %>%
  add_trace(
    z = ~fill, locations = ~code) %>%
  layout(
    title = 'State Adaptation of 100% Clean Energy Targets',
    geo = g
  )
p

library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

p <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
    text = ~paste(df$name, "<br />", df$pop/1e6, " million")
  ) %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)

