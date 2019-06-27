
# ------------------------------- CLINTON/TRUMP COUNTIES ---------------------------------

library(plotly)
library(dplyr)

county_df <- map_data("county")
state_df <- map_data("state")

df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/votes.csv")

df <- select(df, Clinton, Trump, county_name)

df$county_name <- str_to_lower(df$county_name)

df$county_name %<>%
  gsub(" county", "", .) %>%
  gsub(" parish", "", .) %>%
  gsub(" ", "", .) %>%
  gsub("[.]", "", .)

county_df$subregion <- gsub(" ", "", county_df$subregion)


df$Clinton <- df$Clinton*100
df$Trump <- df$Trump*100

for (i in 1:length(df[,1])) {
  if (df$Clinton[i] > df$Trump[i]) {
    df$win[i] = 'Clinton'
  } else {
    df$win[i] = 'Trump'
  }
}

names(df) <- c("clinton", "trump", "subregion", "win")

choropleth <- inner_join(county_df, df, by = "subregion")
choropleth <- choropleth[!duplicated(choropleth$order), ]

p <- ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = win), 
               colour = alpha("white", 1/2), size = 0.1)  +
  geom_polygon(data = state_df, colour = "white", fill = NA) + 
  scale_fill_manual(values = c('blue','red')) +
  theme_void()

p <- ggplotly(p, tooltip = 'text') %>% 
  layout(
    hovermode = 'x',
    margin = list(
      t = 20,
      b = 20,
      l = 20,
      r = 20),
    legend = list(
      orientation = 'h',
      x = 0.5,
      y = 1.01,
      xanchor = 'center'))
p

# ------------------------------- CALIFORNIA COUNTIES ---------------------------------

library(tidyverse)
library(plotly)

df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/californiaPopulation.csv")

cali <- map_data("county") %>%
  filter(region == 'california')

pop <- df %>%
  group_by(County.Name) %>%
  summarise(Pop = sum(Population))

pop$County.Name <- tolower(pop$County.Name) # matching string

cali_pop <- merge(cali, pop, by.x = "subregion", by.y = "County.Name")

cali_pop$pop_cat <- cut(cali_pop$Pop, breaks = c(seq(0, 11000000, by = 500000)), labels=1:22)

# ON USA
geo <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

p <- cali_pop %>%
  group_by(group) %>%
  plot_geo(
    x = ~long, y = ~lat, color = ~pop_cat, colors = c('#ffeda0','#f03b20'),
    text = ~subregion, hoverinfo = 'text') %>%
  add_polygons(line = list(width = 0.4)) %>%
  add_polygons(
    fillcolor = 'transparent',
    line = list(color = 'black', width = 0.5),
    showlegend = FALSE, hoverinfo = 'none'
  ) %>%
  layout(
    title = "California Population by County",
    geo = geo)

# WITH MAPBOX
p <- cali_pop %>%
  group_by(group) %>%
  plot_mapbox(x = ~long, y = ~lat, color = ~pop_cat, colors = c('#ffeda0','#f03b20'),
              text = ~subregion, hoverinfo = 'text', showlegend = FALSE) %>%
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
      zoom = 4,
      center = list(lat = ~median(lat), lon = ~median(long))),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  )
p

library(highcharter)
data(unemployment)

hcmap("countries/us/us-all-all", data = unemployment,
      name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%")


mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))
data_fake <- mapdata %>% 
  select(code = `hc-a2`) %>% 
  mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))
hcmap("countries/us/us-all", data = data_fake, value = "value",
      joinBy = c("hc-a2", "code"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD")) 


