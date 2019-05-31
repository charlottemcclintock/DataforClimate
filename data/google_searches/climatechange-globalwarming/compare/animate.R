
library(tidyverse)

temp <- list.files(pattern="*.csv")
comp <- lapply(temp, function(x) read_csv(x, skip=3, 
        col_names = c("country", "climatechange", "globalwarming"))) %>% 
  bind_rows()

comp$year <- rep(seq(2004, 2019, 1), rep(250, 16))

comp$climatechange <- with(comp, ifelse(is.na(climatechange), 0, climatechange))
comp$globalwarming <- with(comp, ifelse(is.na(globalwarming), 0, climatechange))


comp$climatechange <- str_replace_all(comp$climatechange, "[^[:alnum:]]", "")
comp$globalwarming <- str_replace_all(comp$globalwarming, "[^[:alnum:]]", "")

write_csv(comp, "compare2004-2019.csv")

names(df) <- str_to_lower(names(df))
df <- select(df, country, code)
comp <- left_join(comp, df, by=("country"))

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'equirectangular')
)

p <- plot_geo(comp) %>%
  add_trace(
    z = ~climatechange, color = ~climatechange, colors = 'Blues',
    text = ~country, locations = ~country, marker = list(line = l)
  ) %>%
  layout(
    geo = g
  )
p