
# Final Visualizations

# -------------------------------- set up ----------------------------------------

load("ej/ejscreen.RData")
load("data/climate-opinion.RData")
ej <- readRDS("data/ej.rds")

library(tidyverse)
library(highcharter)
library(scales)

# ------------------------- climate change beliefs ----------------------------------

# belief in climate change
hcmap("countries/us/us-all-all", data = ej,
      name = "Percent Believe in Climate Change", value = "happening", 
      joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 100, by = 10)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%")
ggplot(ej, aes(happening)) + geom_density()

## takeaway: belief in climate change is actually pretty consistent across the country
## in almost every county, more than 50% of people believe in climate change
# concerned about climate change

hcmap("countries/us/us-all-all", data = ej,
      name = "Percent Concerned about Climate Change", value = "worried", 
      joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 100, by = 10)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%")
ggplot(ej, aes(worried)) + geom_density()
ggplot(ej, aes(diff)) + geom_density()

## takeaway: worry is consistently lower than belief, by about 10% on average, 
## pretty consistently between 8-12 percent different across the country 

# climate consensus
hcmap("countries/us/us-all-all", data = ej,
      name = "Percent Believe in Climate Consensus", value = "consensus", 
      joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 100, by = 10)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%")

ej$sciencehaters <- ej$happening-ej$consensus
hcmap("countries/us/us-all-all", data = ej,
      name = "Believe in climate change but not scientific consensus", 
      value = "sciencehaters", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 100, by = 10)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%")

ggplot(ej, aes(sciencehaters)) + geom_density()

## takeaway: belief in climate consensus is much lower than belief in climate change
## between 12 and 30 percent of people in each county believe in climate change
## but don't believe in a scientific climate consensus

ev <- data.frame(belief=c("Belief in Climate Change", "Belief in Evolution"), 
                 value=c(.70, .57))

ggplot(ev, aes(x=belief, y=value, fill=belief)) + 
  stat_summary(fun.y="sum", geom="bar", width=0.7) + 
  coord_flip() + 
  scale_fill_manual(values=c("#1a6d1a", "#8ab780")) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(x="", y="% of Americans", 
       subtitle="Climate change beliefs actually high relative to other science beliefs", 
       title="Reasons for Hope", 
       caption="Climate opinion data from Yale Climate Opinion 2018, evolution belief data from Gallup 2017") + guides(fill=F) +
  annotate("text", x=1, y=.73, label="70") +
  annotate("text", x=2, y=.60, label="57")

concern <- data.frame(belief=c("Global warming is happening", "Worried about global warming", 
                          "Global warming will personally affect me"), 
                 value=c(.70, .61, .41))


ggplot(concern, aes(x=reorder(belief, value), y=value, fill=reorder(belief, -value))) + 
  stat_summary(fun.y="sum", geom="bar", width=0.7) + 
  coord_flip() + 
  #theme_minimal() + 
  scale_fill_manual(values=c("#1a6d1a", "#8ab780", "#cfeac8")) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(x="", y="% of Americans", 
       subtitle="Belief in global warming higher than concern and personal stake", 
       title="Belief doesn't always translate to concern", 
       caption="Climate opinion data from Yale Climate Opinion 2018") + guides(fill=F)+
  annotate("text", x=3, y=.73, label="70") +
  annotate("text", x=2, y=.64, label="61")+
  annotate("text", x=1, y=.44, label="41")

consensus <- data.frame(belief=c("Belief in Climate Change", "Belief in Scientific Climate Consensus"), 
                 value=c(.70, .49))

ggplot(consensus, aes(x=belief, y=value, fill=belief)) + 
  stat_summary(fun.y="sum", geom="bar", width=0.7) + 
  coord_flip() + 
  scale_fill_manual(values=c("#1a6d1a", "#8ab780")) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(x="", y="% of Americans", 
       subtitle="21% of Americans believe in climate change, but not scientific consensus", 
       title="Science Skeptics", 
       caption="Climate opinion data from Yale Climate Opinion 2018") + guides(fill=F) +
  annotate("text", x=1, y=.73, label="70") +
  annotate("text", x=2, y=.52, label="49")

# ---------------------------- climate impacts -------------------------------------

# economic damages
hcmap("countries/us/us-all-all", data = ej,
      name = "Economic Damages as a % of County Income", 
      value = "totaldamagescountyincome", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(-15,30, by = 5)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>% 
  hc_title(text="Economic Damages as a % of County Income")

hcmap("countries/us/us-all-all", data = ej,
      name = "Economic Damages as a % of County Income", 
      value = "totaldamagescountyincome", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(-15,0,30))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>% 
  hc_title(text="Economic Damages as a % of County Income")

stops = color_stops(n = 10, colors = c("#6da458","#90b87e","#b2cda5","#d4e2cd","#f6f6f6",
                                       "#f8d5d5","#f6b3b5","#f09096","#e86c78","#de425b"))
hcmap("countries/us/us-all-all", data = ej,
      name = "Economic Damages as a % of County Income", 
      value = "totaldamagescountyincome", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(stops=stops) %>% 
  hc_title(text="Economic Damages as a % of County Income")




## takeaway: the southeast, even inland, will be disproportionality affected by 
## climate change, some areas will benefit economically from climate change as 
## agriculture moves

# ------------------------------ demographics --------------------------------------

# percent minority
hcmap("countries/us/us-all-all", data = ej,
      name = "Percent Minority", value = "pctmin", 
      joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0,100, by = 20)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 

stops = color_stops(n = 5, colors = c("#e9ffff", "#004c6d"))
hcmap("countries/us/us-all-all", data = ej,
      name = "Percent Minority", value = "pctmin", 
      joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(stops=stops)

#004c6d
#42768e
#76a3b1
#aed0d6
#e9ffff

## takeaway: counties with a high percent of racial and ethnic minorities will be
## disproportionally impacted by climate change

# percent low income
hcmap("countries/us/us-all-all", data = ej,
      name = "Percent Low Income", value = "pctlowinc", 
      joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0,100, by = 20)))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%")

## takeaway: low income communities will be disproportionally affected by 
## climate change


  



