
library(tidyverse)
library(tools)
library(scales)

pl <- read_csv("polar.csv")

pl <- gather(pl, key="party", value="pct", democrats, republicans)
pl$party <- toTitleCase(pl$party)

pl %>% subset(cat=="race") %>% 
  ggplot(aes(x=party, y=pct/100, fill=val)) +
  stat_summary(fun.y="sum", geom="bar", position = "dodge") + 
  facet_wrap(~year) + 
geom_text(aes(y = pct/100, label = round(pct,1)), 
          stat = "identity", position=position_dodge(width = .9), 
          vjust = 1.5, color="white")  +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() + theme(panel.grid.major.x = element_blank())

pl %>% subset(cat=="income") %>% 
  ggplot(aes(x=party, y=pct/100, fill=val)) +
  stat_summary(fun.y="sum", geom="bar", position = "dodge") + 
  facet_wrap(~year) + 
  geom_text(aes(y = pct/100, label = round(pct,1)), 
            stat = "identity", position=position_dodge(width = .9), 
            vjust = 1.5, color="white")  +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  theme_minimal() + theme(panel.grid.major.x = element_blank())
