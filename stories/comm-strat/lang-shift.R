
library(tidyverse)
library(lubridate)

shift <- read_csv("gw-cc-shift.csv")
shift_us <- read_csv("gw-cc-shift-us.csv")

shift <- gather(shift, key="term", value="search_freq", cc, gw)

shift_us <- gather(shift_us, key="term", value="search_freq", cc, gw)

shift$d <- "01"
shift <- unite(shift, month, d, col="date", sep="-")
shift$date <- ymd(shift$date)

shift_us$d <- "01"
shift_us <- unite(shift_us, month, d, col="date", sep="-")
shift_us$date <- ymd(shift_us$date)

ggplot(shift, aes(x=date, y=search_freq, color=term, group = term)) + 
  geom_line() + theme_minimal()

ggplot(shift_us, aes(x=date, y=search_freq, color=term, group = term)) + 
  geom_line() + theme_minimal() + 
  scale_color_manual(values=c("#049133", "#93a400")) +
  labs(x="", y="")
