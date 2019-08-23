

# • Weighted percentages among registered voters of each of the groups discussed in this report:
#   Ø Democrats (total) including leaners: 48%
# o Liberal Democrats: 29%
# o Moderate/ConservativeDemocrats:19%
# § (Moderate Democrats: 18%; Conservative Democrats: 1%)
# Ø Independents excluding leaners: 10%
# Ø Republicans (total) including leaners: 36% o Liberal/ModerateRepublicans:12%
#   § (Liberal Republicans: 1%; Moderate Republicans: 11%) o ConservativeRepublicans:23%
# Ø No party/Not interested in politics/Refused: 6% (included in results reported for "All Registered Voters" only)

# december 2018 numbers
library(tidyverse)
library(scales)

climate <- data.frame(party=c("Liberal Dem", "Moderate/Conservative Dem", 
                   "Liberal/Moderate Rep", "Conservative Rep", "All Registered Voters"), 
           happening=c(.98, .85, .70, .42, .74), 
           humancaused=c(.90, .66, .53, .28, .62),
           worried=c(.95, .80, .54, .32, .67))

climate$party <- factor(climate$party, levels=c("All Registered Voters", "Liberal Dem", "Moderate/Conservative Dem", 
                                                "Liberal/Moderate Rep", "Conservative Rep"))

ggplot(climate, aes(x=party, y=happening)) + 
  stat_summary(fun.y="sum", geom="bar", width=0.75) + 
  theme_minimal() + scale_y_continuous(labels=percent, limits = c(0,1.05)) + 
  labs(x="",y="") +
  geom_text(aes(x=party, y=happening, label=100*happening), vjust=-0.5) 


ggplot(climate, aes(x=party, y=humancaused)) + 
  stat_summary(fun.y="sum", geom="bar", width=0.75) + 
  theme_minimal() + scale_y_continuous(labels=percent, limits = c(0,1.05)) + 
  labs(x="",y="") +
  geom_text(aes(x=party, y=humancaused, label=100*humancaused), vjust=-0.5) 

ggplot(climate, aes(x=party, y=worried)) + 
  stat_summary(fun.y="sum", geom="bar", width=0.75) + 
  theme_minimal() + scale_y_continuous(labels=percent, limits = c(0,1.05)) + 
  geom_text(aes(x=party, y=worried, label=100*worried), vjust=-0.5) 


climate2 <- gather(data=climate, key="question", value="perc", happening, humancaused, worried)

ggplot(climate2, aes(x=party, y=perc, fill=question)) + 
  stat_summary(fun.y="sum", geom="bar", width=0.75, position="dodge")+
  geom_text(aes(x=party, y=perc, label=100*perc), vjust=-0.5, position=position_dodge(width = 0.7)) +
  labs(x="",y="")+ scale_y_continuous(labels=percent, limits = c(0,1.05)) + theme_minimal()

