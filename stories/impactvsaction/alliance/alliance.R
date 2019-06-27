

# C. McClintock
# Brightest Research
# Yale Climate Change Opinion Maps

# -------------------------------- set up ----------------------------------------

# load libraries
library(tidyverse)
library(usmap)
library(maps)
library(mapdata)

# read in the data
al <- read_csv("alliance.csv")

# ------------------------- build climate -------------------------------

# states in climate alliance
plot_usmap(data = al, values = "alliance", lines = "white") + 
  guides(fill=F)

