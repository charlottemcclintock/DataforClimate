
# Cleaning Script
# Political Engagement Data

# -------------------------------- set up ----------------------------------------

# load libraries
library(tidyverse)
library(maps)
library(tools)

# read in data
load("data/elections/countypres_2000-2016.RData")

res <- x