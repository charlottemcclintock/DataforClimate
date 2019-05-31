
# Mapping
# Yale Climate Change Opinion Maps

# -------------------------------- set up ----------------------------------------

# set working directory
setwd("~/Projects/Active/QuadrantQuestion/yale-climate")

# load libraries
library(plotly)
library(tidyverse)
library(maps)

# read in data
load("climate-opinion.RData")

# ----------------------- exploratory visualization -------------------------------

# check distributions of key variables
ggplot(damages, aes(totaldamagescountyincome)) + geom_density()
ggplot(damages, aes(happening)) + geom_density()
ggplot(damages, aes(worried)) + geom_density()
ggplot(damages, aes(diff)) + geom_density()

