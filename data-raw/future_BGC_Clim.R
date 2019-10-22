# Script to integrate future climate into bgcClim.rda

library(devtools)
library(dplyr)
library(magrittr)

# Load future data from csv
future<-read.csv("data-raw/ClimateSummary_Future_v11_5.6.csv")

# Load current data from .rda file
load("./data/bgcClim.rda")

# Create scenario column in current file
bgcClim %<>%
  mutate(Scenario=NA) %>%
  dplyr::select(BGC,period,Scenario,everything())

# Make sure that column names in bgcClim and future match
identical(names(bgcClim),names(future))

# Merge dataframes
  bgcClim %>%
    rbind(future) %>%

# Select only columns that are relevant to asmr
    dplyr::select(BGC,period,Scenario,Var,starts_with("Tmax"),starts_with("Tmin"),starts_with("PPT")) %>%

# Select only rows that are relevant to asmr
  filter(Var=="mean") %>%

# Get rid of seasonal columns
  dplyr::select(-Var,-contains("_"))->

# Assign output to X
  X

# Finally, resave to bgcClim
  bgcClim=X

# save()
  save(bgcClim,file="./data/bgcClim.rda")
