### This is my first script. I am learning how to import data ####
### Created by: Zoe Sidana Bunnath ####
### Created on: 2025-09-08 ####


### libraries #####
library(tidyverse)
library(here)


### Read in my data #####
weightdata<-read.csv(here("Week_02","Data","weightdata.csv"))


### Analysis #####
head(weightdata)
tail(weightdata) ## looks at the bottom 6 lines
view(weightdata)
