#-----------------------------------------
# This script aims to produce the packages
# and reusables needed to run the project
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 5 May 2020
#-----------------------------------------

# Load packages

library(tidyverse)
library(readxl)
library(janitor)
library(data.table)
library(mgcv)
library(broom)
library(ggpubr)
library(Cairo)
library(MCMCpack)
library(arm)

# Turn off scientific notation

options(scipen = 999)

# Load functions that are useful

keepers <- c("keepers")

source("R/cleanup_env.R")

if (!exists(keepers)) {
  keepers <- c("keepers", "cleanup_env")
} else {
  keepers <- union(keepers, "cleanup_env")
}
