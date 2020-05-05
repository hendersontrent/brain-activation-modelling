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
library(mgcv)
library(ggpubr)
library(data.table)
library(Cairo)

# Turn off scientific notation

options(scipen = 999)
