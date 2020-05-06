#-----------------------------------------
# This script aims to produce model
# selections between GAM, GLM and Bayesian
# regression for a few key relationships
# of self-report to brain data
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 6 May 2020
#-----------------------------------------

# Load data

d <- read.csv("data/Everything.csv") %>%
  clean_names() %>%
  drop_na()

# Keep in environment

if (!exists(keepers)) {
  keepers <- c("keepers", "d")
} else {
  keepers <- union(keepers, "d")
}

#-----------------STATISTICAL MODELLING---------

#-------------
#
#-------------



#-------------
#
#-------------



#-------------
#
#-------------



#-------------
#
#-------------



#-------------
#
#-------------


