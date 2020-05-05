#-----------------------------------------
# This script aims to produce initial
# statistical models of brain-self report
# linkage
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 5 May 2020
#-----------------------------------------

# Load data

d <- read.csv("data/Everything.csv") %>%
  clean_names()

# Keep in environment

if (!exists(keepers)) {
  keepers <- c("keepers", "d")
} else {
  keepers <- union(keepers, "d")
}

#-----------------PRE PROCESSING------------------



#-----------------STATISTICAL MODELLING-----------



#-----------------DATA VIS & OUTPUTS--------------


