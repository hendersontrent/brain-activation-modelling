#-----------------------------------------
# This script aims to produce initial
# statistical models of brain-self report
# linkage using Jeff's initial charts
# as a reference
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 5 May 2020
#-----------------------------------------

# Load data

d <- read.csv("data/correlation figure.csv") %>%
  clean_names() %>%
  drop_na()

# Keep in environment

if (!exists(keepers)) {
  keepers <- c("keepers", "d")
} else {
  keepers <- union(keepers, "d")
}

#-----------------STATISTICAL MODELLING-----------

#---------------------
# RESPONSE: Reassuring
#---------------------

model_reassure <- gam(mpfc_reassuring ~ s(t_fscrs_inad) + 
                        s(fears_of_expressing_compassion_to_self),
             data = d)
summary(model_reassure)

#---------------------
# RESPONSE: AI
#---------------------

model_ai <- gam(ai_reassurance ~ s(t_fscrs_inad) + 
                  s(fears_of_expressing_compassion_to_self),
                      data = d)
summary(model_ai)

#-----------------DATA VIS & OUTPUTS--------------


