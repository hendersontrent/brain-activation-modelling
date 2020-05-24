#-----------------------------------------
# This script aims to produce additional
# analysis for the paper
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 24 May 2020
#-----------------------------------------

# Load data

d <- read.csv("data/Everything.csv") %>%
  clean_names() %>%
  drop_na()

#-----------------MODELLING------------------

# Model builds

model <- gam(mpfc_reassuring ~ s(t_fscrs_hate),
             data = d)

model1 <- gam(mpfc_reassuring ~ s(t_fscrs_reass),
              data = d)

model2 <- gam(ai_reassurance ~ s(t_fscrs_hate),
              data = d)

model3 <- gam(ai_reassurance ~ s(t_fscrs_reass),
               data = d)

#-----------------SUMMARIES -----------------

summary(model1)
summary(model3)
