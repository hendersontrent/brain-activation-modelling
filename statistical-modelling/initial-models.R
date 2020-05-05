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

# GAM

model_reassure <- gam(mpfc_reassuring ~ s(t_fscrs_inad) + 
                        s(fears_of_expressing_compassion_to_self),
                          data = d)
summary(model_reassure)

# GLM

model_reassure_glm <- glm(mpfc_reassuring ~ t_fscrs_inad + 
                        fears_of_expressing_compassion_to_self,
                          data = d)
summary(model_reassure_glm)

# Model comparisons

sum(abs(residuals(model_reassure)))
sum(abs(residuals(model_reassure_glm)))

#---------------------
# RESPONSE: AI
#---------------------

# GAM

model_ai <- gam(ai_reassurance ~ s(t_fscrs_inad) + 
                  s(fears_of_expressing_compassion_to_self),
                    data = d)
summary(model_ai)

# GLM

model_ai_glm <- glm(ai_reassurance ~ t_fscrs_inad + 
                      fears_of_expressing_compassion_to_self,
                         data = d)
summary(model_ai_glm)

# Model comparisons

sum(abs(residuals(model_ai)))
sum(abs(residuals(model_ai_glm)))

#-----------------DATA VIS & OUTPUTS--------------


