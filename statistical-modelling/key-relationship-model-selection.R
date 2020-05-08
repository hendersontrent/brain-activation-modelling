#-----------------------------------------
# This script aims to produce model
# selections between GAM and GLM models
# for a few key relationships
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
  clean_names()

# Keep in environment

if (!exists(keepers)) {
  keepers <- c("keepers", "d")
} else {
  keepers <- union(keepers, "d")
}

#-----------------STATISTICAL MODELLING---------

#-------------------------------
# RESPONSE: Amygdala reassurance
#-------------------------------

# GAM

amyg_re_gam <- gam(amygdala_reassurance ~ s(s_dass_stress) + s(t_fscrs_inad) + 
                     s(fears_of_expressing_compassion_to_self),
                   data = d)
summary(amyg_re_gam)

visreg(amyg_re_gam, "s_dass_stress", gg = TRUE, ylab = "Amygdala reassurance")
visreg(amyg_re_gam, "t_fscrs_inad", gg = TRUE, ylab = "Amygdala reassurance")
visreg(amyg_re_gam, "fears_of_expressing_compassion_to_self", gg = TRUE, 
       ylab = "Amygdala reassurance")

# GLM

amyg_re_glm <- glm(amygdala_reassurance ~ s_dass_stress + t_fscrs_inad + 
                     fears_of_expressing_compassion_to_self,
                   data = d)
summary(amyg_re_glm)

sum(abs(residuals(amyg_re_gam)))
sum(abs(residuals(amyg_re_glm)))

#-------------------------------
# RESPONSE: MPFC reassurance
#-------------------------------

# GAM

mpfc_re_gam <- gam(mpfc_reassuring ~ s(s_dass_stress) + s(t_fscrs_inad) + 
                     s(fears_of_expressing_compassion_to_self),
                   data = d)
summary(mpfc_re_gam)

visreg(mpfc_re_gam, "s_dass_stress", gg = TRUE, ylab = "MPFC reassurance")
visreg(mpfc_re_gam, "t_fscrs_inad", gg = TRUE, ylab = "MPFC reassurance")
visreg(mpfc_re_gam, "fears_of_expressing_compassion_to_self", gg = TRUE, 
       ylab = "MPFC reassurance")

# GLM

mpfc_re_glm <- glm(mpfc_reassuring ~ s_dass_stress + t_fscrs_inad + 
                     fears_of_expressing_compassion_to_self,
                     data = d)
summary(mpfc_re_glm)

sum(abs(residuals(mpfc_re_gam)))
sum(abs(residuals(mpfc_re_glm)))

#-------------------------------
# RESPONSE: AI reassurance
#-------------------------------

# GAM

ai_re_gam <- gam(ai_reassurance ~ s(s_dass_stress) + s(t_fscrs_inad) + 
                    s(fears_of_expressing_compassion_to_self),
                  data = d)
summary(acc_re_gam)

visreg(ai_re_gam, "s_dass_stress", gg = TRUE, ylab = "AI reassurance")
visreg(ai_re_gam, "t_fscrs_inad", gg = TRUE, ylab = "AI reassurance")
visreg(ai_re_gam, "fears_of_expressing_compassion_to_self", gg = TRUE, 
       ylab = "AI reassurance")

# GLM

ai_re_glm <- glm(ai_reassurance ~ s_dass_stress + t_fscrs_inad + 
                    fears_of_expressing_compassion_to_self,
                  data = d)
summary(ai_re_glm)

sum(abs(residuals(ai_re_gam)))
sum(abs(residuals(ai_re_glm)))
