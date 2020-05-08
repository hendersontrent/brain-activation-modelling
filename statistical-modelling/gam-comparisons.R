#-----------------------------------------
# This script aims to produce model
# selections between GAM models
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 8 May 2020
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

# With stress

amyg_re_gam_full <- gam(amygdala_reassurance ~ s(s_dass_stress) + s(t_fscrs_inad) + 
                     s(fears_of_expressing_compassion_to_self),
                   data = d)
summary(amyg_re_gam_full)

# Without stress

amyg_re_gam_pars <- gam(amygdala_reassurance ~ s(t_fscrs_inad) + 
                     s(fears_of_expressing_compassion_to_self),
                   data = d)
summary(amyg_re_gam_pars)

# Model comparisons

sum(abs(residuals(amyg_re_gam_full)))
sum(abs(residuals(amyg_re_gam_pars)))

AIC(amyg_re_gam_full)
AIC(amyg_re_gam_pars)

summary(amyg_re_gam_full)$dev.expl
summary(amyg_re_gam_pars)$dev.expl

# Plots of better fitting model

p <- visreg(amyg_re_gam_full, "s_dass_stress", gg = TRUE, ylab = "Amygdala reassurance")
p1 <- visreg(amyg_re_gam_full, "t_fscrs_inad", gg = TRUE, ylab = "Amygdala reassurance")
p2 <- visreg(amyg_re_gam_full, "fears_of_expressing_compassion_to_self", gg = TRUE, 
       ylab = "Amygdala reassurance")

ggarrange(p,p1,p2, ncol = 2, nrow = 2)

#-------------------------------
# RESPONSE: MPFC reassurance
#-------------------------------

# With stress

mpfc_re_gam_full <- gam(mpfc_reassuring ~ s(s_dass_stress) + s(t_fscrs_inad) + 
                     s(fears_of_expressing_compassion_to_self),
                   data = d)
summary(mpfc_re_gam_full)

# Without stress

mpfc_re_gam_pars <- gam(mpfc_reassuring ~ s(t_fscrs_inad) + 
                          s(fears_of_expressing_compassion_to_self),
                        data = d)
summary(mpfc_re_gam_pars)

# Model comparisons

sum(abs(residuals(mpfc_re_gam_full)))
sum(abs(residuals(mpfc_re_gam_pars)))

AIC(mpfc_re_gam_full)
AIC(mpfc_re_gam_pars)

summary(mpfc_re_gam_full)$dev.expl
summary(mpfc_re_gam_pars)$dev.expl

# Plot best fitting model

p3 <- visreg(mpfc_re_gam_pars, "t_fscrs_inad", gg = TRUE, ylab = "MPFC reassurance")
p4 <- visreg(mpfc_re_gam_pars, "fears_of_expressing_compassion_to_self", gg = TRUE, 
       ylab = "MPFC reassurance")

ggarrange(p3,p4, nrow = 2)

#-------------------------------
# RESPONSE: AI reassurance
#-------------------------------

# With stress

ai_re_gam_full <- gam(ai_reassurance ~ s(s_dass_stress) + s(t_fscrs_inad) + 
                   s(fears_of_expressing_compassion_to_self),
                 data = d)
summary(ai_re_gam_full)

# Without stress

ai_re_gam_pars <- gam(ai_reassurance ~ s(t_fscrs_inad) + 
                   s(fears_of_expressing_compassion_to_self),
                 data = d)
summary(ai_re_gam_pars)

# Model comparisons

sum(abs(residuals(ai_re_gam_full)))
sum(abs(residuals(ai_re_gam_pars)))

AIC(ai_re_gam_full)
AIC(ai_re_gam_pars)

summary(ai_re_gam_full)$dev.expl
summary(ai_re_gam_pars)$dev.expl

# Plot best fitting model

p5 <- visreg(ai_re_gam_pars, "t_fscrs_inad", gg = TRUE, ylab = "AI reassurance")
p6 <- visreg(ai_re_gam_pars, "fears_of_expressing_compassion_to_self", gg = TRUE, 
       ylab = "AI reassurance")

ggarrange(p5,p6, nrow = 2)
