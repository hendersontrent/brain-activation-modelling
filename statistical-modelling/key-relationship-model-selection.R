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

amyg_re_gam <- gam(amygdala_reassurance ~ s(s_dass_stress) + s(t_fscrs_inad) + s(t_neuroticism),
                   data = d)
summary(amyg_re_gam)

# GLM

amyg_re_glm <- glm(amygdala_reassurance ~ s_dass_stress + t_fscrs_inad + t_neuroticism,
                   data = d)
summary(amyg_re_glm)

sum(abs(residuals(amyg_re_gam)))
sum(abs(residuals(amyg_re_glm)))

# Bayes

set.seed(123)
amyg_re_bayes <- MCMCregress(amygdala_reassurance ~ 1 + s_dass_stress + t_fscrs_inad + t_neuroticism,
                               data = d, burnin = 1000, mcmc = 10000, thin = 1)

par(mfrow = c(1,1))
plot(amyg_re_bayes) # Bayesian trace and density plots
HPDinterval(amyg_re_bayes) # Highest posterior density intervals
summary(amyg_re_bayes)

#-------------------------------
# RESPONSE: MPFC reassurance
#-------------------------------

# GAM

mpfc_re_gam <- gam(mpfc_reassuring ~ s(s_dass_stress) + s(t_fscrs_inad) + s(t_neuroticism),
                   data = d)
summary(mpfc_re_gam)

# GLM

mpfc_re_glm <- glm(mpfc_reassuring ~ s_dass_stress + t_fscrs_inad + t_neuroticism,
                     data = d)
summary(mpfc_re_glm)

sum(abs(residuals(mpfc_re_gam)))
sum(abs(residuals(mpfc_re_glm)))

# Bayes

set.seed(123)
mpfc_re_bayes <- MCMCregress(mpfc_reassuring ~ 1 + s_dass_stress + t_fscrs_inad + t_neuroticism,
                               data = d, burnin = 1000, mcmc = 10000, thin = 1)

par(mfrow = c(1,1))
plot(mpfc_re_bayes) # Bayesian trace and density plots
HPDinterval(mpfc_re_bayes) # Highest posterior density intervals
summary(mpfc_re_bayes)

