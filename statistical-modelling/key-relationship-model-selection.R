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
# RESPONSE: Amygdala criticism
#-------------------------------

# GAM

amyg_crit_gam <- gam(amygdala_criticism ~ s(s_dass_stress) + s(t_fscrs_inad) + s(t_neuroticism),
                data = d)
summary(amyg_crit_gam)

# GLM

amyg_crit_glm <- glm(amygdala_criticism ~ s_dass_stress + t_fscrs_inad + t_neuroticism,
                     data = d)
summary(amyg_crit_glm)

sum(abs(residuals(amyg_crit_gam)))
sum(abs(residuals(amyg_crit_glm)))

# Bayes

set.seed(123)
amyg_crit_bayes <- MCMCregress(amygdala_criticism ~ 1 + s_dass_stress + t_fscrs_inad + t_neuroticism,
                     data = d, burnin = 1000, mcmc = 10000, thin = 1)

par(mfrow = c(1,1))
plot(amyg_crit_bayes) # Bayesian trace and density plots
HPDinterval(amyg_crit_bayes) # Highest posterior density intervals
summary(amyg_crit_bayes)

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
# RESPONSE: MPFC criticism 
#-------------------------------

# GAM

mpfc_crit_gam <- gam(mpfc_crit ~ s(s_dass_stress) + s(t_fscrs_inad) + s(t_neuroticism),
                     data = d)
summary(mpfc_crit_gam)

# GLM

mpfc_crit_glm <- glm(mpfc_crit ~ s_dass_stress + t_fscrs_inad + t_neuroticism,
                   data = d)
summary(mpfc_crit_glm)

sum(abs(residuals(mpfc_crit_gam)))
sum(abs(residuals(mpfc_crit_glm)))

# Bayes

set.seed(123)
mpfc_crit_bayes <- MCMCregress(mpfc_crit ~ 1 + s_dass_stress + t_fscrs_inad + t_neuroticism,
                             data = d, burnin = 1000, mcmc = 10000, thin = 1)

par(mfrow = c(1,1))
plot(mpfc_crit_bayes) # Bayesian trace and density plots
HPDinterval(mpfc_crit_bayes) # Highest posterior density intervals
summary(mpfc_crit_bayes)

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

#-------------------------------
# RESPONSE: PCC criticism
#-------------------------------

# GAM

pcc_crit_gam <- gam(crit_pcc ~ s(s_dass_stress) + s(t_fscrs_inad) + s(t_neuroticism),
                    data = d)
summary(pcc_crit_gam)

# GLM

pcc_crit_glm <- glm(crit_pcc ~ s_dass_stress + t_fscrs_inad + t_neuroticism,
                     data = d)
summary(pcc_crit_glm)

sum(abs(residuals(pcc_crit_gam)))
sum(abs(residuals(pcc_crit_glm)))

# Bayes

set.seed(123)
pcc_crit_bayes <- MCMCregress(crit_pcc ~ 1 + s_dass_stress + t_fscrs_inad + t_neuroticism,
                           data = d, burnin = 1000, mcmc = 10000, thin = 1)

par(mfrow = c(1,1))
plot(pcc_crit_bayes) # Bayesian trace and density plots
HPDinterval(pcc_crit_bayes) # Highest posterior density intervals
summary(pcc_crit_bayes)

