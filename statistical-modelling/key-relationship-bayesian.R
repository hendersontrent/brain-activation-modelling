#-----------------------------------------
# This script aims to produce Bayesian
# GLMs for a few key relationships of
# self-report to brain data
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 7 May 2020
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

# Specify model

amyg_re_bayes <- stan_glm(amygdala_reassurance ~ s_dass_stress + t_fscrs_inad + 
                            fears_of_expressing_compassion_to_self,
                          data = d,
                          family = gaussian(),
                          seed = 123) # Uses default weakly informative priors

prior_summary(amyg_re_bayes) # Point estimates here are posterior medians

# Plots

posterior <- as.matrix(amyg_re_bayes)

mcmc_areas(posterior,
           pars = c("s_dass_stress", "t_fscrs_inad", "fears_of_expressing_compassion_to_self"),
           prob = 0.8) +
  labs(title = "Amygdala reassurance posterior distributions",
       subtitle = "Includes medians and 80% intervals")

ppc_dens_overlay(y = amyg_re_bayes$y,
                 yrep = posterior_predict(fit, draws = 100)) +
  labs(title = "Amygdala reassurance posterior predictions",
       subtitle = "Draws = 100")

# Return Bayesian uncertainty intervals

amyg_re_bayes_ci <- posterior_interval(amyg_re_bayes, prob = 0.95,
                                       pars = c("s_dass_stress", "t_fscrs_inad", 
                                                "fears_of_expressing_compassion_to_self"))

#-------------------------------
# RESPONSE: MPFC reassurance
#-------------------------------



#-------------------------------
# RESPONSE: AI reassurance
#-------------------------------


