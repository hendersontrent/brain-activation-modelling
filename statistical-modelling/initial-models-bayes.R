#-----------------------------------------
# This script aims to produce Bayesian
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

#-----------------BAYESIAN MODELLING---------

model <- MCMCregress(mpfc_reassuring ~ 1 + t_fscrs_inad + fears_of_expressing_compassion_to_self,
                    data = d, burnin = 1000, mcmc = 10000, thin = 1,
                    b0 = c(0,0,0),
                    B0 = c(0,0,0),
                    c0 = 0.001,
                    d0 = 0.001)

#-----------------MODEL OUTPUTS--------------

par(mfrow = c(1,1))
plot(model) # Bayesian trace and density plots
HPDinterval(model) # Highest posterior density intervals
summary(model) # Standard model summary

