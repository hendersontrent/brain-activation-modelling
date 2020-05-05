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

set.seed(123)
model <- MCMCregress(mpfc_reassuring ~ 1 + t_fscrs_inad + fears_of_expressing_compassion_to_self,
                    data = d, burnin = 1000, mcmc = 10000, thin = 1,
                    b0 = c(0,0,0), # Prior mean
                    B0 = c(0,0,0)) # Prior precision

#-----------------MODEL OUTPUTS--------------

par(mfrow = c(1,1))
plot(model) # Bayesian trace and density plots
HPDinterval(model) # Highest posterior density intervals
summary(model) # Standard model summary
effectiveSize(model) # Check number of effective iterations is close to MCMC hyperparameter

