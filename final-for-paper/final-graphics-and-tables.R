#-----------------------------------------
# This script aims to produce the final
# graphics for the paper
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 13 May 2020
#-----------------------------------------

# Load data

d <- read.csv("data/correlation figure.csv") %>%
  clean_names() %>%
  drop_na()

#-----------------HIGH LEVEL PLOTS----------------

# MPFC ~ Fears

p <- d %>%
  ggplot(aes(x = fears_of_expressing_compassion_to_self, y = mpfc_reassuring)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "#D53E4F") +
  geom_smooth(formula = y ~ s(x), method = "gam") +
  geom_point(size = 2) +
  labs(title = "Fears Express Compassion Self and MPFC",
       x = "Fears",
       y = "% Signal Change MPFC") +
  scale_x_continuous(limits = c(1,4),
                     breaks = seq(from = 1, to = 4, by = 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p)

# MPFC ~ Inadequate Forms

p1 <- d %>%
  ggplot(aes(x = t_fscrs_inad, y = mpfc_reassuring)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "#D53E4F") +
  geom_smooth(formula = y ~ s(x), method = "gam") +
  geom_point(size = 2) +
  labs(title = "Inadequate Forms Criticism and MPFC",
       x = "Forms",
       y = "% Signal Change MPFC") +
  scale_x_continuous(limits = c(1,5),
                     breaks = seq(from = 1, to = 5, by = 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p1)

# AI ~ Inadequate Forms

p2 <- d %>%
  ggplot(aes(x = t_fscrs_inad, y = ai_reassurance)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "#D53E4F") +
  geom_smooth(formula = y ~ s(x), method = "gam") +
  geom_point(size = 2) +
  labs(title = "Inadequate Forms Criticism and AI",
       x = "Forms",
       y = "% Signal Change MPFC") +
  scale_x_continuous(limits = c(1,5),
                     breaks = seq(from = 1, to = 5, by = 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
print(p2)

#-----------------OUTPUTS--------------------

CairoPNG("output/final-graphic", 800, 600)
ggarrange(p, p1, p2, brain_plot,
          ncol = 2, nrow = 2)
dev.off()

#-----------------MODELLING------------------

# Model builds

model <- gam(mpfc_reassuring ~ s(fears_of_expressing_compassion_to_self),
             data = d)

model1 <- gam(mpfc_reassuring ~ s(t_fscrs_inad),
              data = d)

model2 <- gam(ai_reassurance ~ s(t_fscrs_inad),
              data = d)

# Model statistics

m_results <- tidy(model) %>%
  mutate(model = "MPFC ~ Fears")

m1_results <- tidy(model1) %>%
  mutate(model = "MPFC ~ Forms")

m2_results <- tidy(model2) %>%
  mutate(model = "AI ~ Fears")

the_results <- bind_rows(m_results, m1_results, m2_results) %>%
  dplyr::select(6,1,2,3,4,5)

# Model fits

m_fits <- glance(model) %>%
  mutate(model = "MPFC ~ Fears")

m1_fits <- glance(model1) %>%
  mutate(model = "MPFC ~ Forms")

m2_fits <- glance(model2) %>%
  mutate(model = "AI ~ Fears")

fits_results <- bind_rows(m_fits, m1_fits, m2_fits) %>%
  dplyr::select(7,1,2,3,4,5,6)

results_table <- flextable(the_results)
fits_table <- flextable(fits_results)

# Outputs

print(results_table)
print(fits_table)
