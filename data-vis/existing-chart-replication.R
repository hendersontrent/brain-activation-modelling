#-----------------------------------------
# This script aims to reproduce Jeff's 
# initial plots but using GAM instead of
# LOESS
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

CairoPNG("output/replication-charts.png", 800, 600)
ggarrange(p, p1, p2,
          ncol = 2, nrow = 2)
dev.off()
