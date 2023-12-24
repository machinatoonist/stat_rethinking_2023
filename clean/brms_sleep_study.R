library(lme4)
library(brms)
library(ggplot2)
library(dplyr)

data("sleepstudy")
d = sleepstudy

d %>% glimpse()

# This model is ignoring structure in the data
fit_sleep1 = brm(Reaction ~ Days, data = d)

conditional_effects(fit_sleep1)

d %>% 
    ggplot(aes(x = Days, y = Reaction)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal()

# Overall effect of days and intercept and varying intercept and varying effect of days
# by Subject.  
fit_sleep2 = brm(Reaction ~ 1 + Days + (1 + Days | Subject), data = d)

# The uncertainty of the effect has widened
conditional_effects(fit_sleep2)

# Empirical cumulative distribution function
pp_check(fit_sleep1, type = "ecdf_overlay")
pp_check(fit_sleep2, type = "ecdf_overlay")
