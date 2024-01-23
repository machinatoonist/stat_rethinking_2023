# Load packages ----

library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)

options(mc.cores = parallel::detectCores())

# Load data ----
data("cherry_blossom_sample")
running = cherry_blossom_sample

running %>% glimpse()
running %>% filter(runner == 1)

# Remove NAs
running = running %>% 
    select(runner, age, net) %>% 
    na.omit()

# Complete pooled model with predictor ----
# The incorrect approach to modeling the relationship between age and running time
# is to build a complete pooled model
complete_pooled_model = stan_glm(
    net ~ age,
    data = running, family = gaussian,
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735
)

# Posterior summary statistics
model_summary = tidy(complete_pooled_model, 
                     conf.int = TRUE, 
                     conf.level = 0.80)

model_summary

# Posterior median model
B0 = model_summary$estimate[1]
B1 = model_summary$estimate[2]

running %>% 
    ggplot(aes(x = age, y = net)) +
    geom_point() +
    geom_abline(aes(intercept = B0, slope = B1))

# Hierarchical random intercepts model with predictor ----
running_model_1_prior = stan_glmer(
    net ~ age + (1 | runner),
    data = running, family = gaussian,
    prior_intercept = normal(100, 10),
    prior = normal(2.5, 1),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735,
    prior_PD = TRUE
)

set.seed(84735)
updated_data <- running %>% 
    add_epred_draws(object = running_model_1_prior, 
                    ndraws = 4)

# View the structure of the updated data
str(updated_data)
names(updated_data)

ggplot(updated_data, aes(x = age, y = net)) +
    geom_line(aes(y = .epred, group = paste(runner, .draw))) +
    facet_wrap(~ .draw)

running %>% 
    ggplot(aes(x = age, y = net)) +
    geom_point() +
    facet_wrap(~ runner)

# > Update an rstanarm model ----
# Simulate the posterior
running_model_1 = update(running_model_1_prior, prior_PD = FALSE)

# Check the prior specifications
prior_summary(running_model_1)

# Markov chain diagnostics
mcmc_trace(running_model_1)
mcmc_dens_overlay(running_model_1)
mcmc_acf(running_model_1)
neff_ratio(running_model_1)
rhat(running_model_1)

# > Posterior analysis of global relationships ----
tidy_summary_1 = tidy(running_model_1,
                      effects = "fixed",
                      conf.int = TRUE,
                      conf.level = 0.80)

tidy_summary_1

# The typical runner has an 80% chance of of slowing down by between
# 1.02 and 1.58 minutes per year

B0 = tidy_summary_1$estimate[1]
B1 = tidy_summary_1$estimate[2]

running %>% 
    add_fitted_draws(running_model_1, n = 200, re_formula = NA) %>% 
    ggplot(aes(x = age, y = net)) +
    geom_line(aes(y = .value, group = .draw), alpha = 0.1) +
    geom_abline(intercept = B0, slope = B1, color = "blue") +
    lims(y = c(75, 110))

posterior_draws = running %>% 
    add_epred_draws(object = running_model_1, ndraws = 200, re_formula = NA)

posterior_draws %>% glimpse()

posterior_draws %>% 
    ggplot(aes(x = age, y = net)) +
    geom_line(aes(y = .epred, group = .draw), alpha = 0.1) +
    geom_abline(intercept = B0, slope = B1, color = "blue") +
    lims(y = c(75, 110))

# > Posterior analysis of group-specific relationships ----
runner_summaries_1 = running_model_1 %>% 
    spread_draws(`(Intercept)`, b[,runner]) %>% 
    mutate(runner_intercept = `(Intercept)` + b) %>% 
    select(-`(Intercept)`, -b) %>% 
    median_qi(.width = 0.80) %>% 
    select(runner, runner_intercept, .lower, .upper)

runner_summaries_1 %>% 
    filter(runner %in% c("runner:4", "runner:5"))

# 100 posterior plausible models for runners 4 and 5
running %>% 
    filter(runner %in% c("4", "5")) %>% 
    add_epred_draws(running_model_1, ndraws = 100) %>% 
    ggplot(aes(x = age, y = net)) +
    geom_line(aes(y = .epred, group = paste(runner, .draw), color = runner),
              alpha = 1) +
    geom_point(aes(color = runner), size = 4) +
    theme_minimal() 

# Plot runner specific models with the global model
ggplot(running, aes(x = age, y = set, group = runner)) +
    geom_abline(data = runner_summaries_1, color = "gray",
                aes(intercept = runner_intercept, slope = B1)) +
    geom_abline(intercept = B0, slope = B1, color = "dodgerblue") +
    lims(x = c(50, 61), y = c(50, 135))

# > Posterior analysis of within and between group variability ----

tidy_sigma = tidy(running_model_1, effects = "ran_pars")

# sigma_y represents within group variability and is relatively small
(sigma_y = tidy_sigma$estimate[2])

# sigma_0 represents between group variable and is relatively large
(sigma_0 = tidy_sigma$estimate[1])

# Differences between runners account for roughly 86.6% of the total variability in race times
sigma_0^2 / (sigma_0^2 + sigma_y^2)

# Fluctuations within runners account for the remaining 13.4%
sigma_y^2 / (sigma_0^2 + sigma_y^2)
