# Load packages ----

library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)
library(here)

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

saveRDS(complete_pooled_model, file = file.path(here(), "fits/complete_pooled_running.rds"))
complete_pooled_model = readRDS(file = file.path(here(), "fits/complete_pooled_running.rds"))


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

saveRDS(running_model_1, file = file.path(here(), "fits/running_model_1.rds"))
running_model_1 = readRDS(file = file.path(here(), "fits/running_model_1.rds"))

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
    add_epred_draws(object = running_model_1, 
                    ndraws = 200, 
                    re_formula = NA)

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

# Hierarchical random intercepts and slopes model ----
# Plot runner-specific models in the data
running %>% 
    filter(runner %in% c("4", "5", "20", "29", "33", "35")) %>% 
    ggplot(., aes(x = age, y = net)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) + 
    facet_wrap(~runner, nrow = 2)

running %>% 
    ggplot(aes(x = age, y = net, group = runner)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
    theme_minimal()

# > Define model ----
running_model_2 = stan_glmer(
    net ~ age + (age | runner),
    data = running, family = gaussian,
    prior_intercept = normal(100, 10),
    prior = normal(2.5, 1),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735, adapt_delta = 0.99999
)

prior_summary(running_model_2)

saveRDS(running_model_2, file = file.path(here(), "fits/running_model_2.rds"))
running_model_2 = readRDS(file = file.path(here(), "fits/running_model_2.rds"))

# Get MCMC chains for the runner-specific intercepts and slopes
runner_chains_2 = running_model_2 %>% 
    spread_draws(`(Intercept)`, b[term, runner], `age`) %>% 
    pivot_wider(names_from = term, names_glue = "b_{term}",
                values_from = b) %>% 
    mutate(runner_intercept = `(Intercept)` + `b_(Intercept)`,
           runner_age = age + b_age)

# Posterior medians of runner-specific models
runner_summaries_2 = runner_chains_2 %>% 
    group_by(runner) %>% 
    summarise(runner_intercept = median(runner_intercept),
              runner_age = median(runner_age))

head(runner_summaries_2)

runner_summaries_2 %>% 
    ggplot(aes(y = net, x = age, group = runner)) + 
    geom_abline(data = runner_summaries_2, color = "gray",
                aes(intercept = runner_intercept, slope = runner_age)) +
    lims(x = c(50, 61), y = c(50, 135)) +
    theme_minimal()

runner_summaries_2 %>% 
    filter(runner %in% c("runner:1", "runner:10"))

# There is shrinkage to global averages as a result of a small number
# of samples per runner

# > Posterior analysis of within- and between-group variability ----
tidy(running_model_2, effects = "ran_pars")

# The sd in the age coefficients beta_1j is likely around 0.25 minutes per year
# (see sd_age.runner coefficient)
# There is little variability between runners in terms of the rate at which
# running times change with age

# The individual runner's net times tend to deviate from their own mean
# model by roughly 5.17 minutes (see sd_Observation.Residual coefficient)

# There's a weak negative correlation of around -0.077 between the 
# runner specific beta_0j and beta_1j parameters.  Runners that start off
# faster tend to slow down at a slightly faster rate

# Model evaluation and selection ----
plot_pooled_model = pp_check(complete_pooled_model) +
    labs(x = "net", title = "complete pooled model")

pp_check(running_model_1) + 
    labs(x = "net", title = "hierarchical random intercepts model")

pp_check(running_model_2) +
    labs(x = "net", title = "hierarchical random intercepts and slopes model")

# Calculate prediction summaries
# Complete pooled model removed because it doesn't make sense to ignore the
# grouped nature of the running times.

set.seed(84735)
prediction_summary(model = running_model_1, data = running)

prediction_summary(model = running_model_2, data = running)

# Very similar metrics by all metrics

# We could utilise prediction_summary_cv() to obtain cross validated metrics
# of posterior predictive accuracy.  Do not run - likely will take long time.
# predictive_summary_cv(model = running_model_1, data = running, 
#                       k = 10, group = "runner")

# Calculate expected log-predictive densities (ELPD) ----
# The estimated ELPD for running_model_1 is lower (worse) than
# but within 2 standard errors of the running_model_2 ELPD.  This is not
# a significant difference in posterior predictive accuracy
elpd_hierarchical_1 = loo(running_model_1)
elpd_hierarchical_2 = loo(running_model_2)

# In this case the additional complexity of the hierarchical random intercepts
# and slopes model does not offer any significant benefit over the over the
# simpler hierarchical random intercepts model.
# Compare the ELPD
loo_compare(elpd_hierarchical_1, elpd_hierarchical_2)

# Posterior prediction ----
# Plot runner-specific trend for runners 1 and 10
running %>%
    filter(runner %in% c("1", "10")) %>% 
    ggplot(., aes(x = age, y = net)) +
    geom_point() +
    facet_grid(~runner) +
    lims(x = c(54, 61))

# Simulate posterior predictive models for the 3 runners
set.seed(84735)
predict_next_race = posterior_predict(
    running_model_1,
    newdata = data.frame(runner = c("1", "Miles", "10"),
                         age = c(61, 61, 61))
)

# Estimate of time from global posterior median model for average runner
B0 + B1 * 61

# Posterior predictive model plots ----
mcmc_areas(predict_next_race, prob = 0.8) +
    ggplot2::scale_y_discrete(labels = c("runner 1", "Miles", "runner 10"))

# Notice how the distribution for Miles is much wider with a median near
# the global model median because we have no data on him.