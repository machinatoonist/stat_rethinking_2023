library(tidyverse)
library(brms)
library(bayesplot)
library(loo)

mn_radon = read_csv("https://raw.githubusercontent.com/mitzimorris/brms_feb_28_2023/main/data/mn_radon.csv")


packages <- c("readr", "ggplot2", "brms", "bayesplot", "loo", "projpred", "cmdstanr")
require(packages)
suppressMessages(  lapply(packages, library, character.only = TRUE)  )
options(brms.backend = "cmdstanr")

# This example is taken from chapter 12 of the book [Data Analysis Using
#                                                    Regression and Multilevel/Hierarchical
#                                                    Models](http://www.stat.columbia.edu/~gelman/arm/) by Andrew Gelman and
# Jennifer Hill, Cambridge Press, 2006.
# 
# In the early 1990s, the US Environmental Protection Agency (EPA) did a
# national survey of home radon levels. Gelman and Hill analyze data for
# the state of Minnesota.
# 
# -   Survey data
# -   919 measurements of radon gas levels in residential houses
# -   85 counties in Minnesota have survey data, 2 counties have no
# data.
# -   measurements were taken in the basement, if any, else on the
# ground floor.
# -   another EPA dataset provides the soil uranium level for each
# county
# -   Regression model
# -   outcome: home radon level (on the log scale)
# -   predictors: county, floor, uranium (on the log scale)
# 
# The data file is `data/mn_radon.csv` and the data is loaded into
# dataframe `mn_radon`.

mn_radon$county_id <- as.factor(mn_radon$county_id)
print(mn_radon)

# Structure in the data: houses can be grouped by county.
# 
# -   Modeling choices
# -   Complete pooling: all counties are alike. Run a single
# regression to estimate average home radon level in MN.
# -   No pooling: all 85 counties are different. Run 85 separate
# regressions to estimate home radon levels by county.
# -   Partial pooling: counties are similar. Build a multi-level
# regression to share information across counties.
# Complete pooling model ----
fit_complete_pool = brm(log_radon ~ floor, data = mn_radon)
summary(fit_complete_pool)

conditional_effects(fit_complete_pool)

# No pooling model ----
fit_no_pool = brm(log_radon ~ 0 + floor + county_id, data = mn_radon)
summary(fit_no_pool)
conditional_effects(fit_no_pool, effects = c("floor:county_id"))

# Partial pooling model (varying intercept) ----
fit_part_pool_1 = brm(log_radon ~ floor + (1 |county_id), data = mn_radon)
summary(fit_part_pool_1)

bayesplot::mcmc_intervals(fit_part_pool_1, regex_pars = "r_county") +  
    bayesplot::yaxis_text(FALSE)

conditional_effects(fit_part_pool_1)

posterior_interval(fit_part_pool_1)

# Partial pooling (varying intercept/slope) ----
fit_part_pool_2 = brm(log_radon ~ floor + (1 + floor |county_id), 
                      iter = 4000,
                      data = mn_radon)

bayesplot::mcmc_intervals(fit_part_pool_2, regex_pars = "r_county") +  
    bayesplot::yaxis_text(FALSE)

# Partial pooling with a second unpooled predictor ----
fit_part_pool_3 = brm(log_radon ~ floor + log_uranium + (1 + floor |county_id), data = mn_radon)
bayesplot::mcmc_intervals(fit_part_pool_3, regex_pars = "r_county") +  bayesplot::yaxis_text(FALSE)
