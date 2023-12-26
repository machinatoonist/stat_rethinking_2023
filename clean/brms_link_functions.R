library(brms)
library(rstanarm)
library(ggplot2)
library(dplyr)

set.seed(2024)
n = 100
x = rnorm(n)
hist(x)

# plogis is the logistic function, an S-shaped curve that maps any
# real valued number into a value between 0 and 1
y = rbinom(n, 1, plogis(0.5 * x))

hist(y)
hist(plogis(0.5*x))
hist(0.5*x)

d = data.frame(x = x, y = y)

d %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    stat_smooth(method = "glm", 
                method.args = list(family = "binomial"), 
                se = FALSE) +
    theme_minimal() +
    labs(
        title = "Binary Outcome vs Predicted"
    )

# Logit link function ----
# THe logic link uses the logistic distribution, which has a cumulative 
# distribution function(CDF) that is a S-shaped curve.  The coefficients 
# represent log odds.  A one unit increase in the predictor variable results
# in a change in the log odds of the outcome equal to the coefficient.
# bernoulli() uses logit link by default

fit_logit = brm(y ~ x, family = bernoulli(link = "logit"), data = d)

summary(fit_logit)

# Intercept coefficient: the log odds of the outcome variable being 1 (versus 0)
# when the predictor x is equal to 0,
# Slope coefficient for x: a one unit increase in the predictor variable results
# in a change in the log odds of the outcome equal to the coefficient.
# bernoulli() uses logit link by default
brms::posterior_interval(fit_logit, prob = .9)

fit_logit_stan = stan_glm(y ~ x, family = binomial(link = "logit"), data = d)

summary(fit_logit_stan)

rstanarm::posterior_interval(fit_logit_stan, prob = .9)

# Probit link function ----
# The probit function uses the standard normal distribution.  It's CDF is an
# S-shaped curve, similar to the logit, but with thinner tails.
fit_probit_brms = brm(y ~ x, family = bernoulli(link = "probit"), data = d)

summary(fit_probit_brms)

brms::posterior_interval(fit_probit_brms)

fit_probit_stan = stan_glm(y ~ x, family = binomial(link = "probit"), data = d)

summary(fit_probit_stan)

rstanarm::posterior_interval(fit_probit_stan)

# A story about the Poisson link function ----
library(rstanarm)
library(tidybayes)
library(brms)
library(ggplot2)
library(tidyr)
library(stringr)

# R Code to Simulate Data for the Midnight Bakery Story
set.seed(42)  # For reproducibility

# Number of nights to simulate
nights <- 100

# Average number of customers per night
avg_customers <- 50

# Average number of pastries each customer buys
avg_pastries_per_customer <- 1.5

# Generate number of customers each night (Poisson distributed)
customers <- rpois(nights, avg_customers)

# Generate number of pastries sold each night (also Poisson, but influenced by number of customers)
pastries_sold <- rpois(nights, customers * avg_pastries_per_customer)

# Create a data frame
data <- data.frame(night = 1:nights, customers = customers, pastries_sold = pastries_sold)

# Display the first few rows of the data frame
head(data)

data %>% 
    ggplot(aes(x = night, pastries_sold)) +
    geom_point()

(mean_pastry = mean(data$pastries_sold))
(mean_customers = mean(data$customers))

mean_pastry/mean_customers

max(data$pastries_sold)
min(data$pastries_sold)

fit_pastry_rstanarm = stan_glm(pastries_sold ~ customers, 
                               family = poisson(),
                               data = data)

summary(fit_pastry_rstanarm)
rstanarm::posterior_interval(fit_pastry_rstanarm)

posterior_predict_rstanarm = rstanarm::posterior_predict(fit_pastry_rstanarm)

data_pred_rstanarm = data.frame(pastries_sold = posterior_predict_rstanarm) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "simulation", 
                 values_to = "predicted_pastries") %>% 
    mutate(simulation = str_remove(simulation, pattern = "pastries_sold."))

data_pred_rstanarm %>% 
    ggplot(aes(x = predicted_pastries)) +
    geom_density(fill = "blue", alpha = 0.5) +
    theme_minimal()

fitted_draws_pastries = add_epred_draws(data, fit_pastry_rstanarm, dpar = "mu")

fitted_draws_pastries %>% 
    ggplot(aes(.epred)) +
    stat_halfeye(slab_type = "cdf")
