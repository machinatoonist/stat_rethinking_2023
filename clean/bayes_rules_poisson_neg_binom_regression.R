# Predict the number of anti-discrimination laws Y_i with predictors:
# X_i1 = Percentage of state's residents that live in urban areas
# X_i2 = Categorical predictor indicating whether state i leans GOP
# X_i3 = Categorical predictor indicating whether state i is a swing state
# Democrats are the baseline/reference level

# Load packages ----
library(bayesrule)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)

data("equality_index")
equality = equality_index

equality %>% 
    ggplot(aes(x = laws)) +
    geom_histogram(color = "white", breaks = seq(0, 160, by = 10))

# Identify the outlier
equality %>% 
    filter(laws == max(laws))

# Remove the outlier
equality = equality %>% 
    filter(state != "california")

equality %>% 
    ggplot(aes(y = laws, x = percent_urban, color = historical)) +
    geom_point()

equality %>% glimpse()

# Posterior Normal regression model ----
equality_normal_sim = stan_glm(
    laws ~ percent_urban + historical,
    data = equality,
    prior_intercept = normal(7, 1.5),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 83735
)

# Posterior predictive check
pp_check(equality_normal_sim, plotfun = "hist", nreps = 5) +
    geom_vline(xintercept = 0) +
    xlab("laws")

equality %>% glimpse()
levels(equality$historical)

# Prior predictive check ----
equality_model_prior = stan_glm(
    laws ~ percent_urban + historical,
    data = equality,
    family = poisson,
    prior_intercept = normal(2, 0.5),
    prior = normal(0, 2.5, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735,
    prior_PD = TRUE
)

prior_summary(equality_model_prior)

equality %>% 
    add_epred_draws(equality_model_prior, ndraws = 100) %>% 
    ggplot(aes(x = percent_urban, y = laws, color = historical)) +
    geom_line(aes(y = .epred, group = paste(historical, .draw))) +
    ylim(0, 100)

equality_model = update(equality_model_prior, prior_PD = FALSE)

# Posterior predictive ----
mcmc_trace(equality_model)
mcmc_dens_overlay(equality_model)
mcmc_acf(equality_model)

set.seed(1)
pp_check(equality_model, plotfun = "hist", nreps = 5) +
    xlab("laws")

pp_check(equality_model) +
    xlab("laws")

equality %>% 
    add_epred_draws(equality_model, ndraws = 100) %>% 
    ggplot(aes(x = percent_urban, y = laws, color = historical)) +
    geom_line(aes(y = .epred, group = paste(historical, .draw)), 
              alpha = 0.1) +
    geom_point(data = equality, size = 0.5)

tidy(equality_model, conf.int = TRUE, conf.level = 0.80)

# swing states tend to have exp(-0.61) = 0.54 or 54% as many anti-discrimination
# laws as dem leaning states

# percent_urban coefficient beta_1 has a posterior median of 0.0164
# Expect the logged number of anti-discrimination laws to increase by 0.0164
# for every extra percentage point of urban population.  On the unlogged scale
# if the urban population in one state is 1% greater than another state, we'd
# expect it to have 1.0164 times the number of anti-discrimination laws.

equality %>% 
    filter(state == "minnesota")

# Posterior predictions
set.seed(84735)
mn_prediction = posterior_predict(
    equality_model, newdata = data.frame(percent_urban = 73.3,
                                         historical = "dem")
)

head(mn_prediction, 3)

# The predicted distribution of laws for Minnesota is well above the actual of 4
mcmc_hist(mn_prediction, binwidth = 1) +
    geom_vline(xintercept = 4) +
    xlab("Predicted number of laws in Minnesota") +
    geom_vline(xintercept = 4)

# Manual prediction of the number of laws for each parameter set in the chain
set.seed(84735)
as.data.frame(equality_model) %>% 
    mutate(log_lambda = `(Intercept)` + percent_urban * 73.3 +
               historicalgop * 0 + historicalswing * 0,
           lambda = exp(log_lambda),
           y_new = rpois(20000, lambda = lambda)) %>% 
    ggplot(aes(x = y_new)) +
    stat_count()

# Simulate posterior predictive models for each state
set.seed(84735)
poisson_predictions = posterior_predict(equality_model, newdata = equality)

ppc_intervals_grouped(equality$laws, yrep = poisson_predictions,
                      x = equality$percent_urban,
                      group = equality$historical,
                      prob = 0.5, prob_outer = 0.95,
                      facet_args = list(scales = "fixed"))

# The observed number of laws fall 3.4 laws or 1.2 standard deviations from
# their posterior mean predictions.
# Roughly 78% of states fall within their corresponding 95% posterior prediction
# interval.

prediction_summary(model = equality_model, data = equality)

# Cross validation
set.seed(84735)
poisson_cv = prediction_summary_cv(model = equality_model,
                                   data = equality, k = 10)

poisson_cv

# Negative bionomial regression for overdispersed counts ----

# Load data
data("pulse_of_the_nation")

pulse = pulse_of_the_nation %>% 
    filter(books < 100)

pulse %>% glimpse()
ggplot(pulse, aes(x = books)) +
    geom_histogram(color = "white")

ggplot(pulse, aes(y = books, x = age)) +
    geom_point()

ggplot(pulse, aes(y = books, x = wise_unwise)) +
    geom_boxplot()

# The skewed count structure for books makes Poisson regression a reasonable
# first approach

books_poisson_sim = stan_glm(
    books ~ age + wise_unwise,
    data = pulse, family = poisson,
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735
)

# The fit is not great
# The Poisson regression preserved the Poisson property of equal mean and variance
pp_check(books_poisson_sim) +
    xlab("books")

# Mean and variability in readership are much different
pulse %>% 
    summarise(mean = mean(books),
              variance = var(books))

# When we cut the age range and group by wise_unwise and age this difference
# between mean and variance remains
pulse %>% 
    group_by(cut(age, 3), wise_unwise) %>% 
    summarise(mean = mean(books),
              variance = var(books))

# Book readership is said to be "overdispersed".
# A random variable Y is said to be overdispersed if the observed variability
# in Y exceeds the variability expected by the assumed probability model of Y.

# An alternative to the Poisson for count data is the Negative Binomial probability model
# Makes no assumption that E(Y) = Var(Y)

books_negbin_sim = stan_glm(
    books ~ age + wise_unwise,
    data = pulse, family = neg_binomial_2,
    prior_intercept = normal(0, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735
)

prior_summary(books_negbin_sim)

pp_check(books_negbin_sim) +
    xlim(0, 75) +
    xlab("books")

books_negbin_sim %>% tidy(conf.int = TRUE, conf.level = 0.8)
exp(0.266)
# Controlling for wise_unwise there's no relationship with age and readership
# Controlling for age, people who prefer to be wise but unhappy read 1.3 times
# or 30% more books