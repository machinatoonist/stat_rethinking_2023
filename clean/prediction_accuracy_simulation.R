library(rethinking)
library(dagitty)
library(dplyr)
library(ggplot2)

# We have a model that is making predictions.
# There are different possible errors in the inputs to these model predictions
# that could be effecting prediction accuracy.
# Our goal is to measure the effect of the errors in the predictors where
# the measure outcome is forecast error.

# Simulate forecast variables effected by one treatment ----
N = 1000
mu_a = 100
bias = 0.05
rand_error = 0.08
a = rnorm(N, mu_a, 1)  # actuals
t1 = rep(0:1, each = N/2)  # flag to indicate the presence of a treatment causing over estimates
t1b = a * t1 * bias # Bias introduced for over estimation if treatment is present
f1 = a + rnorm(N, 0, mu_a * rand_error)  # forecast with no effect
f1b = f1 + t1b # forecast with bias for treated estimates
dens(a - f1b)
precis(data.frame(counterfactual = f1,
                  forecast = f1b,
                  error = a - f1b))

sim_df = tibble(
    treatment = t1,
    forecast = f1b,
    actual = a,
    error = f1b - a
) %>% 
    mutate(treatment = factor(treatment, 
                              levels = c(0, 1), 
                              labels = c("No", "Yes")))

sim_df %>% 
    count(treatment)

plot_df %>% 
    group_by(treatment) %>% 
    dplyr::summarize(
        error = mean(error),
        rmse = sqrt(sum(error^2)/n()),
        mad = (sum(abs(error))/n())
    )


plot_df %>% 
    ggplot(aes(x = actual, y = forecast, color = treatment)) +
    geom_point()

plot_df %>% 
    ggplot(aes(x = forecast, fill = treatment)) +
    geom_density(alpha = 0.3)


plot_df %>% 
    ggplot(aes(x = error, fill = treatment)) +
    geom_density(alpha = 0.3)

sim_df %>% glimpse()

mf = quap(
    alist(
        error ~ dnorm(mu, sigma),
        mu <- a + t1_b * treatment,
        a ~ dnorm(0, 1),
        t1_b ~ dnorm(0, 1),
        sigma ~ exp(1)
    ), data = sim_df
)

precis(mf)
mf

# Input parameters in simulation
print(paste0("Bias: ", bias * 100, "%"))
print(paste0("Random Error: ", rand_error * 100, "%"))

# For the second model the treatment causes over or under estimates ----
N = 3000
mu_a = 100
bias = 0.05
rand_error = 0.08
a = rnorm(N, mu_a, 1)  # actuals

# Treatment flags
# 0 for no treatment, 1 for Treatment 1, 2 for Treatment 2
treatment = rep(0:2, each = N / 3)

# Applying bias based on treatment
t1b = ifelse(treatment == 1, a * bias, 0)    # Bias for Treatment 1
t2b = ifelse(treatment == 2, a * -bias, 0)   # Bias for Treatment 2 

f1 = a + rnorm(N, 0, mu_a * rand_error)  # Base forecast with no effect
f1b = f1 + t1b + t2b                    # Forecast with bias from both treatments

sim_df = tibble(
    treatment = factor(treatment, levels = c(0, 1, 2), 
                       labels = c("No Error", "Over forecast", "Under forecast")),
    forecast = f1b,
    actual = a,
    error = f1b - a
)

sim_df %>% 
    count(treatment)

sim_df %>% 
    group_by(treatment) %>% 
    dplyr::summarize(
        error = mean(error),
        rmse = sqrt(sum(error^2)/n()),
        mad = (sum(abs(error))/n())
    )


sim_df %>% 
    ggplot(aes(x = actual, y = forecast, color = treatment)) +
    geom_point()

sim_df %>% 
    ggplot(aes(x = forecast, fill = treatment)) +
    geom_density(alpha = 0.3)

sim_df %>% glimpse()

# introduce two indices - one for Treatment 1 and another for Treatment 2. 
# No treatment group will serve as the baseline, so no specific index is needed for it.

mf = quap(
    alist(
        error ~ dnorm(mu, sigma),
        mu <- a + t1_b[treatment],
        a ~ dnorm(0, 1),
        t1_b[treatment] ~ dnorm(0, 2),
        sigma ~ exp(1)
    ), data = sim_df
)

precis(mf, depth = 2)


# rstanarm verion ----
library(rstanarm)
library(tidybayes)

# Convert to rstanarm model
mf_stanarm <- stan_glmer(
    error ~ 1 + (1 | treatment),
    data = sim_df,
    prior = normal(0, 1),
    prior_intercept = normal(0, 1),
    prior_aux = exponential(1),
    chains = 4, 
    control = list(adapt_delta = 0.95),
    iter = 4000,
    cores = 4
)

# Print the summary of the model
print(summary(mf_stanarm))

# Summarise model with tidybayes ----
# See https://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html
get_variables(mf_stanarm)

mf_stanarm %>%
    spread_draws(`b[(Intercept) treatment:Over_forecast]`) %>%
    rename(over_fcst_b = `b[(Intercept) treatment:Over_forecast]`) %>% 
    summarise(mean = mean(over_fcst_b),
              sd = sd(over_fcst_b),
              q10 = quantile(over_fcst_b, probs = 0.1),
              q90 = quantile(over_fcst_b, probs = 0.9))

mf_stanarm %>% 
    spread_draws(b[,(Intercept)]) %>%
    separate("(Intercept)", c("(Intercept)", "treatment"), ":") %>%
    head(10)

mf_stanarm %>% 
    spread_draws(b[,(Intercept)]) %>%
    separate("(Intercept)", c("(Intercept)", "treatment"), ":") %>%
    group_by(treatment) %>% 
    summarise(mean = mean(b),
              sd = sd(b),
              q10 = quantile(b, probs = 0.1),
              q90 = quantile(b, probs = 0.9))

mf_stanarm %>%
    spread_draws(`(Intercept)`, sigma) %>%
    head(10)

mf_stanarm %>%
    spread_draws(`(Intercept)`, sigma) %>%
    median_qi(`(Intercept)`, sigma)

mf_stanarm %>%
    spread_draws(b[,treatment]) %>%
    median_qi()

mf_stanarm %>%
    spread_draws(b[,treatment]) %>%
    summarise_draws()

mf_stanarm %>% 
    spread_draws(`(Intercept)`, b[,treatment]) %>%
    head(10)

mf_stanarm %>%
    spread_draws(`(Intercept)`, b[,treatment]) %>%
    mutate(condition_mean = `(Intercept)` + b) %>%
    median_qi(condition_mean)

# > Plot intervals ----
mf_stanarm %>%
    spread_draws(`(Intercept)`, b[,treatment]) %>%
    median_qi(condition_mean = `(Intercept)` + b, .width = c(.95, .66)) %>%
    ggplot(aes(y = treatment, x = condition_mean, xmin = .lower, xmax = .upper)) +
    geom_pointinterval()

mf_stanarm %>%
    spread_draws(`(Intercept)`, b[,treatment]) %>%
    mutate(condition_mean = `(Intercept)` + b) %>%
    ggplot(aes(y = treatment, x = condition_mean)) +
    stat_halfeye()

mf_stanarm %>%
    spread_draws(`(Intercept)`, b[,treatment]) %>%
    mutate(condition_mean = `(Intercept)` + b) %>%
    ggplot(aes(y = treatment, x = condition_mean, fill = after_stat(abs(x) < .8))) +
    stat_halfeye() +
    geom_vline(xintercept = c(-.8, .8), linetype = "dashed") +
    scale_fill_manual(values = c("gray80", "skyblue"))

# > Posterior means and predictions ----
sim_df %>%
    data_grid(treatment) %>%
    add_epred_draws(mf_stanarm) %>%
    head(10)

sim_df %>%
    data_grid(treatment) %>%
    add_epred_draws(mf_stanarm) %>%
    ggplot(aes(x = .epred, y = treatment)) +
    stat_pointinterval(.width = c(.66, .95))

# > Quantile dotplots ----
sim_df %>%
    data_grid(treatment) %>%
    add_epred_draws(mf_stanarm) %>%
    ggplot(aes(x = .epred, y = treatment)) +
    stat_dotsinterval(quantiles = 100)

# > Posterior predictions ----
# Where add_epred_draws() is analogous to rstanarm::posterior_epred(), 
# add_predicted_draws() is analogous to rstanarm::posterior_predict(), 
# giving draws from the posterior predictive distribution.
grid = sim_df %>%
    data_grid(treatment)

means = grid %>%
    add_epred_draws(mf_stanarm)

preds = grid %>%
    add_predicted_draws(mf_stanarm)

sim_df %>%
    ggplot(aes(y = treatment, x = error)) +
    stat_interval(aes(x = .prediction), data = preds) +
    stat_pointinterval(aes(x = .epred), 
                       data = means, 
                       .width = c(.66, .95), 
                       position = position_nudge(y = -0.3)) +
    geom_point() +
    scale_color_brewer()
