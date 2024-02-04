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

# Longitudinal data ----
# There may be a reason to believe that the correlation with time
# may not be the same across time.  For example, a runner's net time
# at age 60 is likely more strongly correlated with net time than
# at age 59 or 50

# See bayeslongitudinal R package and Laird and Ware (1982)

# Hierarchical model for danceability ----

data("spotify")

spotify = spotify %>% 
    select(artist, title, danceability, valence, genre)

ggplot(spotify, aes(x = genre, danceability)) +
    geom_boxplot()
ggplot(spotify, aes(x = valence, y = danceability)) +
    geom_point()
ggplot(spotify, aes(x = valence, y = danceability, group = artist)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.5)

# Some artist's songs tend to be more danceable than others
# The association between danceability and valence might differe among artists

# song i with artist j, let Yijdenote danceability and Xij1 denote valence
# With Xij1 being edm as a baseline

# The global coefficients reflect an assumption that the relationship
# danceability, valence, and genre are similar for each artist

# The artist specific intercepts beta_0j assume that when holding constant
# a song's valence and genre, some artist's songs tend to be more
# danceable than others
spotify_model_1 = stan_glmer(
    danceability ~ valence + genre + (1 | artist),
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

# Model 2 assumes the relationship between danceability and valence
# might differ by artist
spotify_model_2 = stan_glmer(
    danceability ~ valence + genre + (valence | artist),
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(regularization = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

pp_check(spotify_model_1) +
    labs(title = "Hierarchical random intercepts",
         x = "danceability")

pp_check(spotify_model_2) +
    labs(title = "Hierarchical random intercepts and slopes",
         x = "danceability")

# Calculate ELPD for the 2 models ----
elpd_spotify_1 = loo(spotify_model_1)
elpd_spotify_2 = loo(spotify_model_2)

# Compare the ELPD ----
loo_compare(elpd_spotify_1, elpd_spotify_2)

# Comparing model 1 and 2 there is no significant difference so
# we select model 1 for simplicity

# Posterior summary ----
tidy(spotify_model_1, effects = "fixed", 
     conf.int = TRUE, conf.level = 0.80)


# Interpretation of Model 1 parameters:
# danceability ~ valence + genre + (1 | artist)
# - For the average artist (group) in any genre (global regressor)
#   we'd expect danceability to increase by between 2.16 and 3.01
#   points for every 10 point increase on the valence scale
# - Among genres, controlling for valence, only rock is 
#   significantly less danceable than edm (electronic dance music)
#   It's credible interval is the only one to lie entirely above
#   or below 0.  For the average rock song the danceability is
#   between 1.36 and 12 points lower than that of an edm song with
#   the same valence.
# - When interpreting these summaries, keep in mind that the
#   genre coefficients directly compare each genre to edm alon and
#   not, say, rock to r&b.
# - mcmc_areas() offers a useful visual comparison of all genre
#   posteriors.

# > Plot the posterior models of the genre coefficients ----
mcmc_areas(spotify_model_1, pars = vars(starts_with("genre")), 
           prob = 0.8) +
    geom_vline(xintercept = 0)

# > Posterior results for 2 artists in our sample ----
tidy(spotify_model_1, effects = "ran_vals",
     conf.int = TRUE, conf.level = 0.80) %>% 
    filter(level %in% c("Camilo", "Missy_Elliott")) %>% 
    select(level, estimate, conf.low, conf.high)

# Predict the danceability of their next songs
# Simulate posterior predictive models for the 3 artists
set.seed(84735)
predict_next_song = posterior_predict(
    spotify_model_1,
    newdata = data.frame(
        artist = c("Camilo", "Mohsen Beats", "Missy Elliott"),
        valence = c(80, 60, 90), genre = c("latin", "rock", "rap")
    ))

# Posterior predictive plots
mcmc_areas(predict_next_song, prob = 0.80) +
    ggplot2::scale_y_discrete(
        labels = c("Camilo", "Mohsen Beats", "Missy Elliott")
    )

# Note that the use of normal() priors has resulted in results for
# danceability that are greater than 100.  A beta prior could potentially
# be used to constrain danceability between 0 and 100.

# End of Chapter Exercises ----
# Sleep deprivation, reaction time study ----
# > 17.6 ----
# From https://github.com/chuwyler/bayesrules/blob/main/ipynb/Chapter17_Exercises.ipynb
library(tidyverse)
library(bayesrules)
library(bayesplot)
library(rstan)
library(rstanarm)
library(broom.mixed)
library(tidybayes)
library(forcats)
library(lme4)
# use multiple cores, otherwise hierarchical models with random slopes and intercepts are terribly slow
options(mc.cores = parallel::detectCores()) 

data("sleepstudy")
sleepstudy %>% glimpse()
sleepstudy %>% 
    ggplot(aes(x = Days, y = Reaction)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~Subject) +
    labs(
        title = "Reaction times increase with sleep deprivation"
    )

# > 17.7 Posterior simulation ----
# https://www.bayesrulesbook.com/chapter-17#hierarchical-model-with-varying-intercepts-slopes
# Hierarchical random intercepts model
sleep_model_1 = stan_glmer(formula = Reaction ~ Days + (1 | Subject),
                           data = sleepstudy,
                           family = gaussian, 
                           prior_intercept = normal(250, 25, autoscale = TRUE),
                           prior = normal(0, 2.5, autoscale = TRUE),
                           prior_aux = exponential(1, autoscale = TRUE),
                           prior_covariance = decov(regularization = 1, concentration = 1,
                                                    shape = 1, scale = 1),
                           chains = 4, iter = 5000*2, seed = 84735)

# Hierarchical random intercepts and slopes model
sleep_model_2 = stan_glmer(formula = Reaction ~ Days + (Days | Subject),
                           data = sleepstudy,
                           family = gaussian, 
                           prior_intercept = normal(250, 25, autoscale = TRUE),
                           prior = normal(0, 2.5, autoscale = TRUE),
                           prior_aux = exponential(1, autoscale = TRUE),
                           prior_covariance = decov(regularization = 1, concentration = 1,
                                                    shape = 1, scale = 1),
                           chains = 4, iter = 5000*2, seed = 84735)

pp_check(sleep_model_1)
pp_check(sleep_model_2)

prior_summary(sleep_model_1)

tidy(sleep_model_1, conf.int = TRUE, conf.level = 0.80, effects = "ran_pars")

sleep_summary_1 = tidy(sleep_model_1,
                      effects = "fixed",
                      conf.int = TRUE,
                      conf.level = 0.80)

sleep_summary_1

sleep_summary_2 = tidy(sleep_model_2,
                       effects = "fixed",
                       conf.int = TRUE,
                       conf.level = 0.80)

sleep_summary_2

# Reaction = 251 + 10.5 * Days

# > 17.7c ----
# Posterior samples
set.seed(84735)
sleep_chains_2 <- sleep_model_2 %>%
    spread_draws(`(Intercept)`, b[term, Subject], `Days`) %>%
    pivot_wider(names_from = term,
                names_glue = "b_{term}",
                values_from = b) %>%
    mutate(subject_intercept = `(Intercept)` + `b_(Intercept)`,
           subject_Days = Days + b_Days)

head(sleep_chains_2)

# 80% confidence interval for the days regression coefficient: subject_Days 
sleep_chains_2  %>%
    group_by(Subject) %>%
    summarize(
        Days_lower = quantile(subject_Days, 0.1),
        Days_upper = quantile(subject_Days, 0.9)
    ) %>%
    arrange(Subject)


# Posterior medians for sigma_y, sigma_0, sigma_1, p
sleep_chains_2 %>% 
    ungroup() %>% 
    summarise(
        b_intercept = median(`b_(Intercept)`),
        b_days = median(b_Days)
    )

tidy_sigma = tidy(sleep_model_2, effects = "ran_pars")
tidy_sigma

# sigma_1
(sigma_1 = tidy_sigma$estimate[2])

# sigma_0 
(sigma_0 = tidy_sigma$estimate[1])

# rho
(rho = tidy_sigma$estimate[3])

# sigma_y
(sigma_y = tidy_sigma$estimate[4])

# Differences between subjects account for roughly 93.3% of the total variability in reaction time
sigma_y^2 / (sigma_1^2 + sigma_y^2)

# Fluctuations within subjects account for the remaining 6.7%
sigma_1^2 / (sigma_1^2 + sigma_y^2)

# > 17.8 Posterior Prediction ----
# https://www.bayesrulesbook.com/chapter-17#posterior-prediction-2
# a) Identify the person for whom reaction times change the least
# ans: Subject:335 
sleep_chains_2 %>% 
    select(Subject, subject_intercept, subject_Days) %>% 
    group_by(Subject) %>% 
    summarise(subject_intercept = median(subject_intercept),
              subject_Days = median(subject_Days)) %>% 
    arrange(subject_Days)

# Posterior regression model for subject 335: Reaction time = 250 - 0.229 * Days

# b) The person with the largest change in reaction time
# ans: Subject:308 -> Reaction time = 254 + 19.6 * Days
sleep_chains_2 %>% 
    select(Subject, subject_intercept, subject_Days) %>% 
    group_by(Subject) %>% 
    summarise(subject_intercept = median(subject_intercept),
              subject_Days = median(subject_Days)) %>% 
    arrange(-subject_Days)

# c) The person with the slowest baseline reaction time
# ans: Subject:337 -> Reaction time = 283 + 19.6 * Days
sleep_chains_2 %>% 
    select(Subject, subject_intercept, subject_Days) %>% 
    group_by(Subject) %>% 
    summarise(subject_intercept = median(subject_intercept),
              subject_Days = median(subject_Days)) %>% 
    arrange(-subject_intercept)

# d) The person with the fastest baseline reaction time
# ans: Subject:309 -> Reaction time = 215 + 1.27 * Days
sleep_chains_2 %>% 
    select(Subject, subject_intercept, subject_Days) %>% 
    group_by(Subject) %>% 
    summarise(subject_intercept = median(subject_intercept),
              subject_Days = median(subject_Days)) %>% 
    arrange(subject_intercept)

# e) Simulate, plot and discuss the posterior predictive model of reaction time
# after 5 days of sleep deprivation for two subjects:you and Subject 308 

set.seed(84735)
sleep_model_2_df <- data.frame( sleep_model_2 ) 
sleep_model_2_df %>%
    transmute(
        subject_intercept = X.Intercept. + b..Intercept..Subject.308.,
        subject_slope = Days + b.Days.Subject.308.,
        sigma_y = sigma
    ) %>%
    mutate(mu = subject_intercept + 5 * subject_slope) %>%
    mutate(ynew = rnorm(nrow(sleep_model_2_df), mean = mu, sd = sigma_y)) %>%
    summarize(
        ynew_median = median(ynew),
        ynew_lower = quantile(ynew, 0.1),
        ynew_upper = quantile(ynew, 0.9)
    )

??transmute()

sleepstudy %>% head()
predict_reaction_time = posterior_predict(
    sleep_model_2,
    newdata = data.frame(
        Subject = c("Me", "308"),
        Days = c(5, 5)
    ))

data.frame(predict_reaction_time) %>%
    rename(me = X1, subject_308 = X2) %>%
    summarize_all(list(median = median, 
                       sd = sd, 
                       lower = ~ quantile(.x, probs = 0.1),
                       upper = ~ quantile(.x, probs = 0.9)))

??summarize_all()

mcmc_areas(predict_reaction_time, prob = 0.8) +
    ggplot2::scale_y_discrete(labels = c("me", "Subject 308"))

# > 17.9 Model evaluation ----
# https://www.bayesrulesbook.com/chapter-17#model-evaluation-selection
pp_check(sleep_model_1)
pp_check(sleep_model_2)

# Slightly lower MAE for model 2
prediction_summary(sleep_model_1, data = sleepstudy)
prediction_summary(sleep_model_2, data = sleepstudy)

# expected log-predictive densities (ELPD)
elpd1 <- loo(sleep_model_1, k_threshold = 0.7)
elpd2 <- loo(sleep_model_2, k_threshold = 0.7)
loo_compare(elpd1, elpd2)

# > 17.11 Voices study ----
# a)
data("voices")
voices %>% glimpse()
unique(voices$subject) %>% length()
voices %>% na.omit() %>% 
    count(subject)

voices %>% View()

voices = voices %>% 
    mutate(subject = fct_reorder(subject, pitch, .fun = 'mean', .na_rm = TRUE))

voices %>% 
    ggplot(aes(x = subject, y = pitch)) +
    geom_violin(aes(fill = subject)) +
    geom_jitter() +
    labs(
        title = "Pitch variation by subject"
    ) +
    theme_minimal() +
    theme(legend.position = "none")


voices %>% 
    ggplot(aes(x = subject, y = pitch)) +
    geom_boxplot(aes(fill = subject)) +
    geom_jitter() +
    labs(
        title = "Pitch variation by subject"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

# b
voices %>% na.omit() %>% 
    ggplot(aes(x = attitude, y = pitch, color = subject, group = subject)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(~subject) +
    labs(title = "Voice Pitch Changes with Attitude") +
    theme_minimal()

ggplot(voices %>% na.omit(), aes(x = subject, 
                                 y = pitch, 
                                 color = attitude)) + 
    geom_boxplot() 

# > 17.12 ----
mean(voices$pitch, na.rm = TRUE)

voice_model = stan_glmer(
    pitch ~ attitude + (1 | subject),
    data = voices, family = gaussian,
    prior_intercept = normal(200, 50, autoscale = TRUE),
    prior = normal(2.5, 1, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735)

pp_check(voice_model)

prediction_summary(voice_model, data = voices %>% na.omit())

mcmc_trace(voice_model)
mcmc_dens(voice_model)
mcmc_acf(voice_model)

data.frame(voice_model) %>% glimpse()

voice_model %>% 
    spread_draws(`(Intercept)`, b[,subject]) %>% 
    mutate(subject_intercept = `(Intercept)` + b) %>% 
    select(-`(Intercept)`, -b) %>% 
    median_qi(.width = 0.80) %>% 
    select(subject, subject_intercept, .lower, .upper)

# Posterior summary statistics
voice_model_summary = tidy(voice_model, 
                           conf.int = TRUE,
                           conf.level = 0.95)

tidy(voice_model, effects = "fixed", conf.int = TRUE, conf.level = 0.95)


voice_model_summary

# Posterior median model
(B0 = voice_model_summary$estimate[1])
(B1 = voice_model_summary$estimate[2])

# That the 95% confidence interval does not include 0 is clear evidence that 
# there is a non-zero effect of attitude on pitch

# > 17.13 ----
# pitch = 203 -19.4 * X 
# where:     X = 0 for attitude = "formal" 
#            X = 1 for attitude = "polite"

voices %>% 
    # filter(attitude == "polite") %>% 
    group_by(attitude) %>% 
    summarise(mean_pitch_polite = median(pitch, na.rm = TRUE))

voices %>% 
    filter(subject %in% c("A", "F")) %>% 
    group_by(attitude, subject) %>% 
    summarise(mean_pitch_polite = median(pitch, na.rm = TRUE))

summary(voice_model)
subject_summary = spread_draws(voice_model, `(Intercept)`, b[,subject]) %>%
    mutate(voice_intercept = `(Intercept)` + b) %>% 
    select(-`(Intercept)`, -b) %>% 
    median_qi(.width = 0.80) %>% 
    select(subject, voice_intercept, .lower, .upper)

subject_summary

# A: pitch = 259 - 19.4 * X
# F: pitch = 179 - 19.4 * X

set.seed(84735)
predict_new_pitch = posterior_predict(
    voice_model,
    newdata = data.frame(subject = c("A", "F", "You"),
                         attitude = c("polite", "polite", "polite"))
)


# Posterior predictive model plots ----
mcmc_areas(predict_new_pitch, prob = 0.8) +
    ggplot2::scale_y_discrete(labels = c("A", "F", "You"))

summary(predict_new_pitch)

data.frame(predict_new_pitch) %>% 
    rename(A = X1, F = X2, You = X3) %>% 
    summarize(across(everything(), median))

# 'You' is much wider because there is more uncertainty about the intercept

# > 17.14 Coffee ratings ----
# >> Hierarchical model with no parameters ----
data("coffee_ratings_small")

cf = coffee_ratings_small

cf %>% glimpse()
# The obvious hierarchy in this dataset is farm name but there is an issue,
# many examples have NA as the farm name, in fact the majority of examples
# have no farm listed.
cf %>% 
    count(farm_name, sort = TRUE)

cf %>% nrow()
cf %>% na.omit() %>% nrow()

# The most likely outcome variable to predict from this dataset is total_cup_points
# We might want to predict total cup points based purely on the farm name.
# Let's build a simple model for this.

cf_clean_df = cf %>% 
    na.omit() %>% 
    mutate(farm_name = fct_reorder(farm_name, total_cup_points, .fun = 'mean'))


cf_clean_df %>% 
    group_by(farm_name) %>% 
    summarise(median_pts = median(total_cup_points),
                          sd_pt = sd(total_cup_points))

coffee_model_1 = stan_glmer(
    formula = total_cup_points ~ (1|farm_name),
    data = cf_clean_df, family = gaussian,
    prior_intercept = normal(80, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

pp_check(coffee_model_1)

mcmc_trace(coffee_model_1)
mcmc_dens(coffee_model_1)

prior_summary(coffee_model_1)
neff_ratio(coffee_model_1)
rhat(coffee_model_1)

# Posterior summary statistics
coffee_model_summary = tidy(coffee_model_1, 
                            conf.int = TRUE, 
                            conf.level = 0.80)

coffee_model_summary

range(cf_clean_df$total_cup_points)

cf_clean_df %>% 
    group_by(farm_name) %>% 
    summarise(count = n()) %>% 
    summarise(min_blends = min(count),
              max_blends = max(count))

# Some farms are not well represented so we might expect greater variation in
# coffee from these locations

mean_farm_ratings = cf_clean_df %>% 
    group_by(farm_name) %>% 
    summarise(total_cup_points = mean(total_cup_points),
              flavor = mean(flavor),
              uniformity = mean(uniformity))

mean_farm_ratings %>% glimpse()

set.seed(84735)
mean_predictions = posterior_predict(
    coffee_model_1, newdata = mean_farm_ratings
)


# Plot the posterior predictive intervals
ppc_intervals(mean_farm_ratings$total_cup_points, yrep = mean_predictions,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = mean_farm_ratings$farm_name,
                                breaks = 1:nrow(mean_farm_ratings)) +
    xaxis_text(angle = 90, hjust = 1)

ggplot(mean_farm_ratings, aes(x = total_cup_points)) +
    geom_density()

# >> Hierarchical model with random intercepts ----
cf_clean_df %>% glimpse()


# Select numerical columns except for total_cup_points and farm_name
numerical_vars <- cf_clean_df %>% 
    select(-farm_name, -total_cup_points) %>% 
    names()

# Melt the data frame to long format for plotting
cf_long <- cf_clean_df %>% 
    pivot_longer(cols = all_of(numerical_vars), 
                 names_to = "Variable", 
                 values_to = "Value")

# Plot
ggplot(cf_long, aes(x = Value, y = total_cup_points)) +
    geom_point(alpha = 0.5) + # Use alpha to adjust point opacity if there are many points
    facet_wrap(~Variable, scales = "free_x") + # Use 'scales = "free_x"' to allow each plot to have its own x-axis scale
    labs(x = "Variable Value", y = "Total Cup Points", title = "Relationship of Total Cup Points with Other Parameters") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(GGally)
cf_numerical <- cf_clean_df %>% 
    select(-farm_name)

ggpairs(cf_numerical)

options(repr.plot.width = 15, repr.plot.height = 15)
pairs(cf_clean_df  %>% select_if(is.numeric))
cor( cf_clean_df  %>% select_if( is.numeric ) ) %>% View()

coffee_model_2 = stan_glmer(
    formula = total_cup_points ~ flavor + (1 | farm_name),
    data = cf_clean_df, family = gaussian,
    prior_intercept = normal(81, 8, autoscale = TRUE),
    prior = normal(2.5, 1, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735)


pp_check(coffee_model_2)

# >> Hierarchical model with random intercepts and slopes ----
# Takes much longer to fit
coffee_model_3 = stan_glmer(
    formula = total_cup_points ~ flavor + (flavor | farm_name),
    data = cf_clean_df, family = gaussian,
    prior_intercept = normal(81, 8, autoscale = TRUE),
    prior = normal(2.5, 1, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735)

pp_check(coffee_model_3)

# >> Hierarchical model with random intercepts - 2 variables ----
# I selected flavor and uniformity as regressors because they are correlated
# with total cup points but are not strongly correlated with each other
coffee_model_4 = stan_glmer(
    formula = total_cup_points ~ flavor + uniformity + (1 | farm_name),
    data = cf_clean_df, family = gaussian,
    prior_intercept = normal(81, 8, autoscale = TRUE),
    prior = normal(2.5, 1, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, concentration = 1,
                             shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735)


pp_check(coffee_model_4)


# Calculate ELPD for the 2 models ----
# Model 4 is significantly better than model 2 without too much additional
# complexity
elpd_coffee_2 = loo(coffee_model_2)




elpd_coffee_4 = loo(coffee_model_4)

# Compare the ELPD ----
loo_compare(elpd_coffee_2, elpd_coffee_4)


prediction_summary(model = coffee_model_2, 
                   data = cf_clean_df %>% na.omit())

prediction_summary(model = coffee_model_4, 
                   data = cf_clean_df %>% na.omit())


# >> Posterior summary ----

# >>> Global average model ----

tidy(coffee_model_4, effects = "fixed", 
     conf.int = TRUE, conf.level = 0.80)
# Total cup points = 20.5 + 5.28 * flavor + 2.22 * uniformity

# >>> Variability ----

tidy_sigma = tidy(coffee_model_4, effects = "ran_pars")
tidy_sigma

# sigma_y represents within group variability and is relatively small
(sigma_y = tidy_sigma$estimate[2])

# sigma_0 represents between group variable and is relatively large
(sigma_0 = tidy_sigma$estimate[1])

# Differences between farms account for roughly 20.3% of the total variability in cup points
sigma_0^2 / (sigma_0^2 + sigma_y^2)

# Fluctuations within blends account for the remaining 79.7%
sigma_y^2 / (sigma_0^2 + sigma_y^2)

# >>> Ranking farms ----
cf_clean_df %>% 
    ggplot(aes(x = flavor, y = total_cup_points, fill = farm_name)) + 
    geom_boxplot()

coffee_summary <- coffee_model_4 %>%
    spread_draws(`(Intercept)`, b[,farm_name]) %>% # extract dataframe
    mutate(farm_name_intercept = `(Intercept)` + b) %>% # compute individual runner intercepts
    select(-`(Intercept)`, -b) %>% # drop global intercept and b
    median_qi(.width = 0.80) %>%  # compute credible intervals
    select(farm_name, farm_name_intercept, .lower, .upper)

coffee_summary %>% arrange( desc(farm_name_intercept) )


set.seed(84735)
mean_coffee_predictions = posterior_predict(
    coffee_model_4, newdata = mean_farm_ratings
)

# Plot the posterior predictive intervals
ppc_intervals(mean_farm_ratings$total_cup_points, yrep = mean_coffee_predictions,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = mean_farm_ratings$farm_name,
                                breaks = 1:nrow(mean_farm_ratings)) +
    xaxis_text(angle = 90, hjust = 1)
# > 17.15 ----
library(lme4)
data("sleepstudy")
sleepstudy %>% glimpse()
sleepstudy %>% 
    ggplot(aes(x = Days, y = Reaction)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~Subject) +
    labs(
        title = "Reaction times increase with sleep deprivation"
    )

sleep_model_priors = stan_glmer(formula = Reaction ~ Days + (1 | Subject),
                           data = sleepstudy,
                           family = gaussian, 
                           prior_intercept = normal(250, 100),
                           prior = normal(25, 10),
                           prior_aux = exponential(1/100),
                           prior_covariance = decov(regularization = 1, concentration = 1,
                                                    shape = 1, scale = 1/200),
                           chains = 4, iter = 5000*2, seed = 84735, 
                           prior_PD = TRUE)

set.seed(84735)
updated_sleep_data <- sleepstudy %>%
    add_epred_draws(object = sleep_model_priors, 
                    ndraws = 200)

names(updated_sleep_data)

updated_sleep_data %>%
    ggplot(aes(x = Days, y = Reaction)) +
    geom_line(aes(y = .epred, group = .draw), alpha = 0.15)

sleepstudy %>% 
    add_predicted_draws(sleep_model_priors, ndraws = 12) %>%
    # ungroup() %>% 
    # summarise(num = n_distinct(Subject))
    ggplot(aes(x = Days, y = Reaction)) +
    geom_point(aes(y = .prediction, group = .draw)) + 
    facet_wrap(~ .draw)

