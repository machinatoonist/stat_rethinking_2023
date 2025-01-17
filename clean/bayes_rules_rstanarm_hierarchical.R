library(bayesrules)
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(forcats)

options(mc.cores = parallel::detectCores())

data("spotify")

spotify = spotify %>% 
    select(artist, title, popularity) %>% 
    mutate(artist = fct_reorder(artist, popularity, .fun = 'mean'))

spotify %>% glimpse()

# number of songs 
nrow(spotify)

# number of artist
nlevels(spotify$artist)

# Group means : artist means ----
# summarise the number of songs observed by each artist and mean popularity of these songs
artist_means = spotify %>% 
    group_by(artist) %>% 
    summarise(count = n(), popularity = mean(popularity))

artist_means %>% 
    slice(1:2, 43:44)

# Complete pooled model ----
head(artist_means, 2)

artist_means %>% 
    summarise(min(count), max(count))

ggplot(spotify, aes(x = popularity)) +
    geom_density()

spotify_complete_pooled = stan_glm(
    popularity ~ 1,
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735
)

# Get prior specification
prior_summary(spotify_complete_pooled)

complete_summary = tidy(spotify_complete_pooled,
                        effects = c("fixed", "aux"),
                        conf.int = TRUE, conf.level = 0.80)

complete_summary

set.seed(84735)
predictions_complete = posterior_predict(spotify_complete_pooled,
                                         newdata = artist_means)

ppc_intervals(artist_means$popularity, yrep = predictions_complete,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = artist_means$artist,
                                breaks = 1:nrow(artist_means)) +
    xaxis_text(angle = 90, hjust = 1)

# No pooled model ----
ggplot(spotify, aes(x = popularity, group = artist)) +
    geom_density()

spotify_no_pooled = stan_glm(
    popularity ~ artist - 1,
    data = spotify, family = gaussian,
    prior = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    chains = 4, iter = 5000*2, seed = 84735
)

# Simulate the posterior predictions model
set.seed(84735)
predictions_no = posterior_predict(
    spotify_no_pooled, newdata = artist_means
)

# Plot the posterior predictive intervals
ppc_intervals(artist_means$popularity, yrep = predictions_no,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = artist_means$artist,
                                breaks = 1:nrow(artist_means)) +
    xaxis_text(angle = 90, hjust = 1)

# Building a hierarchical model ----
# Variability in mean song popularity from artist to artist
ggplot(artist_means, aes(x = popularity)) +
    geom_density()

# > Posterior simulation ----
spotify_hierarchical = stan_glmer(
    popularity ~ (1 | artist),
    data = spotify, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

# Confirm prior tunings
prior_summary(spotify_hierarchical)

mcmc_trace(spotify_hierarchical)
mcmc_dens_overlay(spotify_hierarchical)
mcmc_acf(spotify_hierarchical)
neff_ratio(spotify_hierarchical)
rhat(spotify_hierarchical)

# 100 posterior simulated datasets of song popularity along with the actual
pp_check(spotify_hierarchical)

# Store the simulation in a data.frame
spotify_hierarchical_df = as.data.frame(spotify_hierarchical)

# Check the first 3 and last 3 parameter labels
total_params = nlevels(spotify$artist) + 3
total_params_less_3 = total_params - 2

spotify_hierarchical_df %>% 
    colnames() %>% 
    as.data.frame() %>% 
    slice(1:3, total_params_less_3:total_params)

# > Posterior analysis of global parameters ----

# Posterior summary of mu
tidy(spotify_hierarchical, effects = "fixed",
     conf.int = TRUE, conf.level = 0.80)

# Posterior medians for sigma_y and sigma_mu
tidy(spotify_hierarchical, effects = "ran_pars")

# posterior median of sigma_y - (sd_Observation.Residual) : Within any given
# artists, popularity ratings tend to vary by 14 points from song to song

# posterior median of sigma_mu - (sd_(Intercept).artist) : The mean popularity
# tends to vary by 15.1 points from artist to artist. This is the between
# standard deviation

# Variance explained by differences between artists
(correlation_of_popularity_of_songs_by_same_artist = 15.2 ^ 2 / (15.2^2 + 14.0 ^ 2))

# Variance explained by differences between songs within each artist
(variance_explained_by_differences_among_songs_within_artist =  14.0 ^ 2 / (15.2^2 + 14.0 ^ 2))

# > Posterior analysis and group-specific parameters ----
artist_summary = tidy(spotify_hierarchical, effects = "ran_vals",
                      conf.int = TRUE, conf.level = 0.80)

artist_summary %>% 
    select(level, conf.low, conf.high) %>% 
    slice(1:2, 43:44) 

# There's an 80% chance that Camilo's mean popularity rating is between 19.4 and 32.4 above
# that of the average artist.  Likewise, there's an 80% chance that Mia X's mean popularity
# rating is between 23.3 and 40.7 below the popularity of the average artist.

# > Directly simulate posterior models for the artist-specific means ----

# Get MCMC chains for each mu_j
artist_chains = spotify_hierarchical %>% 
    spread_draws(`(Intercept)`, b[,artist]) %>% 
    mutate(mu_j = `(Intercept)` + b)

artist_chains %>% 
    select(artist, `(Intercept)`, b, mu_j) %>% 
    head(4)

# Get posterior summaries for each artist's mean popularity mu_j
artist_summary_scaled = artist_chains %>% 
    select(-`(Intercept)`, -b) %>% 
    mean_qi(.width = 0.8) %>% 
    mutate(artist = fct_reorder(artist, mu_j))

# Check out the results
artist_summary_scaled %>% 
    select(artist, mu_j, .lower, .upper) %>% 
    head(4)

# 80% credible intervals for each artist's mean song popularity
ggplot(artist_summary_scaled,
       aes(x = artist, y = mu_j, ymin = .lower, ymax = .upper)) +
    geom_pointrange() +
    xaxis_text(angle = 90, hjust = 1)

# The larger count of songs is the main reason for greater certainty about Frank Ocean's popularity vs Lil Skies
artist_means %>% 
    filter(artist %in% c("Frank Ocean", "Lil Skies"))

# > Posterior prediction for observed group ----
# Simulate Ocean's posterior predictive model
set.seed(84735)
ocean_chains = spotify_hierarchical_df %>% 
    rename(b = `b[(Intercept) artist:Frank_Ocean]`) %>% 
    select(`(Intercept)`, b, sigma) %>% 
    mutate(mu_ocean = `(Intercept)` + b,
           y_ocean = rnorm(20000, mean = mu_ocean, sd = sigma))

head(ocean_chains, 3)

# Posterior summary for Y_new, j
# The range is much wider than the 80% credible interval for Ocean's mu_j parameter
# We can be much more certain about Ocean's underlying mean song popularity than in 
# the popularity of any single Ocean song.
ocean_chains %>% 
    mean_qi(y_ocean, .width = 0.80)

# Posterior summary of mu_j
artist_summary_scaled %>% 
    filter(artist == "artist:Frank_Ocean")

# > Posterior prediction for yet unobserved group ----
# We have no information about mu_j for Mohsen Beats like we do for Frank Ocean
# We can't take the same approach as for Frank Ocean.
# We do know Mohsen Beats is a member of the broader population of artists
# Mean popularity for these artists are normally distributed around the global
# mean (mu) with between artist standard deviation sigma_mu

# We need to account for:
# 1) Within group sampling variability in Y.  Not all Mohsen Beats songs are
# equally popular
# 2) Between group sampling variability in mu_j.  Not all artists are equally
# popular
# 3) Posterior variability in the global model parameters, sigma_y, mu, sigma_mu

# Manual calculation of prediction
set.seed(84735)
mohsen_chains = spotify_hierarchical_df %>% 
    mutate(sigma_mu = sqrt(`Sigma[artist:(Intercept),(Intercept)]`),
           mu_mohsen = rnorm(20000, `(Intercept)`, sigma_mu),
           y_mohsen = rnorm(20000, mu_mohsen, sigma))

# Posterior predictive summaries
mohsen_chains %>% 
    select(`(Intercept)`, contains("mohsen")) %>% head(5)

mohsen_chains %>% 
    mean_qi(y_mohsen, .width = 0.80)

# Using posterior predict
set.seed(84735)
prediction_shortcut = posterior_predict(
    spotify_hierarchical,
    newdata = data.frame(artist = c("Frank Ocean", "Mohsen Beats"))
)

data.frame(prediction_shortcut) %>% 
    summarise(mean_pop_frank_ocean = mean(X1),
              mean_pop_mohsen_beats = mean(X2))

# Posterior predictive model plots
mcmc_areas(prediction_shortcut, prob = 0.8) +
    ggplot2::scale_y_discrete(labels = c("Frank Ocean", "Mohsen Beats")) +
    theme_minimal()

# > Shrinkage and the bias-variance trade-off ----
set.seed(84735)
predictions_hierarchical = posterior_predict(spotify_hierarchical,
                                             newdata = artist_means)

# Posterior predictive plots
# Contrast the hierarchical model posterior mean predictions with the complete
# pooled model predictions (dashed horizontal line) and no pooled model predictions
# (dark blue dots).  The hierarchical predictions are pulled or 'shrunk' toward the
# global trends of the complete pooled model and away from the local trends of the 
# no pooled model.  
# 1) Shrinkage increases as the number of observations on group j decrease.
# 2) Shrinkage increase when the variability within groups, sigma_y, is large
# in comparison to the variability between groups sigma_mu.
ppc_intervals(artist_means$popularity, yrep = predictions_hierarchical,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = artist_means$artist,
                                breaks = 1:nrow(artist_means)) +
    xaxis_text(angle = 90, hjust = 1) +
    # Average popularity across all songs
    geom_hline(yintercept = 58.4, linetype = "dashed")

artist

artist_means %>% 
    filter(artist %in% c("Camila Cabello", "Lil Skies"))

# Complete pooled models tend to have higher bias and lower variance
# Unpooled models tend to have lower bias and higher variance
# Hierarchical models tend to have lower bias and higher variance. They
# take group-specific trends and global trends into account.

# If the observed categorical data on A covers all categories, it's
# likely not a grouping variance, but rather a potential predictor.
# If the observed categories on X are merely a random sample from
# many of interest it is a potential grouping variable.

data(bikes)
bikes %>% 
    select(rides, weekend) %>% 
    head()

bikes %>% 
    group_by(weekend) %>% 
    tally()

# The observed weekend values cover all categories of interest, so it's
# not a grouping variable, but a potential predictor.

data("big_word_club")
big_word_club = big_word_club %>% 
    select(score_a2, school_id) %>% 
    na.omit()

big_word_club %>% 
    slice(1:2, 602:603)

# Treating school_id as a grouping variable in a hierarchical model of score_a2
# would allow us to extend our conclusions to the broader population of schools.

# Exercises ----
# > 16.2
data("climbers_sub")
climbers_sub %>% glimpse()

unique(climbers_sub$season)

# Expedition id is a potential grouping variable because it's a random
# sample of many of interest

# Season is a potential predictor for climber success because it covers
# all possible values in the 4 seasons.

data("coffee_ratings")
coffee_ratings %>% glimpse()
unique(coffee_ratings$processing_method)

# Processing method is a potential grouping variable because there
# are a fixed number of options and it is not a random sample

# Farm name is a potential grouping variable because it represents a
# random sample of any potential number of farms.

# > 16.6 ----
data("big_word_club")
big_word_club = big_word_club %>% 
    filter(treat == 1) %>% 
    select(school_id, score_pct_change) %>% 
    na.omit() %>% 
    mutate(school_id = fct_reorder(school_id, score_pct_change, .fun = 'mean'))


# a) Number of schools that participated 
(num_schools = length(unique(big_word_club$school_id)))

# b) Range in the number of student participants per school
big_word_club %>% 
    group_by(school_id) %>% 
    summarise(count = n()) %>% 
    summarise(min_student = min(count),
              max_student = max(count))

# c) On average, at which school did students exhibit the greatest and least
# improvement

big_word_club %>% 
    group_by(school_id) %>% 
    summarise(mean_score_change = mean(score_pct_change)) %>% 
    arrange(mean_score_change) %>% 
    slice(1, num_schools)

# d) Construct and discuss a plot which illustrates the variability in
# score_pct_change within and between schools.

school_changes <- big_word_club %>% 
    group_by( school_id ) %>% 
    summarize( mean_pct_change = mean(score_pct_change) )

options(repr.plot.width = 15, repr.plot.height = 5)

school_changes %>% 
    ggplot(aes(x = school_id, y = mean_pct_change)) +
    geom_point() +
    theme_minimal()

big_word_club %>% 
    ggplot(aes(x = school_id, score_pct_change)) +
    geom_boxplot()


big_word_club %>% 
    ggplot(aes(x = school_id, score_pct_change)) +
    geom_violin()

# > 16.7 ----

big_word_club %>% glimpse()


# > 16.8 ----
big_word_hierarchical = stan_glmer(
    score_pct_change ~ (1 | school_id),
    data = big_word_club, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

prior_summary(big_word_hierarchical)

mcmc_trace(big_word_hierarchical)
mcmc_dens_overlay(big_word_hierarchical)
mcmc_acf(big_word_hierarchical)
neff_ratio(big_word_hierarchical)
rhat(big_word_hierarchical)

# 100 posterior simulated datasets of song popularity along with the actual
pp_check(big_word_hierarchical)

# Store the simulation in a data.frame
big_word_hierarchical_df = as.data.frame(big_word_hierarchical)

# Check the first 3 and last 3 parameter labels
total_params = nlevels(big_word_club$school_id) + 3
total_params_less_3 = total_params - 2

big_word_hierarchical_df %>% 
    colnames() %>% 
    as.data.frame() %>% 
    slice(1:3, 27:29)

# > 16.9 ----
# Global effects
# Posterior summary of mu 
tidy(big_word_hierarchical, effects = "fixed",
     conf.int = TRUE, conf.level = 0.80)

# Posterior medians for sigma_y and sigma_mu
tidy(big_word_hierarchical, effects = "ran_pars")

sigma_table <- tidy(big_word_hierarchical, effects = "ran_pars")
sigma_table

sigma_mu <- sigma_table$estimate[[1]]
sigma_y <- sigma_table$estimate[[2]]

# Variance explained by within group variation
sigma_y^2 / (sigma_mu^2 + sigma_y^2)

# Variance explain by between group variation
sigma_mu^2 / (sigma_mu^2 + sigma_y^2)

# Most of the variance is explained by within school variation

# > 16.10 ----
school_summary <- tidy(big_word_hierarchical, 
                       effects = "ran_vals", 
                       conf.int = TRUE, 
                       conf.level = 0.80)
head( school_summary )

# Get MCMC chains for each mu_j
school_chains <- big_word_hierarchical %>%
    spread_draws(`(Intercept)`, b[,school_id]) %>% 
    mutate(mu_j = `(Intercept)` + b) 

head( school_chains )

# Get posterior summaries for each schools's mean score mu_j
school_summary_scaled = school_chains %>% 
    select(-`(Intercept)`, -b) %>% 
    mean_qi(.width = 0.8) %>% 
    mutate(school_id = fct_reorder(school_id, mu_j))

# Check out the results
school_summary_scaled %>% 
    select(school_id, mu_j, .lower, .upper) %>% 
    head(4)

# 80% credible intervals for each school's mean score
ggplot(school_summary_scaled,
       aes(x = school_id, y = mu_j, ymin = .lower, ymax = .upper)) +
    geom_pointrange() +
    xaxis_text(angle = 90, hjust = 1)

# Credible interval for mu_10
school_chains %>% 
    filter(school_id == "school_id:10") %>% 
    select(b, mu_j) %>% 
    mean_qi(.width = 0.8)

school_chains %>%
    filter(school_id == "school_id:10") %>%
    summarize(conf.low = quantile(mu_j, 0.1),
              conf.high = quantile(mu_j, 0.9),
              mu_lt_5 = mean(mu_j <= 5),
              mu_gt_5 = mean(mu_j > 5),
              odds_gt_5 = mu_gt_5/mu_lt_5)

# There is only weak evidence that the improvement is greater than 5% for school 10

# > 16.11 Making predictions ----
# Manual calculation of prediction
# >> Prediction for observed group ----
big_word_hierarchical_df = as.data.frame(big_word_hierarchical)

big_word_hierarchical_df %>% colnames()

num_row = nrow(big_word_hierarchical_df)

school_6_chains = big_word_hierarchical_df %>% 
    rename(b = `b[(Intercept) school_id:6]`) %>% 
    select(`(Intercept)`, b, sigma) %>% 
    mutate(mu_school_6 = `(Intercept)` + b,
           y_school_6 = rnorm(num_row, mean = mu_school_6, sd = sigma))

head(school_6_chains, 3)

school_6_chains %>% 
    mean_qi(y_school_6, .width = 0.80)

# Posterior summary of mu_j
school_summary_scaled %>% 
    filter(school_id == "school_id:6")

# For an existing school a prediction for a new student at that school is 
# basically the posterior mean for that school.

# >> Prediction for unobserved group ----
set.seed(84735)
bayes_prep_chains = big_word_hierarchical_df %>% 
    mutate(sigma_mu = sqrt(`Sigma[school_id:(Intercept),(Intercept)]`),
           mu_bayes_prep = rnorm(num_row, `(Intercept)`, sigma_mu),
           y_bayes_prep = rnorm(num_row, mu_bayes_prep, sigma))

# Posterior predictive summaries
bayes_prep_chains %>% 
    select(`(Intercept)`, contains("bayes_prep")) %>% head(5)

bayes_prep_chains %>% 
    mean_qi(y_bayes_prep, .width = 0.80)

# > Prediction shortcut ----
prediction_shortcut <- posterior_predict(
    big_word_hierarchical,
    newdata = data.frame(school_id = c("6", "17", "Bayes Prep"))
)

head( prediction_shortcut )

mcmc_areas(prediction_shortcut, prob = 0.8) + ggplot2::scale_y_discrete(labels = c("6", "17", "Bayes Prep"))


# > 16.12 shrinkage toward the global mean----
school_means = big_word_club %>% 
    group_by(school_id) %>% 
    summarise(count = n(), score_pct_change = mean(score_pct_change))

set.seed(84735)
predictions_big_word_hierarchical = posterior_predict(
    big_word_hierarchical,
    newdata = school_means)

predictions_big_word_hierarchical

overall_mean_score = mean(big_word_club$score_pct_change)


ppc_intervals(school_means$score_pct_change, 
              yrep = predictions_big_word_hierarchical,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = school_means$school_id,
                                breaks = 1:nrow(school_means)) +
    xaxis_text(angle = 90, hjust = 1) +
    # Average score across all students
    geom_hline(yintercept = overall_mean_score, linetype = "dashed") +
    labs(
        title = "There is a large degree of shrinkage toward the mean across schools",
        subtitle = "This is likely due to the very large variance in scores within schools"
    )

# > 16.13 ----

data("voices")

voices %>% glimpse()

voices = voices %>% 
    mutate(subject = fct_reorder(subject, pitch, .fun = 'mean', .na_rm = TRUE))

overall_mean_pitch = mean(voices$pitch, na.rm = TRUE)

pitch_means = voices %>% 
    group_by(subject) %>% 
    summarise(
        count = n(),
        pitch = mean(pitch, na.rm = TRUE)
    )

pitch_hierarchical = stan_glmer(
    pitch ~ (1 | subject),
    data = voices, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

# Parameter distribution

# Mean global pitch
tidy(pitch_hierarchical, 
     effects = "fixed", 
     conf.int = TRUE, 
     conf.level = 0.95)

sigma_table <- tidy(pitch_hierarchical, 
                    effects = "ran_pars")
sigma_table

sigma_mu <- sigma_table$estimate[[1]]
sigma_y <- sigma_table$estimate[[2]]

# Variance explained by within group variation
sigma_y^2 / (sigma_mu^2 + sigma_y^2)

# Variance explained by between group variation
sigma_mu^2 / (sigma_mu^2 + sigma_y^2)


pp_check(pitch_hierarchical)

# >> Posterior range for parameters by group - Manual ----
voice_chains <- pitch_hierarchical %>%
    spread_draws(`(Intercept)`, b[,subject]) %>% 
    mutate(mu_j = `(Intercept)` + b) 

voice_summary_scaled <- voice_chains %>% 
    select(-`(Intercept)`, -b) %>% 
    mean_qi(.width = 0.80) %>% 
    mutate(subject = fct_reorder(subject, mu_j))

ggplot(voice_summary_scaled, 
       aes(x = subject, y = mu_j, ymin = .lower, ymax = .upper)) +
    geom_pointrange() +
    xaxis_text(angle = 90, hjust = 1)

# >> Posterior range for parameters using posterior predict ----
predictions_voices = posterior_predict(
    pitch_hierarchical,
    newdata = pitch_means)

ppc_intervals(pitch_means$pitch, 
              yrep = predictions_voices,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = pitch_means$subject,
                                breaks = 1:nrow(pitch_means)) +
    xaxis_text(angle = 90, hjust = 1) +
    # Average score across all students
    geom_hline(yintercept = overall_mean_pitch, linetype = "dashed") +
    labs(
        title = "There is very little shrinkage toward the mean across subjects",
    )
