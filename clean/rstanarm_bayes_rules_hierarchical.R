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

# number of artists
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

# Posterior simulation
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

# Posterior analysis of global parameters ----

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

# Posterior analysis and group-specific parameters ----
artist_summary = tidy(spotify_hierarchical, effects = "ran_vals",
                      conf.int = TRUE, conf.level = 0.80)

artist_summary %>% 
    select(level, conf.low, conf.high) %>% 
    slice(1:2, 43:44) 

# There's an 80% chance that Camilo's mean popularity rating is between 19.4 and 32.4 above
# that of the average artist.  Likewise, there's an 80% chance that Mia X's mean popularity
# rating is between 23.3 and 40.7 below the popularity of the average artist.

# Directly simulate posterior models for the artist-specific means ----

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
