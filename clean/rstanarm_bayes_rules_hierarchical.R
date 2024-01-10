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
