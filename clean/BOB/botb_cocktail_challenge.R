# Convergence is not uniform across the sample size
# Paul's paper
# Graphical methods - trace plots - label switching
library(brms)
library(tidyverse)
library(janitor)
library(mclust)


library(tidybayes)
library(tidyr)
library(bayesplot)
library(posterior)

library(forcats)
library(modelr)
library(ggdist)
library(ggplot2)
library(cowplot)
library(rstan)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(distributional)
library(rethinking)

theme_set(theme_tidybayes() + panel_border())

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores(), brms.backend = "rstan")


load("data/mixture_data.rda")

df %>% glimpse()

df %>% 
    tabyl(ingredient)

df %>% nrow()

df %>% 
    group_by(ingredient) %>% 
    summarise(mean_alc = mean(alcohol_by_volume),
              sd_alc = sd(alcohol_by_volume),
              relative_sd = sd_alc/mean_alc * 100)

df %>% 
    ggplot(aes(x = alcohol_by_volume)) +
    facet_wrap(~ingredient) +
    geom_density()

clust_model = Mclust(df$alcohol_by_volume)
summary(clust_model)

# Plotting the BIC to visualize the best number of clusters according to BIC
plot(clust_model)

# Define a mixture model with 3 Gaussian components
# Use CmdStan as the backend instead of RStan

df_small = sample_n(df, size = 5000, set.seed = 84735)

df_small %>% count(ingredient)

family = mixture(gaussian, gaussian, gaussian) 

# Fit the mixture model
# Divergent chains after warmup - increase adapt_delta default 0.8, increase max_treedepth default 10,
# iter increased to 4000 from 2000 default

df %>% glimpse()

df = df %>% 
    mutate(alc_std = standardize(alcohol_by_volume))

fit = brm(formula = alc_std ~ 1, 
          data = df, 
          family = family,
          chains = 4,
          control = list(adapt_delta = 0.8),
          iter = 4000, 
          warmup = 2000)

# Summary of the model
print(summary(fit))

get_variables(fit)

saveRDS(fit, "clean/BOB/cocktail_fit_std.rds")
# fit = readRDS("clean/BOB/cocktail_fit_adapt_delta_95.rds")

# fit = readRDS("clean/BOB/cocktail_fit_adapt_delta_95.rds")
# fit = readRDS("clean/BOB/cocktail_fit_adapt_delta_80.rds")

post_samples <- posterior_samples(fit)

fit$model

post_samples %>% names()

mcmc_trace(post_samples, pars = c("b_mu1_Intercept", 
                                  "b_mu2_Intercept", 
                                  "b_mu3_Intercept", 
                                  "sigma1",
                                  "sigma2",
                                  "sigma3"))

mcmc_rank_hist(post_samples, pars = c("b_mu1_Intercept", 
                                      "b_mu2_Intercept", 
                                      "b_mu3_Intercept"))


neff_ratio(fit)
rhat(fit)

mcmc_dens(post_samples, pars = c("b_mu1_Intercept", 
                                 "b_mu2_Intercept", 
                                 "b_mu3_Intercept"))

fit %>%
    spread_draws(b_mu1_Intercept, b_mu2_Intercept, b_mu3_Intercept) %>%
    head(10)

fit %>%
    spread_draws(b_mu1_Intercept, b_mu2_Intercept, b_mu3_Intercept) %>%
    median_qi(.width = 0.9)

fit %>%
    spread_draws(b_mu1_Intercept, b_mu2_Intercept, b_mu3_Intercept) %>%
    summarise_draws()

fit %>%
    spread_draws(b_mu1_Intercept, b_mu2_Intercept, b_mu3_Intercept) %>%
    median_qi( .width = c(.95, .66)) %>%
    ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() 

{
    set.seed(5)
    n = 10
    n_condition = 5
    ABC =
        tibble(
            condition = rep(c("A","B","C","D","E"), n),
            response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
        )
    
    m = brm(
        response ~ (1|condition), 
        data = ABC, 
        prior = c(
            prior(normal(0, 1), class = Intercept),
            prior(student_t(3, 0, 1), class = sd),
            prior(student_t(3, 0, 1), class = sigma)
        ),
        control = list(adapt_delta = .99),
        
        file = "models/tidy-brms_m.rds" # cache model (can be removed)  
    )
    }
