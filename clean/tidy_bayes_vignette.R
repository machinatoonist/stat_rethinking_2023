# See https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
# For tutorial by Matthew Kay

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(distributional)
library(here)


theme_set(theme_tidybayes() + panel_border())

# Help stan run faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(2022)
n <- 100
temp <- rnorm(n)
shark <- rnorm(n, temp)
ice_cream <- rnorm(n, temp)

spur_exp <- tibble(ice_cream, temp, shark) %>%
    mutate(across(everything(), standardize))

ggplot(data = spur_exp,  aes(x = ice_cream, y = shark)) +
    geom_point()
pairs(spur_exp)

mod_t <- brm(ice_cream ~ 1 + temp, data = spur_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m1-t"))

summary(mod_t)

mod_s <- brm(ice_cream ~ 1 + shark, data = spur_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m1-s"))

summary(mod_s)

least_squares_model = lm(ice_cream ~ temp + shark, data = spur_exp)
summary(least_squares_model)

mod_all <- brm(ice_cream ~ 1 + temp + shark, data = spur_exp, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = here("fits", "chp5", "b5m1-all"))

summary(mod_all)

# Get variables ----
get_variables(mod_all)

extract_samples = mod_all %>% 
    spread_draws(b_temp, b_shark, ndraws = 1000) 

extract_samples %>% 
    ggplot(aes(x = b_temp)) +
    geom_density()

extract_samples %>% 
    ggplot(aes(x = b_shark)) +
    geom_density()

# Simple hierarchical model with shrinkage towards a global mean: ----
# To demonstrate tidybayes, we will use a simple dataset with 10 observations from 5 conditions each:
    
set.seed(5)
n = 10
n_condition = 5
ABC =
    tibble(
        condition = rep(c("A","B","C","D","E"), n),
        response = rnorm(n * 5, c(0,1,2,1,-1), 0.5)
    )

head(ABC)

ABC %>%
    ggplot(aes(y = condition, x = response)) +
    geom_point()

m = brm(
    response ~ (1|condition), 
    data = ABC, 
    prior = c(
        prior(normal(0, 1), class = Intercept),
        prior(student_t(3, 0, 1), class = sd),
        prior(student_t(3, 0, 1), class = sigma)
    ),
    control = list(adapt_delta = .99),
    
    file = here("fits", "chp5", "tidy-brms_m")
)

get_variables(m)


extract_samples = mod_all %>% 
    spread_draws(b_temp, b_shark, ndraws = 1000) 

m %>%
    spread_draws(r_condition[condition,term]) %>%
    head(10)

# Can label the columns anything you like
m %>%
    spread_draws(r_condition[c,t]) %>%
    head(10)

# There only Intercept in the term column so we can drop that column
m %>%
    spread_draws(r_condition[condition,]) %>%
    head(10)

# If you have used spread_draws() with a raw sample from Stan or JAGS, you may be
# used to using recover_types before spread_draws() to get index column values back 
# (e.g. if the index was a factor). This is not necessary when using spread_draws() 
# on rstanarm models, because those models already contain that information in their 
# variable names. For more on recover_types, see vignette("tidybayes").

m %>%
    spread_draws(b_Intercept, sigma) %>%
    head(10)

# Point summaries and intervals ----

m %>%
    spread_draws(b_Intercept, sigma) %>%
    median_qi(b_Intercept, sigma, .width = c(.055, 0.5, 0.89))


