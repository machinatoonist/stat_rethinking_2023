# From https://sr2-solutions.wjakethompson.com/linear-models-causal-inference

library(rethinking)
library(brms)
library(tidyverse)
library(tidybayes)
library(tidyr)

data("Howell1", package = "rethinking")

full_how <- Howell1 %>%
    mutate(log_weight = log(weight),
           log_weight_c = log_weight - mean(log_weight))

full_how %>% glimpse()

# log_linear_fit = quap(
#     alist(
#         height ~ dnorm(mu, sigma),
#         mu <- a + b * (weight_l - xbar_l),
#         a ~ dnorm(20, 20),
#         b ~ dlnorm(0, 1),
#         sigma ~ dunif(0, 50)
#     ), data = d
# )

b4h3 <- brm(height ~ 1 + log_weight_c, data = full_how, family = gaussian,
            prior = c(prior(normal(158, 20), class = Intercept),
                      prior(lognormal(0, 1), class = b, lb = 0),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234)

summary(b4h3)

# the intercept estimate of 
sprintf("%0.1f", summary(b4h3)[["fixed"]]["Intercept", "Estimate"]) 
# represents the predicted average height for an individual with an average log-weight (log-kg). 

# The β estimate of 
sprintf("%0.1f", summary(b4h3)[["fixed"]]["log_weight_c", "Estimate"]) 
# represents the average expected increase in height associated with an one-unit increase in weight (log-kg).
paste(round(summary(b4h3)[["fixed"]]["log_weight_c", "Estimate"]/exp(1), 1), "cm/kg")

how_fits <- tibble(weight = seq(min(full_how$weight), max(full_how$weight), length.out = 100)) %>% 
    mutate(log_weight = log(weight),
           log_weight_c = log_weight - mean(full_how$log_weight)) %>%
    add_epred_draws(b4h3) %>%
    group_by(weight) %>%
    mean_qi(.epred, .width = 0.97)

how_preds <- tibble(weight = seq(min(full_how$weight), max(full_how$weight), length.out = 100)) %>% 
    mutate(log_weight = log(weight),
           log_weight_c = log_weight - mean(full_how$log_weight)) %>%
    add_predicted_draws(b4h3) %>%
    group_by(weight) %>%
    mean_qi(.prediction, .width = 0.97)

ggplot(full_how, aes(x = weight)) +
    geom_point(aes(y = height), alpha = 0.4) +
    geom_ribbon(data = how_preds, aes(ymin = .lower, ymax = .upper),
                alpha = 0.2) +
    geom_lineribbon(data = how_fits,
                    aes(y = .epred, ymin = .lower, ymax = .upper),
                    fill = "grey60", linewidth = 1) +
    labs(x = "Weight", y = "Height")


??seq_range()
