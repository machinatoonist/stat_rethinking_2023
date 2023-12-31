library(rstanarm)
library(dplyr)
library(tidyr)
library(ggplot2)

data("rugged")
dd <- rugged
dd$log_gdp <- log(dd$rgdppc_2000)
dd <- dd[complete.cases(dd$rgdppc_2000), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

# Make variable to index Africa (1) or not (2) ----
dd$cid = ifelse(dd$cont_africa == 1, 1, 2)

m8.3rstan <- stan_glmer(
    log_gdp_std ~ rugged_std * cid + (rugged_std | cid),
    data = dd,
    family = gaussian(),
    prior = normal(0, 2.5),
    prior_intercept = normal(0, 2.5),
    seed = 123
)

summary(m8.3rstan)

library(loo)
loo_m8.3rstan <- loo(m8.3rstan)

plot(m8.3rstan)

# Create a new data frame for predictions
new_data <- expand.grid(cid = unique(dd$cid), rugged_std = seq(0, 1, length.out = 100))

# Get predictions
preds <- posterior_predict(m8.3rstan, newdata = new_data, type = "response")

# Calculate the mean of the predictions
preds_mean <- apply(preds, 2, mean)

cbind(new_data, preds_mean)

# Plot
ggplot(new_data, aes(x = rugged_std, y = preds_mean)) + 
    geom_line() + 
    facet_wrap(~ cid)

# Create a new data frame for predictions
new_data <- expand.grid(cid = unique(dd$cid), rugged_std = seq(0, 1, length.out = 100))

# Get predictions
preds <- posterior_epred(m8.3rstan, newdata = new_data, transform = TRUE)

# Calculate the mean of the predictions
preds_mean <- apply(preds, 2, mean)


