library(rethinking)
library(dplyr)
library(readr)
library(here)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)
library(brms)
library(glue)
library(loo)
library(ggridges)
library(tidybayes)


sppnames = c("afarensis", "africanus", "habilis", "boisei",
             "rudolfensis", "ergaster", "sapiens")

brainvolcc = c(438, 452, 612, 521, 752, 871, 1358)

masskg = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)

d = data.frame(species = sppnames, brain = brainvolcc, mass = masskg)

d$mass_std = (d$mass - mean(d$mass))/sd(d$mass)
d$brain_std = d$brain / max(d$brain)

# Use a log normal prior for sigma to keep it positive

m7.1 = quap(
    alist(
        brain_std ~ dnorm(mu, exp(log_sigma)),  # Using exp(log_sigma) so the result is always positive
        mu <- a + b * mass_std,
        a ~ dnorm(0.5, 1),
        b ~ dnorm(0, 10),
        log_sigma ~ dnorm(0, 1)
    ), data = d
)

set.seed(12)
s = sim(m7.1)
r = apply(s, 2, mean) - d$brain_std

resid_var = var2(r)
freq_var = var(r)
outcome_var = var2(d$brain_std)
1 - resid_var/outcome_var

R2_is_bad = function(quap_fit) {
    # set.seed(12)
    s = sim(quap_fit, refresh = 0)
    r = apply(s, 2, mean) - d$brain_std
    1 - var2(r)/var2(d$brain_std)
}

R2_is_bad(m7.1)

m7.2 = quap(
    alist(
        brain_std ~ dnorm(mu, exp(log_sigma)),
        mu <- a + b[1] * mass_std + b[2] * mass_std ^ 2,
        a ~ dnorm(0.5, 1),
        b ~ dnorm(0, 10),
        log_sigma ~ dnorm(0, 1)
    ), data = d, start = list(b = rep(0, 2))
)
m7.2
precis(m7.2, depth = 2)

m7.3 = quap(
    alist(
        brain_std ~ dnorm(mu, exp(log_sigma)),
        mu <- a + b[1] * mass_std + b[2] * mass_std ^ 2 +
            b[3] * mass_std ^ 3,
        a ~ dnorm(0.5, 1),
        b ~ dnorm(0, 10),
        log_sigma ~ dnorm(0, 1)
    ), data = d, start = list(b = rep(0, 3))
)
precis(m7.3, depth = 2)

m7.4 = quap(
    alist(
        brain_std ~ dnorm(mu, exp(log_sigma)),
        mu <- a + b[1] * mass_std + b[2] * mass_std ^ 2 +
            b[3] * mass_std ^ 3 + b[4] * mass_std ^ 4,
        a ~ dnorm(0.5, 1),
        b ~ dnorm(0, 10),
        log_sigma ~ dnorm(0, 1)
    ), data = d, start = list(b = rep(0, 4))
)
precis(m7.4, depth = 2)

m7.5 = quap(
    alist(
        brain_std ~ dnorm(mu, exp(log_sigma)),
        mu <- a + b[1] * mass_std + b[2] * mass_std ^ 2 +
            b[3] * mass_std ^ 3 + b[4] * mass_std ^ 4 +
            b[5] * mass_std ^ 5,
        a ~ dnorm(0.5, 1),
        b ~ dnorm(0, 10),
        log_sigma ~ dnorm(0, 1)
    ), data = d, start = list(b = rep(0, 5))
)
precis(m7.5, depth = 2)

m7.6 = quap(
    alist(
        brain_std ~ dnorm(mu, exp(log_sigma)),
        mu <- a + b[1] * mass_std + b[2] * mass_std ^ 2 +
            b[3] * mass_std ^ 3 + b[4] * mass_std ^ 4 +
            b[5] * mass_std ^ 5 + b[6] * mass_std ^ 6,
        a ~ dnorm(0.5, 1),
        b ~ dnorm(0, 10),
        log_sigma ~ dnorm(0, 1)
    ), data = d, start = list(b = rep(0, 6))
)
precis(m7.6, depth = 2)

post = extract.samples(m7.1)
mass_seq = seq(from = min(d$mass_std), to = max(d$mass_std), length.out = 100)
l = link(m7.1, data = list(mass_std = mass_seq))

plot_fit = function(model) {
    par(mar = c(4, 4, 2, 2))
    post = extract.samples(model)
    mass_seq = seq(from = min(d$mass_std), to = max(d$mass_std), length.out = 100)
    l = link(model, data = list(mass_std = mass_seq))
    mu = apply(l, 2, mean)
    ci = apply(l, 2, PI)
    plot(brain_std ~ mass_std, data = d)
    lines(mass_seq, mu)
    shade(ci, mass_seq)
    print(R2_is_bad(model))
    
}

plot_fit(m7.1)
plot_fit(m7.2)

plot_fit(m7.3)

plot_fit(m7.4)

plot_fit(m7.5)

plot_fit(m7.6)

# Function refits the model by leaving one out and replots
brain_loo_plot(fit = m7.5)

# function (fit, atx = c(35, 47, 60), aty = c(450, 900, 1300), 
#           xlim, ylim, npts = 100) 
# {
#     post <- extract.samples(fit)
#     n <- dim(post$b)[2]
#     if (is.null(n)) 
#         n <- 1
#     if (missing(xlim)) 
#         xlim <- range(d$mass_std)
#     else xlim <- (xlim - mean(d$mass))/sd(d$mass)
#     if (missing(ylim)) 
#         ylim <- range(d$brain_std)
#     else ylim <- ylim/max(d$brain)
#     plot(d$brain_std ~ d$mass_std, xaxt = "n", yaxt = "n", xlab = "body mass (kg)", 
#          ylab = "brain volume (cc)", col = rangi2, pch = 16, 
#          xlim = xlim, ylim = ylim)
#     axis_unscale(1, atx, d$mass)
#     axis_unscale(2, at = aty, factor = max(d$brain))
#     d <- as.data.frame(fit@data)
#     for (i in 1:nrow(d)) {
#         di <- d[-i, ]
#         m_temp <- quap(fit@formula, data = di, start = list(b = rep(0, 
#                                                                     n)))
#         xseq <- seq(from = xlim[1] - 0.2, to = xlim[2] + 0.2, 
#                     length.out = npts)
#         l <- link(m_temp, data = list(mass_std = xseq), refresh = 0)
#         mu <- apply(l, 2, mean)
#         lines(xseq, mu, lwd = 2, col = col.alpha("black", 0.3))
#     }
#     model_name <- deparse(match.call()[[2]])
#     mtext(model_name, adj = 0)
# }

# Information entropy ----
# Suppose there is a 0.7 probability of sunshine and 0.3 probability of rain
get_entropy = function(p) {
    -sum(p * log(p))
}

p = c(0.3, 0.7)

get_entropy(p)

# Adding more potential outcomes increases entropy
# p(rain) = 0.15, p(snow) = 0.15, p(sunshine) = 0.7
p = c(0.15, 0.15, 0.7)

get_entropy(p)

# Reducing uncertainty reduces entropy
# p(sun) = 0.99, p(rain) = 0.01
p = c(0.99, 0.01)

get_entropy(p)

# Log Pointwise Predictive Density ----

set.seed(1)

lppd(m7.1, n = 1e4)

N = 20
kseq = 1:5
dev = sapply(kseq, function(k) {
    print(k);
    r = replicate(1e4, sim_train_test(N = N, k = k));
    c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})
dev

# Widely Application Information Criteria (WAIC) ----
data(cars)
library(dplyr)
cars %>% glimpse()
m = quap(
    alist(
        dist ~ dnorm(mu, sigma),
        mu <- a + b * speed,
        a ~ dnorm(0, 10),
        b ~ dnorm(0, 10),
        sigma ~ dexp(1)
    ), data = cars
)
m

set.seed(94)
post = extract.samples(m, n = 1000)
post

# log-likelihood of each observation i at each sample s from the posterior
# 
# 1. Compare the actual data (`cars$dist`) to the model predictions (`mu`). 
# `mu` is computed for each car's speed, using the parameters (`a` and `b`) 
# drawn from the posterior distribution.
# 
# 2. **Standard Deviation `post$sigma[s]`**: This represents the model's 
# uncertainty about the distance (`dist`) for a given speed. Each `s` 
# represents a different set of parameters drawn from the posterior, 
# reflecting different plausible states of the world according to your model.
# 
# 3. **Likelihood Instead of Distance Measurement**: Instead of using a metric 
# like RMSE (Root Mean Square Error), which measures the distance between 
# observed data and model predictions, you're calculating the likelihood. 
# This likelihood quantifies how probable the observed distances are given 
# the model's predictions (for mean `mu`) and uncertainty (`sigma`).
# 
# 4. **Calculating Likelihood at Each Point**: Calculate the 
# likelihood for each observed distance given the model parameters. 
# This is done using the `dnorm()` function, which gives the probability 
# density of observing each specific `dist` value under a normal distribution 
# defined by `mu` and `sigma`.
# 
# 5. **Log-Likelihood**: Finally, taking the log of this likelihood results 
# in the log-likelihood. This transformation is beneficial for numerical 
# stability and simplifies calculations, especially when multiplying many 
# probabilities together (as in the likelihood of observing an entire dataset).
# 
# In essence, this process evaluates how well your model (with its specific 
# parameters for mean and standard deviation) explains the observed data. Each 
# set of parameters from the posterior distribution gives a slightly different 
# perspective on this fit, and the log-likelihoods you calculate provide a 
# quantifiable measure of this fit.

n_samples = 1000
logprob = sapply(1:n_samples,
                 function(s) {
                     mu = post$a[s] + post$b * cars$speed
                     # Calculate the log-likelihood of each point
                     dnorm(cars$dist, mu, post$sigma[s], log = TRUE)
                 })

n_cases = nrow(cars)
# 
lppd = sapply(1:n_cases, function(i) log_sum_exp(logprob[i,]) - log(n_samples))

sum(lppd)

pWAIC = sapply(1:n_cases, function(i) var(logprob[i,]))

# Compute WAIC
-2 * ( sum(lppd) - sum(pWAIC))

# Compute the standard error
waic_vec = -2 * (lppd - pWAIC)
sqrt(n_cases * var(waic_vec))

# Comparing models ----
library(rethinking)
data("WaffleDivorce")
d = WaffleDivorce

d$A = standardize(d$MedianAgeMarriage)
d$D = standardize(d$Divorce)
d$M = standardize(d$Marriage)

m5.1 = quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bA * A,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

m5.2 = quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bM * M,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

m5.3 = quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bM * M + bA * A,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

set.seed(24071847)
compare(m5.1, m5.2, m5.3)

compare(m5.1, m5.2, m5.3, func = PSIS)

set.seed(24071847)
PSIS_m5.3 = PSIS(m5.3, pointwise = TRUE)

set.seed(24071847)
WAIC_m5.3 = WAIC(m5.3, pointwise = TRUE)

plot(PSIS_m5.3$k, WAIC_m5.3$penalty, xlab = "PSIS Pareto k",
     ylab = "WAIC penalty", col = rangi2, lwd = 2)

# Re-estimate divorce model using the Student-t distribution with v = 2
m5.3t = quap(
    alist(
        D ~ dstudent(nu = 2, mu, sigma),
        mu <- a + bM * M + bA * A,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

summary(m5.3)
summary(m5.3t)

plot(PSIS_m5.3$k, WAIC_m5.3$penalty, xlab = "PSIS Pareto k",
     ylab = "WAIC penalty", col = rangi2, lwd = 2)


compare(m5.1, m5.2, m5.3, m5.3t, func = PSIS)

set.seed(24071847)
PSIS_m5.3t = PSIS(m5.3t, pointwise = TRUE)

set.seed(24071847)
WAIC_m5.3t = WAIC(m5.3t, pointwise = TRUE)

plot(PSIS_m5.3t$k, WAIC_m5.3t$penalty, xlab = "PSIS Pareto k",
     ylab = "WAIC penalty", col = rangi2, lwd = 2)

# End of chapter practice ----
# 7.2
library(purrr)
p_logp <- function(p) {
    if (p == 0) return(0)
    p * log(p)
}
calc_entropy <- function(x) {
    avg_logprob <- sum(map_dbl(x, p_logp))
    - 1 * avg_logprob
}

?map_dbl()
calc_entropy(c(0.7, 0.3))

# 7.3
probs <- c(0.20, 0.25, 0.25, 0.30)
calc_entropy(probs)

# 7.4
probs <- c(1, 1, 1, 0)
probs <- probs / sum(probs)
probs

calc_entropy(probs)

# 7M3
gen_data <- function(n) {
    tibble(x1 = rnorm(n = n)) %>%
        mutate(y = rnorm(n = n, mean = 0.3 + 0.8 * x1),
               across(everything(), standardize))
}

library(ggplot2)
library(brms)
library(here)
library(tibble)
library(tidyr)
library(readr)
library(ggridges)
library(forcats)
library(stringr)


tibble(x1 = rnorm(n = 100) ) %>% 
    mutate(y = rnorm(n = 100, mean = 0.3 + 0.8 * x1)) %>% 
    ggplot(aes(x1, y)) +
    geom_point()

fit_model <- function(dat) {
    suppressMessages(output <- capture.output(
        mod <- brm(y ~ 1 + x1, data = dat, family = gaussian,
                   prior = c(prior(normal(0, 0.2), class = Intercept),
                             prior(normal(0, 0.5), class = "b"),
                             prior(exponential(1), class = sigma)),
                   iter = 4000, warmup = 3000, chains = 4, cores = 6, seed = 1234)
    ))
    
    return(mod)
}

d = gen_data(100)
m.7m3 = fit_model(d)
summary(m.7m3)

calc_info <- function(model) {
    # Compute the Pointwise Log-Likelihood
    mod_lppd <- log_lik(model) %>% 
        as_tibble(.name_repair = "minimal") %>% 
        set_names(paste0("obs_", 1:ncol(.))) %>% 
        rowid_to_column(var = "iter") %>% 
        pivot_longer(-iter, names_to = "obs", values_to = "logprob") %>% 
        mutate(prob = exp(logprob)) %>% 
        group_by(obs) %>% 
        summarize(log_prob_score = log(mean(prob))) %>% 
        pull(log_prob_score) %>% 
        sum()
    
    mod_waic <- suppressWarnings(waic(model)$estimates["waic", "Estimate"])
    mod_psis <- suppressWarnings(loo(model)$estimates["looic", "Estimate"])
    
    tibble(lppd = mod_lppd, waic = mod_waic, psis = mod_psis)
}

log_lik(m.7m3) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    set_names(paste0("obs_", 1:ncol(.))) %>% 
    rowid_to_column(var = "iter") %>% 
    pivot_longer(-iter, names_to = "obs", values_to = "logprob") %>% 
    mutate(prob = exp(logprob)) %>% 
    group_by(obs) %>% 
    summarize(log_prob_score = log(mean(prob))) %>% 
    pull(log_prob_score) %>% 
    sum()

??rowid_to_column()
?log_lik()

calc_info(m.7m3)

sample_sim <- tibble(sample_size = rep(c(100, 500, 1000), each = 1)) %>%
    mutate(sample_data = map(sample_size, gen_data),
           model = map(sample_data, fit_model),
           infc = map(model, calc_info)) %>%
    select(-sample_data, -model) %>% 
    unnest(infc) %>% 
    write_rds(here("fits", "chp7", "b7m3-sim.rds"), compress = "gz")

sample_sim %>%
    pivot_longer(cols = c(lppd, waic, psis)) %>%
    mutate(sample_size = fct_inorder(as.character(sample_size)),
           name = str_to_upper(name),
           name = fct_inorder(name)) %>%
    ggplot(aes(x = value, y = sample_size)) +
    geom_col() +
    facet_wrap(~name, ncol = 1) +
    # facet_wrap(~name, nrow = 1, scales = "free_x") +
    # geom_density_ridges(bandwidth = 4) +
    scale_y_discrete(expand = c(0, .1)) +
    scale_x_continuous(breaks = seq(-2000, 3000, by = 750)) +
    coord_cartesian(clip = "off") +
    labs(x = "Value", y = "Sample Size") +
    theme_minimal()

# 7M4 ----
gen_data <- function(n) {
    tibble(x1 = rnorm(n = n),
           x2 = rnorm(n = n),
           x3 = rnorm(n = n)) %>%
        mutate(y = rnorm(n = n, mean = 0.3 + 0.8 * x1 +
                             0.6 * x2 + 1.2 * x3),
               across(everything(), standardize))
}

fit_model <- function(dat, p_sd) {
    suppressMessages(output <- capture.output(
        mod <- brm(y ~ 1 + x1 + x2 + x3, data = dat,
                   family = gaussian,
                   prior = c(prior(normal(0, 0.2), class = Intercept),
                             prior_string(glue("normal(0, {p_sd})"), class = "b"),
                             prior(exponential(1), class = sigma)),
                   iter = 4000, warmup = 3000, chains = 4, cores = 4, seed = 1234)
    ))
    
    return(mod)
}

calc_info <- function(model) {
    w <- suppressWarnings(brms::waic(model))
    p <- suppressWarnings(brms::loo(model))
    
    tibble(p_waic = w$estimates["p_waic", "Estimate"],
           p_loo = p$estimates["p_loo", "Estimate"])
}


prior_sim <- tibble(sample_size = 20,
                    prior_sd = rep(c(0.1, 1, 2), each = 1)) %>% 
    mutate(sample_data = map(sample_size, gen_data),
           model = map2(sample_data, prior_sd, fit_model),
           infc = map(model, calc_info)
           ) %>%
    select(-sample_data, -model) %>% 
    unnest(infc) 
    # write_rds(here("fits", "chp7", "b7m4-sim.rds"), compress = "gz")

prior_sim %>%
    pivot_longer(cols = c(p_waic, p_loo)) %>%
    mutate(prior_sd = glue("&sigma; = {prior_sd}"),
           prior_sd = fct_inorder(prior_sd),
           name = factor(name, levels = c("p_waic", "p_loo"),
                         labels = c("p<sub>WAIC</sub>", "p<sub>PSIS</sub>"))) %>%
    ggplot(aes(x = value, y = prior_sd, height = stat(density))) +
    facet_wrap(~name, nrow = 1) +
    geom_density_ridges(stat = "binline", bins = 20) +
    labs(x = "Effective Parameters", y = "Prior") 
    # theme(axis.text.y = element_markdown())

# 7H1 ----
data(Laffer)

laf_dat <- Laffer %>%
    mutate(across(everything(), standardize),
           tax_rate2 = tax_rate ^ 2)

laf_line <- brm(tax_revenue ~ 1 + tax_rate, data = laf_dat, family = gaussian,
                prior = c(prior(normal(0, 0.2), class = Intercept),
                          prior(normal(0, 0.5), class = b),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                file = here("fits", "chp7", "b7h1-line.rds"))

laf_quad <- brm(tax_revenue ~ 1 + tax_rate + tax_rate2, data = laf_dat,
                family = gaussian,
                prior = c(prior(normal(0, 0.2), class = Intercept),
                          prior(normal(0, 0.5), class = b),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                file = here("fits", "chp7", "b7h1-quad.rds"))

laf_spln <- brm(tax_revenue ~ 1 + s(tax_rate, bs = "bs"), data = laf_dat,
                family = gaussian,
                prior = c(prior(normal(0, 0.2), class = Intercept),
                          prior(normal(0, 0.5), class = b),
                          prior(normal(0, 0.5), class = sds),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                control = list(adapt_delta = 0.95),
                file = here("fits", "chp7", "bh71-spln.rds"))

tr_seq <- tibble(tax_rate = seq(0, 40, length.out = 100)) %>%
    mutate(tax_rate = (tax_rate - mean(Laffer$tax_rate)) / sd(Laffer$tax_rate),
           tax_rate2 = tax_rate ^ 2)

predictions <- bind_rows(
    predicted_draws(laf_line, newdata = tr_seq) %>%
        median_qi(.width = 0.89) %>%
        mutate(type = "Linear"),
    predicted_draws(laf_quad, newdata = tr_seq) %>%
        median_qi(.width = 0.89) %>%
        mutate(type = "Quadratic"),
    predicted_draws(laf_spln, newdata = tr_seq) %>%
        median_qi(.width = 0.89) %>%
        mutate(type = "Spline")
)

fits <- bind_rows(
    epred_draws(laf_line, newdata = tr_seq) %>%
        median_qi(.width = c(0.67, 0.89, 0.97)) %>%
        mutate(type = "Linear"),
    epred_draws(laf_quad, newdata = tr_seq) %>%
        median_qi(.width = c(0.67, 0.89, 0.97)) %>%
        mutate(type = "Quadratic"),
    epred_draws(laf_spln, newdata = tr_seq) %>%
        median_qi(.width = c(0.67, 0.89, 0.97)) %>%
        mutate(type = "Spline")
)

blue_colors <- c("#ADD8E6", "#87CEEB", "#4682B4")

ggplot() +
    facet_wrap(~type, nrow = 1) +
    geom_ribbon(data = predictions,
                aes(x = tax_rate, ymin = .lower, ymax = .upper),
                alpha = 0.2) +
    geom_lineribbon(data = fits,
                    aes(x = tax_rate, y = .epred, ymin = .lower, ymax = .upper),
                    linewidth = 0.6) +
    geom_point(data = laf_dat, aes(x = tax_rate, y = tax_revenue),
               alpha = 0.5) +
    scale_fill_manual(values = blue_colors) +
    # scale_fill_manual(values = ramp_blue(seq(0.9, 0.1, length.out = 3)),
    #                   breaks = c(0.67, 0.89, 0.97)) +
    labs(x = "Standardized Tax Rate", y = "Standardized Tax Revenue",
         fill = "Interval") +
    theme_minimal()

laf_line <- add_criterion(laf_line, criterion = c("loo", "waic"))
laf_quad <- add_criterion(laf_quad, criterion = c("loo", "waic"))
laf_spln <- add_criterion(laf_spln, criterion = c("loo", "waic"))

loo_compare(laf_line, laf_quad, laf_spln, criterion = "waic")

loo_compare(laf_line, laf_quad, laf_spln, criterion = "loo")

# 7H2 ----
library(gghighlight)

criteria_influence <- function(mod) {
    tibble(pareto_k = mod$criteria$loo$diagnostics$pareto_k,
           p_waic = mod$criteria$waic$pointwise[, "p_waic"]) %>%
        rowid_to_column(var = "obs")
}

influ <- bind_rows(
    criteria_influence(laf_line) %>%
        mutate(type = "Linear"),
    criteria_influence(laf_quad) %>%
        mutate(type = "Quadratic"),
    criteria_influence(laf_spln) %>%
        mutate(type = "Spline")
)

ggplot(influ, aes(x = pareto_k, y = p_waic)) +
    facet_wrap(~type, nrow = 1) +
    geom_vline(xintercept = 0.7, linetype = "dashed") +
    geom_hline(yintercept = 0.4, linetype = "dashed") +
    geom_point() +
    gghighlight(pareto_k > 0.7 | p_waic > 0.4, n = 1, label_key = obs,
                label_params = list(size = 3)) +
    labs(x = "Pareto *k*", y = "p<sub>WAIC</sub>")

laf_line2 <- brm(bf(tax_revenue ~ 1 + tax_rate, nu = 1),
                 data = laf_dat, family = student,
                 prior = c(prior(normal(0, 0.2), class = Intercept),
                           prior(normal(0, 0.5), class = b),
                           prior(exponential(1), class = sigma)),
                 iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                 file = here("fits", "chp7", "b7h2-line.rds"))

laf_quad2 <- brm(bf(tax_revenue ~ 1 + tax_rate + tax_rate2, nu = 1),
                 data = laf_dat, family = student,
                 prior = c(prior(normal(0, 0.2), class = Intercept),
                           prior(normal(0, 0.5), class = b),
                           prior(exponential(1), class = sigma)),
                 iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                 file = here("fits", "chp7", "b7h2-quad.rds"))

laf_spln2 <- brm(bf(tax_revenue ~ 1 + s(tax_rate, bs = "bs"), nu = 1),
                 data = laf_dat, family = student,
                 prior = c(prior(normal(0, 0.2), class = Intercept),
                           prior(normal(0, 0.5), class = b),
                           prior(normal(0, 0.5), class = sds),
                           prior(exponential(1), class = sigma)),
                 iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                 control = list(adapt_delta = 0.99),
                 file = here("fits", "chp7", "bh72-spln.rds"))

predictions <- bind_rows(
    predicted_draws(laf_line2, newdata = tr_seq) %>%
        median_qi(.width = 0.89) %>%
        mutate(type = "Linear"),
    predicted_draws(laf_quad2, newdata = tr_seq) %>%
        median_qi(.width = 0.89) %>%
        mutate(type = "Quadratic"),
    predicted_draws(laf_spln2, newdata = tr_seq) %>%
        median_qi(.width = 0.89) %>%
        mutate(type = "Spline")
)

fits <- bind_rows(
    epred_draws(laf_line2, newdata = tr_seq) %>%
        median_qi(.width = c(0.67, 0.89, 0.97)) %>%
        mutate(type = "Linear"),
    epred_draws(laf_quad2, newdata = tr_seq) %>%
        median_qi(.width = c(0.67, 0.89, 0.97)) %>%
        mutate(type = "Quadratic"),
    epred_draws(laf_spln2, newdata = tr_seq) %>%
        median_qi(.width = c(0.67, 0.89, 0.97)) %>%
        mutate(type = "Spline")
)

ggplot() +
    facet_wrap(~type, nrow = 1) +
    geom_ribbon(data = predictions,
                aes(x = tax_rate, ymin = .lower, ymax = .upper),
                alpha = 0.2) +
    geom_lineribbon(data = fits,
                    aes(x = tax_rate, y = .epred, ymin = .lower, ymax = .upper),
                    size = 0.6) +
    geom_point(data = laf_dat, aes(x = tax_rate, y = tax_revenue),
               alpha = 0.5) +
    scale_fill_manual(values = ramp_blue(seq(0.9, 0.1, length.out = 3)),
                      breaks = c(0.67, 0.89, 0.97)) +
    labs(x = "Standardized Tax Rate", y = "Standardized Tax Revenue",
         fill = "Interval")

laf_line2 <- add_criterion(laf_line2, criterion = c("loo", "waic"))
laf_quad2 <- add_criterion(laf_quad2, criterion = c("loo", "waic"))
laf_spln2 <- add_criterion(laf_spln2, criterion = c("loo", "waic"))

loo_compare(laf_line2, laf_quad2, laf_spln2, criterion = "waic")
loo_compare(laf_line2, laf_quad2, laf_spln2, criterion = "loo")

# 7H3 ----
islands <- tibble(island = paste("Island", 1:3),
                  a = c(0.2, 0.8, 0.05),
                  b = c(0.2, 0.1, 0.15),
                  c = c(0.2, 0.05, 0.7),
                  d = c(0.2, 0.025, 0.05),
                  e = c(0.2, 0.025, 0.05)) %>%
    pivot_longer(-island, names_to = "species", values_to = "prop")

islands %>%
    group_by(island) %>%
    summarize(prop = list(prop), .groups = "drop") %>%
    mutate(entropy = map_dbl(prop, calc_entropy))

d_kl <- function(p, q) {
    sum(p * (log(p) - log(q)))
}

crossing(model = paste("Island", 1:3),
         predicts = paste("Island", 1:3)) %>%
    filter(model != predicts) %>%
    left_join(islands, by = c("model" = "island")) %>%
    rename(model_prop = prop) %>%
    left_join(islands, by = c("predicts" = "island", "species")) %>%
    rename(predict_prop = prop) %>%
    group_by(model, predicts) %>%
    summarize(q = list(model_prop),
              p = list(predict_prop),
              .groups = "drop") %>%
    mutate(kl_distance = map2_dbl(p, q, d_kl))

# 7H4 ----
library(dagitty)
library(ggdag)

hma_dag <- dagitty("dag{H -> M <- A}")
coordinates(hma_dag) <- list(x = c(H = 1, M = 2, A = 3),
                             y = c(H = 1, M = 1, A = 1))

ggplot(hma_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_text(color = "black", size = 10) +
    geom_dag_edges(edge_color = "black", edge_width = 2,
                   arrow_directed = grid::arrow(length = grid::unit(15, "pt"),
                                                type = "closed")) +
    theme_void()

d <- sim_happiness(seed = 1977, N_years = 1000)
dat <- d %>%
    filter(age > 17) %>%
    mutate(a = (age - 18) / (65 - 18),
           mid = factor(married + 1, labels = c("single", "married")))

b6.9 <- brm(happiness ~ 0 + mid + a, data = dat, family = gaussian,
            prior = c(prior(normal(0, 1), class = b, coef = midmarried),
                      prior(normal(0, 1), class = b, coef = midsingle),
                      prior(normal(0, 2), class = b, coef = a),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = here("fits", "chp7", "b7h4-6.9"))

b6.10 <- brm(happiness ~ 1 + a, data = dat, family = gaussian,
             prior = c(prior(normal(0, 1), class = Intercept),
                       prior(normal(0, 2), class = b, coef = a),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp7", "b7h4-6.10"))

b6.9 <- add_criterion(b6.9, criterion = "loo")
b6.10 <- add_criterion(b6.10, criterion = "loo")

loo_compare(b6.9, b6.10)

adjustmentSets(hma_dag, exposure = "A", outcome = "H")

# 7H5 ----
data(foxes)

fox_dat <- foxes %>%
    as_tibble() %>%
    select(area, avgfood, weight, groupsize) %>%
    mutate(across(everything(), standardize))

b7h5_1 <- brm(weight ~ 1 + avgfood + groupsize + area, data = fox_dat,
              family = gaussian,
              prior = c(prior(normal(0, 0.2), class = Intercept),
                        prior(normal(0, 0.5), class = b),
                        prior(exponential(1), class = sigma)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
              file = here("fits", "chp7", "b7h5_1"))

b7h5_2 <- brm(weight ~ 1 + avgfood + groupsize, data = fox_dat,
              family = gaussian,
              prior = c(prior(normal(0, 0.2), class = Intercept),
                        prior(normal(0, 0.5), class = b),
                        prior(exponential(1), class = sigma)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
              file = here("fits", "chp7", "b7h5_2"))

b7h5_3 <- brm(weight ~ 1 + groupsize + area, data = fox_dat,
              family = gaussian,
              prior = c(prior(normal(0, 0.2), class = Intercept),
                        prior(normal(0, 0.5), class = b),
                        prior(exponential(1), class = sigma)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
              file = here("fits", "chp7", "b7h5_3"))

b7h5_4 <- brm(weight ~ 1 + avgfood, data = fox_dat,
              family = gaussian,
              prior = c(prior(normal(0, 0.2), class = Intercept),
                        prior(normal(0, 0.5), class = b),
                        prior(exponential(1), class = sigma)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
              file = here("fits", "chp7", "b7h5_4"))

b7h5_5 <- brm(weight ~ 1 + area, data = fox_dat,
              family = gaussian,
              prior = c(prior(normal(0, 0.2), class = Intercept),
                        prior(normal(0, 0.5), class = b),
                        prior(exponential(1), class = sigma)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
              file = here("fits", "chp7", "b7h5_5"))

b7h5_1 <- add_criterion(b7h5_1, criterion = "waic")
b7h5_2 <- add_criterion(b7h5_2, criterion = "waic")
b7h5_3 <- add_criterion(b7h5_3, criterion = "waic")
b7h5_4 <- add_criterion(b7h5_4, criterion = "waic")
b7h5_5 <- add_criterion(b7h5_5, criterion = "waic")

comp <- loo_compare(b7h5_1, b7h5_2, b7h5_3, b7h5_4, b7h5_5, criterion = "waic")
comp

plot_comp <- comp %>%
    as_tibble(rownames = "model") %>%
    mutate(across(-model, as.numeric),
           model = fct_inorder(model))

waic_val <- plot_comp %>%
    select(model, waic, se = se_waic) %>%
    mutate(lb = waic - se,
           ub = waic + se)

diff_val <- plot_comp %>%
    select(model, waic, se = se_diff) %>%
    mutate(se = se * 2) %>%
    mutate(lb = waic - se,
           ub = waic + se) %>%
    filter(se != 0)

ggplot() +
    geom_pointrange(data = waic_val, mapping = aes(x = waic, xmin = lb, xmax = ub,
                                                   y = fct_rev(model))) +
    geom_pointrange(data = diff_val, mapping = aes(x = waic, xmin = lb, xmax = ub,
                                                   y = fct_rev(model)),
                    position = position_nudge(y = 0.2), shape = 2,
                    color = "#009FB7") +
    labs(x = "Deviance", y = NULL)

