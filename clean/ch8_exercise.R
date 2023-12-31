library(rethinking)
library(dplyr)
library(tidyr)
library(ggplot2)


data("rugged")
d <- rugged

# Make log version of outcome
d$log_gdp = log(d$rgdppc_2000)
dim(d)

# extract countries with GDP data
dd = d[complete.cases(d$rgdppc_2000), ]
??complete.case()
dim(dd)

# rescale variables ----
# log GDP is divided by the average value so it is a proportion of the 
# international average.  1 means average, 0.8 means 80% of the average and
# 1.1 means 10% more than average
dd$log_gdp_std = dd$log_gdp / mean(dd$log_gdp)

# Scaling so 0 represents zero ruggedness and 1 represents the most rugged
dd$rugged_std = dd$rugged / max(dd$rugged)

# Fit model ----
m8.1 = quap(
    alist(
        log_gdp_std ~ dnorm( mu, sigma),
        mu <- a + b * (rugged_std - 0.215),
        a ~ dnorm(1, 1),
        b ~ dnorm(0, 1),
        sigma ~ dexp(1)
    ), data = dd
)

# Look at prior predictions ----
set.seed(7)
prior = extract.prior(m8.1)

# set up the plot dimensions
plot(NULL, xlim = c(0, 1), ylim = c(0.5, 1.5),
     xlab = "ruggedness", ylab = "log GDP")
abline(h = min(dd$log_gdp_std), lty = 2)
abline(h = max(dd$log_gdp_std), lty = 2)

# draw 50 lines from the prior
rugged_seq = seq(from = -0.1, to = 1.1, length.out = 30)
mu = link(m8.1, post = prior, data = data.frame(rugged_std = rugged_seq))
for (i in 1:50) lines( rugged_seq, mu[i,], col = col.alpha("black", 0.3))

# More than half of the slopes are over 0.6
sum(abs(prior$b) > 0.6) / length(prior$b)

# Modify the priors to get more plausible prior predictions

m8.1 = quap(
    alist(
        log_gdp_std ~ dnorm( mu, sigma),
        mu <- a + b * (rugged_std - 0.215),
        a ~ dnorm(1, 0.1),
        b ~ dnorm(0, 0.3),
        sigma ~ dexp(1)
    ), data = dd
)

# No overall association between terrain ruggedness and log GDP
precis(m8.1)

# Make variable to index Africa (1) or not (2) ----
dd$cid = ifelse(dd$cont_africa == 1, 1, 2)

# Fit model that indexes on continent ----
m8.2 = quap(
    alist(
        log_gdp_std ~ dnorm(mu, sigma),
        mu <- a[cid] + b * (rugged_std - 0.215),
        a[cid] ~ dnorm(1, 0.1),
        b ~ dnorm(0, 0.3),
        sigma ~ dexp(1)
    ), data = dd
)

compare(m8.1, m8.2)

precis(m8.2, depth = 2)


# Posterior contrast between two intercepts ----
post <- extract.samples(m8.2)
post %>% glimpse()

diff_a1_a2 = post$a[, 1] - post$a[, 2]
rethinking::PI(diff_a1_a2, prob = 0.9)

# Plot posterior predictions (retrodictions) ----
rugged.seq = seq(from = -0.1, to = 1.1, length.out = 30)

# compute mu over samples, fixing cid = 2 and then cid = 1
mu.NotAfrica = link(m8.2, data = data.frame(cid = 2, rugged_std = rugged.seq))

mu.Africa = link(m8.2, data = data.frame(cid = 1, rugged_std = rugged.seq))

# summarise to means and intervals
(mu.NotAfrica_mu = apply(mu.NotAfrica, 2, mean))
(mu.Africa_mu = apply(mu.Africa, 2, mean))

(mu.NotAfrica_ci = apply(mu.NotAfrica, 2, PI, prob = 0.97))
(mu.Africa_ci = apply(mu.Africa, 2, PI, prob = 0.97))

cbind(
    dd[dd$cid == 1, c("log_gdp_std", "rugged_std")],
    
)
dd[dd$cid == 1, c("log_gdp_std", "rugged_std")]

dd = 
    dd %>% 
    mutate(cont = ifelse(cid == 1, "Africa", "Not Africa"))

fit_data = 
    rbind(
        data.frame(rugged_std = rugged.seq,
                   mu = mu.Africa_mu, 
                   ci_low = mu.Africa_ci[1,],
                   ci_high = mu.Africa_ci[2,]) %>% 
            mutate(cont = "Africa"),
        data.frame(rugged_std = rugged.seq,
                   mu = mu.NotAfrica_mu,
                   ci_low = mu.NotAfrica_ci[1,],
                   ci_high = mu.NotAfrica_ci[2,]) %>% 
            mutate(cont = "Not Africa")
        )


fit_data %>% 
    ggplot(aes(x = rugged_std, y = mu, color = cont)) +
    geom_line() +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = cont), alpha = 0.2) +
    theme_minimal() +
    geom_point(data = dd, aes(x = rugged_std, y = log_gdp_std, color = cont), size = 2) +
    labs(
        title = "Regression of Ruggedness and GDP",
        x = "Ruggedness",
        y = "Log GDP (as proportion of mean)", 
        color = "Continent"
    ) +
    theme(legend.position = "bottom")

# Adding an interaction that works ----
m8.3 <-  quap(
    alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
    ), data = dd
)

precis(m8.3, depth = 2)

compare(m8.1, m8.2, m8.3, func = PSIS)

plot(PSIS(m8.3, pointwise = TRUE)$k)

# Plotting the interaction ----
# plot Africa - cid = 1
d.A1 <- dd[dd$cid == 1,]
plot(d.A1$rugged_std, d.A1$log_gdp_std, pch = 16, col = rangi2,
     xlab = "ruggedness (standardised)", ylab = "log GDP (as proportion of mean)",
     xlim = c(0, 1)
     )


mu <- link(m8.3, data = data.frame(cid = 1, rugged_std = rugged_seq))
mu_mean = apply(mu, 2, mean)
mu_ci = apply(mu, 2, PI, prob = 0.97)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_ci, rugged_seq, col = col.alpha(rangi2, 0.3))
mtext("African nations")

# plot non-African - cid = 2
d.A0 <- dd[dd$cid == 2,]
plot(d.A0$rugged_std, d.A0$log_gdp_std, pch = 1, col = "black",
     xlab = "ruggedness (standardised)", ylab = "log GDP (as proportion of mean)",
     xlim = c(0, 1))
mu = link(m8.3, data = data.frame(cid = 2, rugged_std = rugged_seq))
mu_mean = apply(mu, 2, mean)
mu_ci = apply(mu, 2, PI, prob = 0.97)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_ci, rugged_seq)
mtext("Non-African nations")


# Symmetry of interactions ----
rugged_seq = seq(from = -0.2, to = 1.2, length.out = 30)
muA = link(m8.3, data = data.frame(cid = 1, rugged_std = rugged_seq))
muN = link(m8.3, data = data.frame(cid = 2, rugged_std = rugged_seq))
delta = muA - muN

logGDP_diff = apply(delta, 2, mean)

log_GDP_ci = apply(delta, 2, PI)

data.frame(rugged_std = rugged_seq, logGDP_diff = logGDP_diff) %>% 
    ggplot(aes(x = rugged_std, y = logGDP_diff)) +
    geom_line()


plot(rugged_seq, logGDP_diff, type = "l", lwd = 2, col = rangi2,
     xlab = "ruggedness (standardised)", ylab = "expected difference log GDP",
     xlim = c(0, 1)
)

lines(rugged_seq, logGDP_diff, lwd = 2)
shade(log_GDP_ci, rugged_seq, col = col.alpha(rangi2, 0.3))
mtext("Difference between expected log GDP ")

