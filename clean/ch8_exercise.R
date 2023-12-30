library(rethinking)

data("rugged")
d <- rugged

# Make log version of outcome
d$log_gdp = log(d$rgdppc_2000)
dim(d)

# extract countries with GDP data
dd = d[complete.cases(d$rgdppc_2000), ]
??complete.case()
dim(dd)

# rescale variables
# log GDP is divided by the average value so it is a proportion of the 
# international average.  1 means average, 0.8 means 80% of the average and
# 1.1 means 10% more than average
dd$log_gdp_std = dd$log_gdp / mean(dd$log_gdp)

# Scaling so 0 represents zero ruggedness and 1 represents the most rugged
dd$rugged_std = dd$rugged / max(dd$rugged)

m8.1 = quap(
    alist(
        log_gdp_std ~ dnorm( mu, sigma),
        mu <- a + b * (rugged_std - 0.215),
        a ~ dnorm(1, 1),
        b ~ dnorm(0, 1),
        sigma ~ dexp(1)
    ), data = dd
)

# Look at prior predictions
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
