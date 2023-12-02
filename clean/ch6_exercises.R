library(rethinking)

# Simulate Berkson's Paradox ----
set.seed(1914)
N = 200
p = 0.1
# uncorrelated newsworthiness and trustworthiness 
nw = rnorm(N)
tw = rnorm(N)
# select top 10% of combined scores
s = nw + tw # total score
q = quantile(s, probs = 1 - p)
q
selected = ifelse(s >= q, TRUE, FALSE)
plot(nw[selected], tw[selected])
cor(nw[selected], tw[selected])

# Multicollinearity ----
N = 100
set.seed(909)
height = rnorm(N, 10, 2)
leg_prop = runif(N, 0.4, 0.5)
leg_left = leg_prop * height + rnorm(N, 0, 0.02)
leg_right = leg_prop * height + rnorm(N, 0, 0.02)

d = data.frame(height, leg_left, leg_right)

m6.1 = quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + bl * leg_left + br * leg_right,
        a ~ dnorm(10, 100),
        bl ~ dnorm(2, 10),
        br ~ dnorm(2, 10),
        sigma ~ dexp(1)
        
    ), data = d,
    verbose = TRUE
)

precis(m6.1)
plot(precis(m6.1))


# The multiple linear regression answers the question: What is the the value
# of knowing each predictor, after already knowing all of the other predictors?

# i.e. including both leg lengths is redundant,  there is little value in
# knowing each leg's length after already knowing the other leg's length

post = extract.samples(m6.1)

plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)

# The model has produced a good estimate of the sum of bl and br.

# Posterior distribution of the sum:
sum_blbr = with(post, expr = bl + br)
dens(sum_blbr, col = rangi2, lwd = 2, xlab = "Sum of bl and br")

m6.2 = quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + bl * leg_left,
        a ~ dnorm(10, 100),
        bl ~ dnorm(2, 10),
        sigma ~ dexp(1)
    ), data = d
)

precis(m6.2)

# Conclusion: When two predictor variables are very strongly correlated
# with each other, including both in a model may lead to confusion.

# Multi-collinear milk ----
# What to do when the collinearity exists in real data that is not anticipated?
library(rethinking)

data("milk")
d = milk
d$K = standardize(d$kcal.per.g)
d$F = standardize(d$perc.fat)
d$L = standardize(d$perc.lactose)

pairs(d)

# kcal.per.g regressed on perc.fat
m6.3 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bF * F,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

# kcal.per.g regessed on perc.lactose
m6.4 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bL * L,
        a ~ dnorm(0, 0.2),
        bL ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

precis(m6.3)
precis(m6.4)

# Both models have narrow posteriors on one size or other of zero
# Fitting with both the posterior means are closer to zero and the
# sd of both are twice as large
m6.4 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bF * F + bL * L,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        bL ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

pairs(~kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)

precis(m6.4)

post = extract.samples(m6.4)
post

dens(post$bF, dnorm(0, .5))
dens(post)
post$Fp = rnorm(n = length(post$bF), mean = 0, sd = .5)
dens(post)

# Simulating collinearity ----
sim.coll = function(r = 0.9) {
    d$x <- rnorm(nrow(d), mean = r * d$perc.fat,
                 sd = sqrt((1 - r^2) * var(d$perc.fat)))
    m <- lm(kcal.per.g ~ perc.fat + x, data = d)
    sqrt(diag(vcov(m)))[2] # stddev of parameter
}
rep.sim.coll <- function(r = 0.9, n = 100) {
    stddev <- replicate(n, sim.coll(r))
    mean(stddev)
}
r.seq <- seq(from = 0, to = 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r = z, n = 100))
plot(stddev ~ r.seq, type = "l", col = rangi2, lwd = 2, xlab = "correlation")
