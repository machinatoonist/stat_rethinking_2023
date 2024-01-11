# Markov Chain Monte Carlo ----
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

dat <- list(
    D = standardize(d$Divorce),
    M = standardize(d$Marriage),
    A = standardize(d$MedianAgeMarriage)
)

f <- alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M + bA * A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
)

mq <- quap(f, data = dat)

precis(mq)

# Extract Stan code ----
library(cmdstanr)
mHMC <- ulam(f, data = dat, chains = 4, cores = 4)

precis(mHMC)

rethinking::stancode(mHMC)

# Save Stan code ----
stan_path = file.path(here(), "clean/stan_code")
write_stan_file(rethinking::stancode(mHMC), dir = stan_path, basename = "09_mHMC.stan")

mHMC_stan <- cstan(file = file.path(stan_path, "09_mHMC.stan"), data = dat)

post <- extract.samples(mHMC_stan)

precis(post) 

# Metropolis algorithm ----
num_weeks = 1e5
positions = rep(0, num_weeks)
current = 10
for (i in 1:num_weeks) {
    ## record current position
        positions[i] = current
    ## flip coin to generate proposal
        proposal = current + sample(c(-1, 1), size = 1)
    ## now make sure he loops around the archipelago
        if (proposal < 1) proposal = 10
        if (proposal > 10) proposal = 1
    # move?
        prob_move = proposal/current
        current = ifelse( runif(1) < prob_move, proposal, current)
}

plot(1:100, positions[1:100])

plot(table(positions))

# High dimensional problems with sampling ----
D = 1000
T = 1e3
Y = rmvnorm(T, rep(0, D), diag(D))
rad_dist = function(Y) sqrt(sum(Y^2))
Rd = sapply(1:T, function(i) rad_dist(Y[i,]))
dens(Rd)
?rmvnorm()
