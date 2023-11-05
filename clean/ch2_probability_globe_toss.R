# Simulate globe tossing experiment ----

sample_g <- c("W", "L", "W", "W", "W", "L", "W", "L", "W")
W <- sum(sample_g == "W")
L <- sum(sample_g == "L")
p <- c(0, 0.25, 0.5, 0.75, 1) # Possible proportions of water
dimensions <- 4

find_ways <- function(p, dim) {
    ways <- sapply(p, function(x) (x*dim)^W * ((1 - x)*dim)^L)
    prob <- ways/sum(ways)
    cbind(p, ways, prob)
    
}

(num_ways = find_ways(p, dimensions))

(num_ways = find_ways(p, dim = 200))
plot(num_ways[,1], num_ways[,3], "b")

# 1: Generative simulation ----
sim_globe = function(p = 0.7, N = 10) {
    sample(c("W", "L"), size = N, replace = TRUE, prob = c(p, 1 - p))
}

sim_globe(p = 0.5, N = 9)
replicate(sim_globe(p = 0.5, N = 10), n = 10)

# 2: The estimator ----
compute_posterior = function(the_sample, p = c(0, 0.25, 0.5, 0.75, 1), dim = 4) {
    W <- sum(the_sample == "W")
    L <- sum(the_sample == "L")
    ways <- sapply(p, function(x) (x*dim)^W * ((1 - x)*dim)^L)
    post <- round(ways/sum(ways), 3)   
    data.frame(p, ways, post)
}

compute_posterior(the_sample = sim_globe())

compute_posterior(the_sample = sim_globe(p = 0.5, N = 50))

# Beta distribution ----
curve(sin, -2*pi, xname = "t")
curve(exp, -2*pi, xname = "t")

# Used as a likelihood function (distribution function assigned to an observed variable)
# Compute the likelihood of 6 successes in 9 trials if the probability of success is 0.5
# Compute the likelihood of 6 W's from 9 tosses with an underlying proportion of 0.5
dbinom(x = 6, size = 9, prob = 0.5)

p = 0.5
print(paste0("The relative number of ways to get 6 W's : ", 
             dbinom(x = 6, size = 9, prob = p), " with p = ", p))

print(paste0("The relative number of ways to get 5 W's : ", 
             dbinom(x = 5, size = 9, prob = p), " with p = ", p))

print(paste0("The relative number of ways to get 4 W's : ", 
             dbinom(x = 4, size = 9, prob = p), " with p = ", p))


# Grid approximation ----
# Define grid 
grid = seq(from = 0, to = 1, length.out = 20)
grid

# Define prior
# Estimate of the true proportion
prior = rep(1, 20)
prior = ifelse(test = grid < 0.5, 0, 1)
prior = exp(-2*abs(grid - 0.5))

# Compute likelihood of each value in a grid
# Number of ways to get 6 positives from 9 chances
likelihood = dbinom(6, size = 9, prob = grid)

# Compute product of likelihood and prior
posterior_unstd = prior * likelihood

# Standardise the posterior so it sums to 1
posterior = posterior_unstd / sum(posterior_unstd)

plot(x = grid, y = posterior, type = "b",  
     xlab = "probability of water", 
     ylab = "posterior probability",
     main = "Posterior")
mtext("20 points")

plot(x = grid, y = likelihood, type = "b",
     xlab = "probability",
     ylab = "likelihood", 
     main = "Likelihood")

plot(x = grid, y = prior, type = "b",
     xlab = "probability",
     ylab = "density")

# Quadratic approximation ----
library(rethinking)

globe_qa = quap(
    alist(
        W ~ dbinom(W + L, p), # Binomial likelihood
        p ~ dunif(0, 1)       # Uniform prior
    ),
    data = list(W = 6, L = 3)
)

# display summary of quadratic approximation
precis(globe_qa)

# analytical calculation
W = 6
L = 3
curve(dbeta(x, W + 1, L + 1), from = 0, to = 1)

# quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, from = 0, to = 1, add = TRUE)

# Markov Chain Monte Carlo ----
# Metropolis (Rosenbluth) algorithm
n_samples = 10000
p = rep(NA, n_samples)
p[1] = 0.5
W = 6
L = 3
for (i in 2:n_samples) {
    p_new = rnorm(n = 1, mean = p[i - 1], sd = 0.1)
    if (p_new < 0) p_new = abs(p_new)
    if (p_new > 1) p_new = 2 - p_new
    q0 = dbinom(W, W + L, p[i - 1])
    q1 = dbinom(W, W + L, p_new)
    p[i] = ifelse(runif(1) < q1 / q0, p_new, p[i - 1])
}

dens(p, xlim = c(0, 1))
curve(dbeta(x, 6, 3), lty = 2, add = TRUE)
