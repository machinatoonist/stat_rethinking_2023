# 4.1.1 Normalisation

pos = replicate(1000, sum(runif(16, -1, 1)))

sum(pos)

plot(pos)

library(tidybayes)
library(ggplot2)
library(dplyr)

break_func = function(x) {
    length(seq(min(x), max(x), by = 0.2)) + 1
}

ggplot() +
    stat_histinterval(aes(x = pos), .width = c(0.66, 0.89), breaks = break_func) +
    labs(x = "Position", y = "Density")

histo = function(p, int = 1) {
    
    break_func = function(x) {
        length(seq(min(x), max(x), by = int)) + 1
    }
    
    ggplot() +
        stat_histinterval(aes(x = p), .width = c(0.66, 0.89), breaks = break_func) +
        labs(x = "Position", y = "Density")
}

library(rethinking)

dens(pos)
hist(pos)
plot(density(pos))
histo(pos, int = 0.3)

# Random growth rate at each step between 1 and 1.1
prod(1 + runif(12, 0, 0.1))

growth = replicate(1e4, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

histo(growth, int = 0.01)

big = replicate(1e4, prod(1 + runif(12, 0, 0.5)))
small = replicate(1e4, prod(1 + runif(12, 0, 0.1)))

histo(big, int = 0.2)
histo(small, int = 0.01)

big_log = log(replicate(1e4, prod(1 + runif(12, 0, 0.5))))
histo(big_log, int = 0.02)

(euler = 2 + 1/factorial(2) + 1/factorial(3) + 1/factorial(4))

(euler = 1 + sum(sapply(seq(1, 20, 1), function(x) 1/(factorial(x)))))

factorial(0)

# Simple exponential probability distribution
simple_dist = function(x) {
    exp(-x^2)/sqrt(pi)
}

normal_dist = function(x, sd) {
    exp(-x^2)/(2 * sd^2)/sqrt(2 * sd^2 * pi)
}

df = tibble(
    x = seq(-10, 10, 0.1),
    y = simple_dist(x),
    z = normal_dist(x, sd = 1)) 

p = ggplot() 

# p = p + stat_histinterval(aes(x = pos), .width = c(0.66, 0.89), breaks = break_func)

p = p + geom_density(aes(x = pos))

p = p  + geom_line(data = df, aes(x = x, y = y)) 

p = p + geom_line(data = df, aes(x = x, y = z), color = "red")

p
    

# Sample data
df1 <- data.frame(x = rnorm(100), y = rnorm(100))
df2 <- data.frame(x = seq(-3, 3, length.out=100), y = seq(-3, 3, length.out=100))

# Create the base plot
p <- ggplot()

# Add a scatter plot layer using df1
p <- p + geom_point(data = df1, aes(x = x, y = y), color = 'blue')

# Add a line plot layer using df2
p <- p + geom_line(data = df2, aes(x = x, y = y), color = 'red')

# Display the plot
print(p)

# Grid approximation for the globe tossing experiment ----
# prior = rep(1, 20)
# grid = seq(from = 0, to = 1, length.out = 20)
# likelihood = dbinom(6, size = 9, prob = grid)
# posterior_unstd = prior * likelihood
# posterior = posterior_unstd / sum(posterior_unstd)

w = 6; n = 9;
p_grid = seq(from = 0, to = 1, length.out = 100)
posterior = dbinom(x = w, size = n, prob = p_grid) * dunif(p_grid, min = 0, max = 1)
posterior_std = posterior / sum(posterior)

# Gaussian model of height ----
data("Howell1")
d = Howell1

precis(d)
summary(d)
glimpse(d)
str(d)

d2 = d[d$age >= 18, ]

d2 %>% glimpse()

dens(d2$height, norm.comp = TRUE)

# Prior predictive simulation ----
# Average height mu
curve(dnorm(x, 178, 20), from = 100, to = 250)

# standard deviation sigma
curve(dunif(x, 0, 50), from = -10, to = 60)
curve(dexp(x, rate = .3), from = -10, 60)

# prior predictive simulation
sample_mu = rnorm(1e4, mean = 178, sd = 20)
# sample_sigma = runif(1e4, min = 0, max = 50)
sample_sigma = rexp(1e4, rate = 0.3)

prior_h = rnorm(1e4, mean = sample_mu, sd = sample_sigma)
dens(prior_h)
histo(prior_h)

# It's important that your prior not be based on the values in your data, but
# only on what you know about the data before you see it


# Grid approximation of the posterior distribution ----
mu_list = seq(from = 150, to = 160, length.out = 100)
sigma_list = seq(from = 7, to = 9, length.out = 100)
post = expand.grid(mu = mu_list, sigma = sigma_list)
post$LL = sapply(1:nrow(post), function(i) sum(
    dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)))
post$prod = post$LL + dnorm(post$mu, 178, 20, log = TRUE) +
    dunif(post$sigma, 0, 50, TRUE)
post$prob = exp(post$prod - max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)

image_xyz(post$mu, post$sigma, post$prob)

# Sampling from the posterior ----
post %>% str()
sample_rows = sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample_mu = post$mu[sample_rows]
sample_sigma = post$sigma[sample_rows]

plot(sample_mu, sample_sigma, cex = 0.6, pch = 16, col = col.alpha(rangi2, 0.1))

# Summarise the samples
dens(sample_mu, adj = 1)
histo(sample_mu, int = .1)

dens(sample_sigma)
histo(sample_sigma, int = 0.08)

PI(sample_sigma)
PI(sample_mu)

HPDI(sample_sigma)
HPDI(sample_mu)

d3 = sample(d2$height, size = 20)
# Grid approximation of the posterior distribution ----
mu_list = seq(from = 150, to = 170, length.out = 200)
sigma_list = seq(from = 4, to = 20, length.out = 100)
post2 = expand.grid(mu = mu_list, sigma = sigma_list)
post2$LL = sapply(1:nrow(post2), function(i) sum(
    dnorm(d2$height, post2$mu[i], post2$sigma[i], log = TRUE)))
post2$prod = post2$LL + dnorm(post2$mu, 178, 20, log = TRUE) +
    dunif(post2$sigma, 0, 50, TRUE)
post2$prob = exp(post2$prod - max(post2$prod))

sample2_rows = sample(1:nrow(post2), size = 1e4, replace = TRUE, prob = post2$prob)
sample2_mu = post2$mu[sample2_rows]
sample2_sigma = post2$sigma[sample2_rows]

plot(sample2_mu, sample2_sigma, cex = 0.7, col = col.alpha(rangi2, 0.1), 
     xlab = "mu", ylab = "sigma", pch = 16)

dens(sample2_sigma, adj = 2, norm.comp = TRUE)
histo(sample2_sigma, int = 0.05)
