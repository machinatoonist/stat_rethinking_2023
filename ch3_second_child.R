library(rethinking)


data(homeworkch3)

sum(birth1)/length(birth1)

sum(birth2)/length(birth2)
length(birth1)
length(birth2)

(num_boys = sum(birth1) + sum(birth2))

# Use grid approximation compute the posterior distribution for the probability
# of being a boy

# grid
p_grid = seq(from = 0, to = 1, length.out = 20)

# uniform prior
prior = rep(1, 20)

# likelihood 
likelihood = dbinom(6, size = 9, prob = p_grid)

# compute posterior 
unstd_posterior = likelihood * prior

std_posterior = unstd_posterior / sum(unstd_posterior)

samples = sample(p_grid, size = 1e4, replace = TRUE, prob = std_posterior)

PI(samples, prob = 0.5)

HPDI(samples, prob = 0.5)

# Maximum a posteriori (MAP)
p_grid[which.max(std_posterior)]

chainmode(samples, adj = 0.1)

mean(samples)

median(samples)

# Estimating loss function
sum(std_posterior * abs(0.5 - p_grid)) # Expected loss if we decide on 0.5

# Apply this function across all possible decisions
loss = sapply(p_grid, FUN = function(x) sum(std_posterior * abs(x - p_grid)))
loss

p_grid[which.min(loss)]

# Probability of seeing W in N globe tosses given prob = 0.7
N = 2
dbinom(0:N, size = 2, prob = 0.7)

# Simulate 10 observations of N tosses
rbinom(n = 10, N, prob = 0.7)

dummy_w = rbinom(n = 1e5, size = N, prob = 0.7)
table(dummy_w)/1e5

# Simulate 9 tosses
dummy_w = rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

# But there is uncertainty in the parameter 0.7.  To propagate our uncertainty:
w = rbinom(1e5, size = 9, samples)

simplehist(w, xlab = "dummy water count with parameter uncertainty")

std_posterior

# 3H1 Posterior probability for a birth being a boy
boys = sum(birth1) + sum(birth2)
total = length(birth1) + length(birth2)

p_grid = seq(0, 1, length.out = 100)
prior = rep(100, 1)
likelihood = dbinom(boys, size = total, prob = p_grid)
posterior = prior * likelihood
posterior_std = posterior/sum(posterior)

library(ggplot2)
library(dplyr)

tibble(p_grid = p_grid, posterior = posterior_std) %>% 
    ggplot(aes(x = p_grid, y = posterior_std)) +
    geom_point() +
    geom_line() +
    theme_minimal()

p_grid[which.max(posterior_std)]

# 3H2 Draw 10,000 random parameter values from the posterior ----
prob_boy = sample(p_grid, size = 10000, replace = TRUE, posterior_std)
samples <- sample(p_grid, prob = posterior_std, size = 1e4, replace = TRUE)

length(prob_boy)
median(prob_boy)
mean(prob_boy)
max(prob_boy)
min(prob_boy)
plot(prob_boy)
plot.ecdf(prob_boy)
dens(prob_boy, adj = 2)

# Highest posterior density interval
rethinking::HPDI(prob_boy, prob = 0.5)
HPDI(prob_boy, prob = 0.89)
HPDI(prob_boy, prob = 0.97)

{
    samples <- sample(p_grid, prob = posterior_std, size = 1e4, replace = TRUE)
    HPDI(samples, prob = c(0.50, 0.89, 0.97))
}

# 3H3 Take 10000 replicates of 200 births and count the number of boys in each ----
births_200 = rbinom(1e4, size = 200, prob = prob_boy)
length(births_200)

mean(births_200)
median(births_200)
dens(births_200, adj = 1)

tibble(boys = births_200) %>% 
    ggplot(aes(x = boys)) +
    geom_density() +
    geom_vline(xintercept = num_boys, linetype = "dashed", color = "red") +
    theme_minimal()

{
    library(tidybayes)
    break_func <- function(x) {
        length(seq(min(x), max(x), by = 1)) + 1
    }
    
    set.seed(300)
    b <- rbinom(1e4, size = 200, prob = samples)
    
    ggplot() +
        stat_histinterval(aes(x = b), .width = c(0.66, 0.89), breaks = break_func) +
        geom_vline(aes(xintercept = boys), linetype = "dashed", color = "red") +
        labs(x = "Number of Boys", y = "Density")
    }

# 3H4 Take 10000 replicates of 100 births ----
births_100 = rbinom(1e4, size = 100, prob_boy)
num_first_boys = sum(birth1)


tibble(boys = births_100) %>% 
    ggplot(aes(x = boys)) +
    geom_density() +
    geom_vline(xintercept = num_first_boys, linetype = "dashed", color = "red") +
    theme_minimal()

ggplot() + 
    stat_histinterval(aes(x = births_100), .width = c(0.66, 0.89), breaks = break_func) +
    geom_vline(aes(xintercept = num_first_boys), linetype = "dashed", color = "red") +
    labs(x = "Number of Boys", y = "Density")

{
    set.seed(301)
    b <- rbinom(1e4, size = 100, prob = samples)
    
    
    ggplot() +
        stat_histinterval(aes(x = b), .width = c(0.66, 0.89), breaks = break_func) +
        geom_vline(aes(xintercept = sum(birth1)), linetype = "dashed",
                   color = "red") +
        labs(x = "Number of Boys", y = "Density")
    }

# 3H5 Count the number first borns who were girls ----
num_births = sum(birth1 == 0)

second_births = rbinom(1e4, num_births, prob_boy)

first_girl_second_boy = sum((birth1 == 0) * birth2)

tibble(boys = second_births) %>% 
    ggplot(aes(x = boys)) +
    geom_density() + 
    geom_vline(xintercept = first_girl_second_boy, linetype = "dashed", color = "red") +
    theme_minimal()

{
    fb_girls <- length(birth1) - sum(birth1)
    
    set.seed(302)
    b <- rbinom(1e4, size = fb_girls, prob = samples)
    
    obs_bfg <- sum(birth2[which(birth1 == 0)])
    
    ggplot() +
        stat_histinterval(aes(x = b), .width = c(0.66, 0.89), breaks = break_func) +
        geom_vline(aes(xintercept = obs_bfg), linetype = "dashed",
                   color = "red") +
        labs(x = "Number of Boys", y = "Density")
    }
