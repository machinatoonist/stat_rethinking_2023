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
# Lecture 2 60min