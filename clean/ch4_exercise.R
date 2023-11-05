# 4.1.1 Normalisation

pos = replicate(1000, sum(runif(16, -1, 1)))

plot(pos)

library(tidybayes)

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

# Random growth rate at each step between 1 and 1.1
prod(1 + runif(12, 0, 0.1))

growth = replicate(1e4, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

histo(growth, int = 0.01)
