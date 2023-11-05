
# we want to build is a model that calculates the proportion of prize money 
# that should be split between two players in a series of games such as 
# tennis where the latent chance of either player is not known and is based 
# on the relative superiority of their skills.  Suppose that the player who 
# wins the best out of 21 games (11 games in total) will win all of the prize 
# money P.  But the series of games is interrupted before a clear winner is 
# identified.  We want to split the prize money to reflect the likelihood of 
# that either player would have won competition.  The predicted probability 
# of each player winning the next game can be estimated based on the current 
# win loss ratio.  But with each successive game and potential outcome this 
# ratio and hence the win probability would change.  For example, if playing 
# in a best of 7 match where Albert had won 3 games and Bill had 1 game Albert 
# would have a 3/4 chance of winning the next game.  But if Bill does win the 
# next game Albert's chances of winning the second game will reduce to 3/5 and 
# so on until the critical number of games has been won.  
# Outline how this problem might be modeled using the probabilistic 
# programming package PyMC.  

# true params
skill_albert = 0.7
skill_bill = 1 - skill_albert

# Data from games so far in terms of wins for Albert
wins = 3
losses = 1
 
# The number of games to play was supposed to be 11
N = 11

# grid
p_grid = seq(from = 0, to = 1, length.out = 20)

# uniform prior
prior = rep(1, 20)

# likelihood 
likelihood = dbinom(wins, size = wins + losses, prob = p_grid)

# compute posterior 
unstd_posterior = likelihood * prior

std_posterior = unstd_posterior / sum(unstd_posterior)

samples = sample(p_grid, size = 1e4, replace = TRUE, prob = std_posterior)

PI(samples, prob = 0.5)

HPDI(samples, prob = 0.5)

# Maximum a posteriori (MAP)
p_grid[which.max(std_posterior)]

wins/(wins + losses)

mean(samples)

median(samples)

# Apply a loss function across all possible decisions
loss = sapply(p_grid, FUN = function(x) sum(std_posterior * abs(x - p_grid)))
loss

(p_win = p_grid[which.min(loss)]) # This is the same as the median of the samples

message(paste0("Albert has a ", round(p_win * 100, 2), "% chance of winning the next game"))

# If Albert wins the probability of winning the next game goes up

# If Albert loses the probability of winning the next game goes down

(outcome = rbinom(1, 1, p_win))
(wins = wins + outcome)
(losses = losses + as.numeric(outcome == 0))

if ((N - wins)/N > 0.5) {
    return("Albert wins")
} else if ((N - losses)/N > 0.5) {
    return("Bill wins")
} else next


