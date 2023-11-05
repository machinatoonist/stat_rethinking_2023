sample(c("W", "L"), size = 10, replace = TRUE, prob = 0.5)
?sample()

sim_game = function(p = 0.5, N = 10) {
    sample(c("W", "L"), size = N, replace = TRUE, prob = c(p, 1 - p))
}

wins = sim_game(p = 0.5, 10)

N = 100

(p_new = sum(wins == "W")/length(wins))


N_range = seq(N, 100, 1)

# Evaluate the function over a range of initial conditions
results <- sapply(beta_range, function(beta) {
    expected_value_of_pot(alpha_initial, beta_initial, N, pot)
})

df_plot <- data.frame(Beta = beta_range, ExpectedValue = results)

library(ggplot2)
library(dplyr)
library(plotly)

# Function to simulate a single game series
sim_game_series <- function(p, N) {
    wins = 0
    total_games = 0
    win_rate = numeric(N)
    alpha = 1
    beta = 1
    
    for (i in 1:N) {
        outcome = sample(c("W", "L"), size = 1, prob = c(p, 1 - p))
        if (outcome == "W") {
            wins = wins + 1
            alpha = alpha + 1
        } else {
            beta = beta + 1
        }
        total_games = total_games + 1
        p = alpha / (alpha + beta)  # Bayesian update of the probability
        win_rate[i] = wins / total_games
    }
    
    return(win_rate)
}

# Function to simulate multiple game series
sim_multiple_series <- function(p = 0.5, N = 100, s = 10) {
    df = data.frame()
    
    for (i in 1:s) {
        win_rate = sim_game_series(p, N)
        temp_df = data.frame(Game = 1:N, WinRate = win_rate, Series = rep(i, N))
        df = rbind(df, temp_df)
    }
    
    return(df)
}

# Simulate and plot
N = 1000  # Number of games in each series
p = 0.5  # Initial probability of winning
s = 10  # Number of series to simulate

df = sim_multiple_series(p, N, s)

gg = ggplot(df, aes(x = Game, y = WinRate, group = Series)) +
    geom_line(alpha = 0.5) +
    labs(
        title = "Trajectories of Total Wins to Total Games Played",
        x = "Game",
        y = "Win Rate"
    ) +
    theme_minimal()

ggplotly(gg)

# Now overlay this with a constant p of 0.5