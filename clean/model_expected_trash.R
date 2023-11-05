library(ggplot2)
library(dplyr)
library(plotly)

# Function to compute expected value and confidence interval of the pot for player A
expected_value_and_confidence <- function(N, N_int, win_differential, pot) {
    alpha = N_int + win_differential
    beta = N_int - win_differential
    
    # Calculate the probability of A winning the next round
    prob_A_next_round = alpha / (alpha + beta)
    
    # Calculate the probability of A winning if the game goes to the next round
    prob_A_second_round = alpha / (alpha + beta + 1)
    
    # Calculate the probability of B winning if the game goes to the next round
    prob_B_second_round = (beta + 1) / (alpha + beta + 1)
    
    # Calculate the probability of A winning if the game goes to a tie-breaker
    prob_A_tie_breaker = 1 / 2
    
    # Calculate the overall probability of A winning the pot
    prob_A_wins_pot = prob_A_next_round + 
        (1 - prob_A_next_round) * prob_A_second_round + 
        (1 - prob_A_next_round) * prob_B_second_round * prob_A_tie_breaker
    
    # Calculate the expected value of the pot for player A
    expected_value_pot = prob_A_wins_pot * pot
    
    # Confidence interval for prob_A_wins_pot
    ci_low = qbeta(0.025, alpha, beta) * pot
    ci_high = qbeta(0.975, alpha, beta) * pot
    
    return(data.frame(ExpectedValue = expected_value_pot, CI_low = ci_low, CI_high = ci_high))
}

# Initialize variables
N = 7  # Total intended rounds
pot = 200  # Total value of the pot
N_int = 4  # Number of rounds played when the game was interrupted
win_differential_range = seq(-N_int, N_int, 1)

# Create an empty data frame to store results
results_df <- data.frame()

# Loop through each win_differential to populate the results_df
for (win_differential in win_differential_range) {
    result <- expected_value_and_confidence(N, N_int, win_differential, pot)
    result$WinDifferential = win_differential
    results_df <- rbind(results_df, result)
}

# Plot the graph
gg = ggplot(results_df, aes(x = WinDifferential, y = ExpectedValue)) +
    geom_line() +
    geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
    labs(
        title = "Expected Value of Pot for Player A with Confidence Interval",
        subtitle = paste0("$", pot, " pot size when game is interrupted after ", N_int, " rounds"),
        x = "Win Differential (A - B)",
        y = "Expected value of the pot for player A"
    ) +
    theme_minimal()

ggplotly(gg)
