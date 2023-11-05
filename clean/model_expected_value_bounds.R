library(ggplot2)
library(dplyr)
library(plotly)

# Function to compute expected value and confidence interval of the pot for player A
expected_value_and_confidence <- function(N_int, win_differential, pot) {
    if (win_differential > 0) {
        (alpha = win_differential)
        (beta = pmax(N_int - alpha, 0.1))
        
    } else if (win_differential == 0) {
            alpha = N_int / 2
            beta = alpha
        } else {
            (beta = -win_differential)
            (alpha = pmax(N_int - beta, 0.1))
            
            }
    
    message(paste0("alpha: ", alpha))
    message(paste0("beta: ", beta))
    
    # Calculate the probability of A winning the next round
    prob_A_next_round = alpha / (alpha + beta)
    
    # Confidence interval for prob_A_next_round
    ci_low = qbeta(0.025, alpha, beta)
    ci_high = qbeta(0.975, alpha, beta)
    
    # Calculate the expected value of the pot for player A
    expected_value_pot = prob_A_next_round * pot
    
    # Calculate the confidence interval for the expected value of the pot
    ci_low_pot = ci_low * pot
    ci_high_pot = ci_high * pot
    
    return(data.frame(ExpectedValue = expected_value_pot, CI_low = ci_low_pot, CI_high = ci_high_pot))
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
    print(win_differential)
    result <- expected_value_and_confidence(N_int, win_differential, pot)
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
