# Function to compute expected value of the pot for player A
expected_value_of_pot <- function(alpha, beta, N, pot) {
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
    
    return(expected_value_pot)
}

# Initialize variables
N = 7  # Total intended rounds
pot = 200  # Total value of the pot
alpha_initial = 30  # Fixed prior wins for A
beta_initial = 10
beta_range = seq(1, 10, 1)  # Varying prior wins for B
N_range = seq(N, 100, 1)

# Evaluate the function over a range of initial conditions
results <- sapply(beta_range, function(beta) {
    expected_value_of_pot(alpha_initial, beta_initial, N, pot)
})

df_plot <- data.frame(Beta = beta_range, ExpectedValue = results)

# Plot the graph
ggplot(df_plot, aes(x = Beta, y = ExpectedValue)) +
    geom_line() +
    geom_point() +
    ggtitle("Expected Value of Pot for Player A") +
    xlab("Prior Wins for Player B") +
    ylab("Expected Value of Pot for Player A") +
    theme_minimal()

# Prepare data for plotting
df_plot <- data.frame(Beta = beta_range, ExpectedValue = results)

# Plot the graph
g1 = ggplot(df_plot, aes(x = Beta, y = ExpectedValue)) +
    geom_line() +
    geom_point() +
    ggtitle("Expected Value of Pot for Player A") +
    xlab("Prior Wins for Player B") +
    ylab("Expected Value of Pot for Player A") +
    theme_minimal()

ggplotly(g1)

N_range = seq(N, 100, 1)

# Evaluate the function over a range of initial conditions
results_N <- sapply(N_range, function(N) {
    expected_value_of_pot(alpha_initial, beta_initial, N, pot)
})

# Prepare data for plotting
df_N_plot <- data.frame(N = N_range, ExpectedValue = results_N)

# Plot the graph
ggplot(df_N_plot, aes(x = N, y = ExpectedValue)) +
    geom_line() +
    geom_point() +
    labs(
        title = paste0("Expected Value of Pot for Player A given ", 
                       alpha_initial, " wins and ", beta_initial, " losses"),
        subtitle = paste0("$",pot, " pot size when game is interrupted"),
        x = "Total number of rounds to play (N)",
        y = "Expected value of the pot for player A"
    ) +
    theme_minimal()
