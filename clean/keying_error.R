set.seed(123)  # For reproducibility

library(ggplot2)
library(brms)
library(dplyr)
library(tidyr)

# Sample size
n <- 1000

# Generate keying_error (binary)
keying_error <- rbinom(n, 1, 0.5)  # 50% chance of keying error

# Generate total_sales (independent of keying_error)
total_sales <- rnorm(n, mean = 100000, sd = 20000)  # Arbitrary mean and sd

# Generate prediction_error
# Positive relationship with keying_error and inverse relationship with total_sales
base_error <- rnorm(n, mean = 5, sd = 2)  # Base error
keying_error_effect <- keying_error * rnorm(n, mean = 2, sd = 1)  # Additional error due to keying error
sales_effect <- -0.002 * total_sales  # Inverse relationship with total_sales
prediction_error <- base_error + keying_error_effect + sales_effect + rnorm(n, mean = 0, sd = 1)  # Total prediction error

# Combine into a dataframe
data <- data.frame(keying_error, total_sales, prediction_error)
data$total_sales_scaled <- as.vector(scale(data$total_sales))
data %>% glimpse()

# Plot 1: Prediction Error vs Keying Error
ggplot(data, aes(x = as.factor(keying_error), y = prediction_error)) +
    geom_boxplot() +
    labs(x = "Keying Error", y = "Prediction Error", title = "Prediction Error by Keying Error")


# Plot 2: Prediction Error vs Total Sales
ggplot(data, aes(x = total_sales, y = prediction_error)) +
    geom_point(alpha = 0.5) +
    labs(x = "Total Sales", y = "Prediction Error", title = "Prediction Error vs Total Sales")


# Building the model with an interaction term
brm_model <- brm(
    prediction_error ~ keying_error + total_sales_scaled,  
    data = data,
    family = gaussian(),
    prior = c(
        set_prior("normal(0, 5)", class = "b"),
        set_prior("normal(0, 5)", class = "sigma", lb = 0)
    ),
    chains = 4,
    control = list(adapt_delta = 0.95, max_treedepth = 15),  
    iter = 4000
)

# Check the summary of the model
summary(brm_model)

plot(brm_model)

brms::launch_shinystan(brm_model)

# Creating a new data frame for predictions
new_data <- data.frame(
    keying_error = rep(0:1, each = 100),
    total_sales_scaled = rep(seq(from = min(data$total_sales_scaled), to = max(data$total_sales_scaled), length.out = 100), 2)
)

# Divide total_sales into quantiles
data$total_sales_quantile <- cut(data$total_sales_scaled, 
                                 breaks = quantile(data$total_sales_scaled, probs = 0:5 / 5), 
                                 include.lowest = TRUE, labels = FALSE)

# Adding predicted draws to your data
predicted_effects <- add_predicted_draws(brm_model, newdata = data) 

# Calculate contrasts at each quantile
contrast_effects <- predicted_effects %>% 
    group_by(total_sales_quantile) %>% 
    summarise(
        contrast = mean(.prediction[keying_error == 1]) - mean(.prediction[keying_error == 0]),
        mean_sales = mean(total_sales, na.rm = TRUE)
    )

# Plotting the contrast across total_sales quantiles
ggplot(contrast_effects, aes(x = mean_sales, y = contrast)) +
    geom_line() +
    geom_point() +
    labs(x = "Total Sales", y = "Contrast of Keying Error Effect",
         title = "Contrast of Keying Error Effects Across Total Sales Quantiles")
