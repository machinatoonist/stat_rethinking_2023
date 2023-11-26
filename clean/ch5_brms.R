library(rethinking)
library(brms)
library(dplyr)
library(ggplot2)

data("WaffleDivorce")
d <- WaffleDivorce
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

class(d)

# Model Divorce rate with Median Age when Married ----
# Define the model using brms formula syntax
model_formula <- bf(D ~ 1 + A)  # '1' for the intercept

# 5.3 Fit the model with priors
brm_model_fit_5_1 <- brm(
    formula = model_formula,
    data = d,
    family = gaussian(),  # assuming D is a continuous variable
    prior = c(
        prior(normal(0, 0.2), class = "Intercept"),  # Prior for intercept (a)
        prior(normal(0, 0.5), class = "b", coef = "A"),  # Prior for slope (bA)
        prior(exponential(1), class = "sigma")  # Prior for sigma
    ),
    chains = 4,  
    iter = 2000  
)

# Check the model summary
summary(brm_model_fit_5_1)  # Similar but more verbose than precis()


# > Fig 5.3 Prior predictive checks ----
set.seed(10)

# Number of simulations
n_sims <- 50

# Simulate priors
a_sims <- rnorm(n_sims, 0, 0.2)
bA_sims <- rnorm(n_sims, 0, 0.5)

# Create a sequence of values for A
A_values <- seq(-2, 2, length.out = 100)

# Initialize an empty data frame for predictions
prior_predictions <- data.frame(A = rep(A_values, n_sims), 
                                sim = rep(1:n_sims, each = length(A_values)))

# Generate predictions
prior_predictions$D_pred <- with(prior_predictions, a_sims[sim] + bA_sims[sim] * A)

prior_predictions %>% glimpse()

# Plot
ggplot(prior_predictions, aes(x = A, y = D_pred, group = sim)) +
    geom_line(alpha = 0.4, color = "black") +
    labs(x = "Age (Standardized)", y = "Predicted Divorce Rate", 
         title = "Prior Predictive Plot") +
    theme_minimal()

# > Posterior predictions ----

# Extract predicted values
d$predicted <- fitted(brm_model_fit_5_1)[, "Estimate"]

# Create the plot
ggplot(d, aes(x = A, y = D)) +
    geom_point() +  # Plot the raw data points
    geom_line(aes(y = predicted), color = "blue") +  # Add the fitted model line
    labs(x = "Age", y = "Divorce Rate", title = "Fitted Model vs. Raw Data") +
    theme_minimal()


# Sequence of predictor values
A_seq <- seq(from = -3, to = 3.2, length.out = 30)

# Generate posterior predictions
post_preds <- posterior_predict(brm_model_fit_5_1, newdata = data.frame(A = A_seq))

# Calculate mean and percentile interval
mu_mean <- apply(post_preds, 2, mean)
mu_PI <- apply(post_preds, 2, function(x) quantile(x, probs = c(0.055, 0.945)))

# Create a data frame for ggplot
plot_data <- data.frame(A = A_seq, mu_mean = mu_mean, lwr = mu_PI[1,], upr = mu_PI[2,])

# Plot
ggplot() +
    geom_point(data = d, aes(x = A, y = D), color = rangi2) +  # Raw data points
    geom_line(data = plot_data, aes(x = A, y = mu_mean), color = "blue", size = 1) +  # Fitted line
    geom_ribbon(data = plot_data, aes(x = A, ymin = lwr, ymax = upr), alpha = 0.2) +  # Percentile interval
    labs(x = "Age (Standardized)", y = "Divorce Rate", title = "Posterior Predictions with 89% Interval") +
    theme_minimal()


#TODO check why the interval is wider in this plot than in Richard's version.
#TODO a less verbose summary than summary()

# Model Divorce Rate with Marriage Rate ----

# Define the model using brms formula syntax
model_formula_2 <- bf(D ~ 1 + M)  # '1' for the intercept
d %>% glimpse()

# 5.3 Fit the model with priors
brm_model_fit_5_2 <- brm(
    formula = model_formula_2,
    data = d,
    family = gaussian(),  # assuming D is a continuous variable
    prior = c(
        prior(normal(0, 0.2), class = "Intercept"),  # Prior for intercept (a)
        prior(normal(0, 0.5), class = "b", coef = "M"),  # Prior for slope (bM)
        prior(exponential(1), class = "sigma")  # Prior for sigma
    ),
    chains = 4,  
    iter = 2000  
)

# Check the model summary
summary(brm_model_fit_5_2)  

# Extract predicted values
d$predicted_2 <- fitted(brm_model_fit_5_2)[, "Estimate"]

# Create the plot
ggplot(d, aes(x = M, y = D)) +
    geom_point() +  # Plot the raw data points
    geom_line(aes(y = predicted_2), color = "blue") +  # Add the fitted model line
    labs(x = "Age", y = "Divorce Rate", title = "Fitted Model vs. Raw Data") +
    theme_minimal()


# Sequence of predictor values
M_seq <- seq(from = -2, to = 3.2, length.out = 30)

# > Generate posterior predictions ----
post_preds_2 <- posterior_predict(brm_model_fit_5_2, 
                                  newdata = data.frame(M = M_seq))

# Calculate mean and percentile interval
mu_mean_2 <- apply(post_preds_2, 2, mean)
mu_PI_2 <- apply(post_preds_2, 2, function(x) quantile(x, probs = c(0.055, 0.945)))

# Create a data frame for ggplot
plot_data_2 <- data.frame(M = M_seq, 
                        mu_mean = mu_mean_2, 
                        lwr = mu_PI_2[1,], 
                        upr = mu_PI_2[2,])

# > Plot predictions and percentile interval ----
ggplot() +
    geom_point(data = d, aes(x = M, y = D), color = rangi2) +  # Raw data points
    geom_line(data = plot_data_2, aes(x = M, y = mu_mean_2), color = "blue", size = 1) +  # Fitted line
    geom_ribbon(data = plot_data_2, aes(x = M, ymin = lwr, ymax = upr), alpha = 0.2) +  # Percentile interval
    labs(x = "Marriage rate", y = "Divorce Rate", title = "Posterior Predictions with 89% Interval") +
    theme_minimal()

# Multiple regression model for divorce ----
model_formula_3 <- bf(D ~ 1 + M + A)  # '1' for the intercept

brm_model_fit_5_3 <- brm(
    formula = model_formula_3,
    data = d,
    family = gaussian(),  # assuming D is a continuous variable
    prior = c(
        prior(normal(0, 0.2), class = "Intercept"),  # Prior for intercept (a)
        prior(normal(0, 0.5), class = "b", coef = "M"),  # Prior for slope (bM)
        prior(normal(0, 0.5), class = "b", coef = "A"),  # Prior for slope (bA)
        prior(exponential(1), class = "sigma")  # Prior for sigma
    ),
    chains = 4,  
    iter = 2000  
)

summary(brm_model_fit_5_3)
