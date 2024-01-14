library(rstanarm)
library(dplyr)
library(purrr)

options(mc.cores = parallel::detectCores())

set.seed(44)  # For reproducibility

# Generate unique company names
company_names <- paste0("Company_", 1:10)

# Function to generate a number of employees from a bimodal distribution
sample_bimodal <- function(n, mean1, sd1, mean2, sd2) {
    # Half of the time sample from the first distribution, otherwise from the second
    ifelse(runif(n) < 0.5,
           rnorm(n, mean = mean1, sd = sd1),
           rnorm(n, mean = mean2, sd = sd2))
}

# Function to generate log-normal scores with a specific mean and standard deviation
generate_log_normal_scores <- function(n, mean, sd) {
    mu <- log(mean^2 / sqrt(sd^2 + mean^2))
    sigma <- sqrt(log(1 + (sd^2 / mean^2)))
    rlnorm(n, meanlog = mu, sdlog = sigma)
}

# Function to generate data for single company
generate_company_data <- function(company_name) {
    num_employees <- round(sample_bimodal(1, mean1 = 50, sd1 = 10, mean2 = 2000, sd2 = 200))
    # num_employees <- sample(30:2000, 1)  # Random number of employees between 5 and 2000
    employee_ids <- paste0(company_name, "_", 1:num_employees)
    scores <- generate_log_normal_scores(num_employees, mean = 50, sd = 20)
    
    data.frame(company_name = company_name, employee_id = employee_ids, score = scores)
}

# Generate data for all companies
company_df <- map_df(company_names, generate_company_data)

# Check the global mean and standard deviation
print(paste("Global Mean:", mean(company_df$score)))
print(paste("Global Standard Deviation:", sd(company_df$score)))

# Order companies by mean scores
company_df = company_df %>% 
    mutate(company_name = fct_reorder(company_name, score, .fun = 'mean'))

# Display the first few rows of the DataFrame
head(company_df)

company_df %>% 
    count(company_name)

company_df %>% 
    ggplot(aes(x = score)) +
    geom_density() +
    theme_minimal()

mean_company_scores = 
    company_df %>% 
    group_by(company_name) %>% 
    summarise(score = round(mean(score)),
              num_employees = n())

mean_company_scores

(avg_score_across_all_employees = mean(company_df$score))

company_df %>% glimpse()

# Posterior simulation
hierarchical_model = stan_glmer(
    score ~ (1 | company_name),
    data = company_df, family = gaussian,
    prior_intercept = normal(50, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

pp_check(hierarchical_model)

# Directly simulate posterior models for the artist-specific means ----
# Get MCMC chains for each mu_j
company_chains = hierarchical_model %>% 
    spread_draws(`(Intercept)`, b[,company_name]) %>% 
    mutate(mu_j = `(Intercept)` + b)

artist_chains %>% 
    select(artist, `(Intercept)`, b, mu_j) %>% 
    head(4)

# Get posterior summaries for each artist's mean popularity mu_j
artist_summary_scaled = artist_chains %>% 
    select(-`(Intercept)`, -b) %>% 
    mean_qi(.width = 0.8) %>% 
    mutate(artist = fct_reorder(artist, mu_j))

# Check out the results
artist_summary_scaled %>% 
    select(artist, mu_j, .lower, .upper) %>% 
    head(4)

# 80% credible intervals for each artist's mean song popularity
ggplot(artist_summary_scaled,
       aes(x = artist, y = mu_j, ymin = .lower, ymax = .upper)) +
    geom_pointrange() +
    xaxis_text(angle = 90, hjust = 1)

# Shrinkage and the bias-variance trade-off ----
set.seed(43)
predictions_hierarchical = posterior_predict(hierarchical_model,
                                             newdata = mean_company_scores)

# Posterior predictive plots
# Contrast the hierarchical model posterior mean predictions with the complete
# pooled model predictions (dashed horizontal line) and no pooled model predictions
# (dark blue dots).  The hierarchical predictions are pulled or 'shrunk' toward the
# global trends of the complete pooled model and away from the local trends of the 
# no pooled model.  
# 1) Shrinkage increases as the number of observations on group j decrease.
# 2) Shrinkage increase when the variability within groups, sigma_y, is large
# in comparison to the variability between groups sigma_mu.
ppc_intervals(mean_company_scores$score, yrep = predictions_hierarchical,
              prob_outer = 0.80) +
    ggplot2::scale_x_continuous(labels = mean_company_scores$company_name,
                                breaks = 1:nrow(mean_company_scores)) +
    # Average score across all employees
    geom_hline(yintercept = avg_score_across_all_employees, 
               linetype = "dashed") +
    labs(title = "Companies with smaller numbers of employees vary more from the global mean",
         subtitle = "A hierarchical model corrects for this effect",
         x = "",
         y = "Mean Company Score") +
    theme_minimal() +
    xaxis_text(angle = 90, hjust = 1)
