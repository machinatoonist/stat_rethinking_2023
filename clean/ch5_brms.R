library(rethinking)
library(brms)
library(dplyr)
library(ggplot2)
library(tidybayes)

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

# Categorical variables ----
data("Howell1")
d = Howell1
str(d)

# Convert 'sex' to a factor (1 for female, 2 for male)
d$sex <- factor(ifelse(d$male == 1, 2, 1), levels = c(1, 2), labels = c("Female", "Male"))

# Define and fit the model
model_fit_5_8brms <- brm(
    height ~ 0 + sex,  # Using sex as a predictor with no intercept; this gives individual intercepts for each sex
    data = d,
    family = gaussian(),
    prior = c(
        prior(normal(178, 20), class = "b"),  # Prior for the sex-specific intercepts
        prior(normal(0, 10), class = "sigma", lb = 0) # Using a half normal prior for sigma
    ),
    # control = list(adapt_delta = 0.95),  #increase the target acceptance rate if divergent transitions.
    chains = 4,
    iter = 2000
)

summary(model_fit_5_8brms)

stancode(model_fit_5_8brms)
sdata <- standata(model_fit_5_8brms)
names(sdata)

plot(model_fit_5_8brms)

### obtain model summaries and plots
summary(model_fit_5_8brms, waic = TRUE)
plot(model_fit_5_8brms, ask = FALSE)
plot(conditional_effects(model_fit_5_8brms), ask = FALSE)

brms::launch_shinystan(model_fit_5_8brms, rstudio = TRUE)
launch_shiny(model_fit_5_8brms, rstudio = TRUE)


library(tidybayes)
library(ggplot2)

# > Extract posterior samples ----
posterior_samples <- model_fit_5_8brms %>% 
    gather_draws()

plot(model_fit_5_8brms, variable = c("b_sex"))

# Extract posterior predictions
posterior_predictions <- model_fit_5_8brms %>%
    add_predicted_draws(newdata = d) 

# Trace plot for a specific parameter
ggplot(posterior_samples, aes(x = .iteration, y = .value, color = .chain)) +
    geom_line() +
    facet_wrap(~.variable, scales = "free") +
    theme_minimal() +
    labs(title = "Trace Plot", x = "Iteration", y = "Sampled Value")



model_fit_5_8 = quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a[sex],
        a[sex] ~ dnorm(178, 20),
        sigma ~ dunif(0, 50)
    ), data = d
)

precis(model_fit_5_8, depth = 2)


# > Expected difference in the height of females and males ----
# This is called a CONTRAST.
post = extract.samples(model_fit_5_8)
post$diff_fm = post$a[,1] - post$a[,2]
precis(post, depth = 2)


# Exercise 5M1 - Spurious correlation ----
library(here)

set.seed(2022)
n <- 100
temp <- rnorm(n)
shark <- rnorm(n, temp)
ice_cream <- rnorm(n, temp)

spur_exp <- tibble(ice_cream, temp, shark) %>%
    mutate(across(everything(), standardize))

ggplot(data = spur_exp,  aes(x = ice_cream, y = shark)) +
    geom_point()
pairs(spur_exp)

mod_t <- brm(ice_cream ~ 1 + temp, data = spur_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m1-t"))

summary(mod_t)

mod_s <- brm(ice_cream ~ 1 + shark, data = spur_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m1-s"))

summary(mod_s)

least_squares_model = lm(ice_cream ~ temp + shark, data = spur_exp)
summary(least_squares_model)

mod_all <- brm(ice_cream ~ 1 + temp + shark, data = spur_exp, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = here("fits", "chp5", "b5m1-all"))

summary(mod_all)

mod_all

get_variables(mod_all)

mod_all %>% 
    spread_draws(b_temp, b_shark) %>% 
    summarise(b_temp = mean(b_temp),
              b_shark = mean(b_shark))

bind_rows(
    spread_draws(mod_t, b_temp) %>%
        mutate(model = "mod_t"),
    spread_draws(mod_s, b_shark) %>%
        mutate(model = "mod_s"),
    spread_draws(mod_all, b_temp, b_shark) %>%
        mutate(model = "mod_all")
) %>%
    pivot_longer(cols = starts_with("b_"), names_to = "parameter",
                 values_to = "value") %>%
    drop_na(value) %>%
    mutate(model = factor(model, levels = c("mod_t", "mod_s", "mod_all")),
           parameter = factor(parameter, levels = c("b_temp", "b_shark"))) %>%
    ggplot(aes(x = value, y = fct_rev(model))) +
    facet_wrap(~parameter, nrow = 1) +
    stat_halfeye(.width = 0.89) +
    labs(x = "Parameter Value", y = "Model")

temp = seq(5, 30, length.out = 30)

# Exercise 5M2 - Masked relationship ----

set.seed(2023)
n <- 100
wealth <- rnorm(n)
age <- rnorm(n, wealth)
hazard <- rnorm(n, -wealth)

mask_exp <- tibble(wealth, age, hazard) %>%
    mutate(across(everything(), standardize))

ggplot(data = mask_exp,  aes(x = age, y = wealth)) +
    geom_point()
pairs(mask_exp)


mod_a <- brm(wealth ~ 1 + age, data = mask_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m12-a"))

mod_h <- brm(wealth ~ 1 + hazard, data = mask_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m12-h"))

mod_w <- brm(wealth ~ 1 + age + hazard, data = mask_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m12-w"))

summary(mod_w)

variables(mod_w)
variables(mod_a)
variables(mod_h)


bind_rows(
    spread_draws(mod_a, b_age) %>%
        mutate(model = "mod_a"),
    spread_draws(mod_h, b_hazard) %>%
        mutate(model = "mod_h"),
    spread_draws(mod_w, b_age, b_hazard) %>%
        mutate(model = "mod_w")
) %>%
    pivot_longer(cols = starts_with("b_"), names_to = "parameter",
                 values_to = "value") %>%
    drop_na(value) %>%
    mutate(model = factor(model, levels = c("mod_a", "mod_h", "mod_w")),
           parameter = factor(parameter, levels = c("b_age", "b_hazard"))) %>%
    ggplot(aes(x = value, y = fct_rev(model))) +
    facet_wrap(~parameter, nrow = 1) +
    stat_halfeye(.width = 0.89) +
    labs(x = "Parameter Value", y = "Model")

# From https://sr2-solutions.wjakethompson.com/causes-confounds-colliders
set.seed(2023)
n <- 100
u <- rnorm(n)
days_away <- rnorm(n, u)
instruction <- rnorm(n, u)
test_score <- rnorm(n, instruction - days_away)

mask_exp <- tibble(test_score, instruction, days_away) %>%
    mutate(across(everything(), standardize))

pairs(mask_exp)

mod_i <- brm(test_score ~ 1 + instruction, data = mask_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m2-i"))

mod_d <- brm(test_score ~ 1 + days_away, data = mask_exp, family = gaussian,
             prior = c(prior(normal(0, 0.2), class = Intercept),
                       prior(normal(0, 0.5), class = b),
                       prior(exponential(1), class = sigma)),
             iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
             file = here("fits", "chp5", "b5m2-d"))

mod_test <- brm(test_score ~ 1 + instruction + days_away, data = mask_exp,
                family = gaussian,
                prior = c(prior(normal(0, 0.2), class = Intercept),
                          prior(normal(0, 0.5), class = b),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                file = here("fits", "chp5", "b5m2-test"))

bind_rows(
    spread_draws(mod_i, b_instruction) %>%
        mutate(model = "mod_i"),
    spread_draws(mod_d, b_days_away) %>%
        mutate(model = "mod_d"),
    spread_draws(mod_test, b_instruction, b_days_away) %>%
        mutate(model = "mod_test")
) %>%
    pivot_longer(cols = starts_with("b_"), names_to = "parameter",
                 values_to = "value") %>%
    drop_na(value) %>%
    mutate(model = factor(model, levels = c("mod_i", "mod_d", "mod_test")),
           parameter = factor(parameter, levels = c("b_instruction",
                                                    "b_days_away"))) %>%
    ggplot(aes(x = value, y = fct_rev(model))) +
    labs(
        title = "The association between instruction and days test score is masked",
        subtitle = "Fitting test scores with both parameter reveals stronger effects"
    ) +
    facet_wrap(~parameter, nrow = 1) +
    stat_halfeye(.width = 0.89) +
    labs(x = "Parameter Value", y = "Model")

# Exercise 5M4: Predicting divorce rate with percent LDS population by state ----
lds_path = "https://raw.githubusercontent.com/wjakethompson/sr2-solutions/main/data/lds-data-2021.csv"
lds <- read_csv(lds_path)

write_csv(lds, file = here("data", "lds-data-2021.csv"))
lds <- read_csv(here("data", "lds-data-2021.csv"),
                col_types = cols(.default = col_integer(),
                                 state = col_character())) %>%
    mutate(lds_prop = members / population,
           lds_per_capita = lds_prop * 100000)

lds %>% glimpse()

data("WaffleDivorce")
lds_divorce <- WaffleDivorce %>%
    as_tibble() %>%
    select(Location, Divorce, Marriage, MedianAgeMarriage) %>%
    left_join(select(lds, state, lds_per_capita),
              by = c("Location" = "state")) %>%
    mutate(lds_per_capita = log(lds_per_capita)) %>%
    mutate(across(where(is.numeric), standardize)) %>% 
    filter(!is.na(lds_per_capita))

lds_divorce

lds_mod <- brm(Divorce ~ 1 + Marriage + MedianAgeMarriage + lds_per_capita,
               data = lds_divorce, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b, coef = Marriage),
                         prior(normal(0, 0.5), class = b, coef = MedianAgeMarriage),
                         prior(normal(0, 0.5), class = b, coef = lds_per_capita),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = here("fits", "chp5", "b5m4"))

spread_draws(lds_mod, `b_.*`, regex = TRUE) %>%
    pivot_longer(starts_with("b_"), names_to = "parameter",
                 values_to = "value") %>%
    ggplot(aes(x = value, y = parameter)) +
    labs(
        title = "A higher LDS percent population is associated with a lower divorce rate"
    ) +
    stat_halfeye(.width = c(0.67, 0.89, 0.97)) +
    labs(x = "Parameter Value", y = "Parameter")
