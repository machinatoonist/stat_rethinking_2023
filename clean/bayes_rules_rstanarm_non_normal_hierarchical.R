# Load Packages ----
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)

data("climbers_sub")
climbers_sub %>% glimpse()

climbers_sub %>% 
    count(died, peak_name) %>% 
    filter(died == TRUE)

climbers = climbers_sub %>% 
    select(expedition_id, member_id, success, year, season,
           age, expedition_role, oxygen_used)

nrow(climbers)

climbers %>% 
    tabyl(success)

# Size per expedition
climbers %>% 
    group_by(expedition_id) %>% 
    summarise(members = n()) %>% 
    summarise(mean_members = mean(members),
              sd_members = sd(members),
              largest = max(members),
              smallest = min(members))

climbers_per_expedition = climbers %>% 
    group_by(expedition_id) %>% 
    summarise(count = n())

nrow(climbers_per_expedition)

# Success rate for each expedition
expedition_success = climbers %>% 
    group_by(expedition_id) %>% 
    summarise(success_rate = mean(success))

expedition_success %>% 
    ggplot(aes(x = success_rate)) +
    geom_histogram(color = "white")

# Success rate by age and oxygen use ----
climbers %>% glimpse()
 climbers %>% 
     tabyl(oxygen_used, success) 

 data_by_age_oxygen = climbers %>% 
     group_by(age, oxygen_used) %>% 
     summarise(success_rate_2 = sum(success == TRUE)/n(),
               success_rate = mean(success),
               n = n())
 
 data_by_age_oxygen %>% 
     arrange(desc(success_rate))
 
 ggplot(data_by_age_oxygen, aes(x = age, y = success_rate, color = oxygen_used)) +
     geom_point() +
     labs(
         title = "The Use of Oxygen is a Clear Predictor of Success Rate",
         x = "Age",
         y = "Mean Success Rate"
     ) + 
     theme_minimal()

 # Random intercepts logistic regression model ----
 # log(pi_ij/(1 - pi_ij)) = (beta_0 + b_0j) + beta_1 * X_ij1 + beta_2 * X_ij2
 
 # where b_0j\sigma_0 ~ N(0, sigma_0^2)
 # Expedition specific intercept beta_0j = underlying success rate measured by the
 # log(odds of success) for each expedition j.  These expedition specific intercepts
 # are assumed to be normally distributed around some global intercept beta_0 with 
 # standard deviation sigma_0. beta_0 baseline success rate across all expeditions.
 # sigma_0 captures the between group variability in success rates from expedition
 # to expedition.  beta_1 is the global relationship between success and age when
 # controlling for oxygen use.  beta_2 = global relationship between success and
 # oxygen use whe controlling for age.
 
 climb_model = stan_glmer(
     success ~ age + oxygen_used + (1 | expedition_id),
     data = climbers, family = binomial,
     prior_intercept = normal(0, 2.5, autoscale = TRUE),
     prior = normal(0, 2.5, autoscale = TRUE),
     prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
     chains = 4, iter = 5000*2, seed = 84735
 )

 
saveRDS(climb_model, "fits/bayes_rules_climb_model.rds")

climb_model = readRDS("fits/bayes_rules_climb_model.rds")

# Example use of mcmc diagnostics ----
 x <- example_mcmc_draws(chains = 4, params = 6)
 dim(x)
 dimnames(x)
 ?mcmc_rank_hist()

 # Confirm prior specification ----
 prior_summary(climb_model)
 

 # mcmc_trace(climb_model, size = 0.1)
 mcmc_trace(climb_model, regex_pars = "EVER04")
 
 climb_model$coefficients
 mcmc_rank_hist(climb_model, regex_pars = "EVER04")
 mcmc_rank_hist(climb_model, pars = c("age", "oxygen_usedTRUE"))

 color_scheme_set("viridisA")
 mcmc_rank_hist(climb_model, pars = c("age", "oxygen_usedTRUE"), ref_line = TRUE)
 mcmc_rank_overlay(climb_model, "age", ref_line = TRUE)
 
 mcmc_acf(climb_model, pars = c("age", "oxygen_usedTRUE"))
 neff_ratio(climb_model, pars = c("age", "oxygen_usedTRUE"))
 rhat(climb_model)
 
 # Posterior predictive check ----
 success_rate = function(x) {mean(x == 1)}

 color_scheme_set("brightblue")
 
 pp_check(climb_model, nreps = 100,
          plotfun = "stat", stat = "success_rate") +
     labs(title = "Proportion of climbers that were successful in each of 100 posterior simulated datasets", 
          x = "success rate")
 
 # Posterior analysis ----
 # Posterior summaries for global regression parameters
 # the age coefficient is comfortable below zero.  We have significant evidence
 # that, when controlling for whether or not a climber uses oxygen, the
 # likelihood decreases with age.  Translate the log(odds) to the odds scale:
 # There is an 80% chance that the odds of successfully summiting drop somewhere
 # between 
 
 global_params = tidy(climb_model, effects = "fixed", 
                      conf.int = TRUE, 
                      conf.level = 0.80)
 global_params
 beta_1_low = global_params$conf.low[2]
 beta_1_high = global_params$conf.high[2]
 beta_1_odds_low = exp(beta_1_low)
 beta_1_odds_high = exp(beta_1_high)
 
 print(paste0("80% chance the odds of successfully summiting drop by between ",
              round((1 - beta_1_odds_high)*100, 1), "% and ",
              round((1 - beta_1_odds_low)*100, 1), 
              "% for every additional year of age (controlled for oxygen use)"))
 
 beta_2_low = global_params$conf.low[3]
 beta_2_high = global_params$conf.high[3]
 beta_2_odds_low = exp(beta_2_low)
 beta_2_odds_high = exp(beta_2_high)
 
 print(paste0("80% chance the odds of successfully summiting increase by between ",
              round((beta_2_odds_low), 1), " and ",
              round((beta_2_odds_high), 1), 
              " fold when using oxygen vs not using oxygen (controlled for age)"))
 
 
 # Global model
global_intercept = global_params$estimate[1]
beta_1 = global_params$estimate[2]
beta_2 = global_params$estimate[3]
 
 print(paste0("log(odds) = ", round(global_intercept, 2), " ", 
              round(beta_1, 4), "*X_1 + ", round(beta_2, 2), "*X_2"))
 
 tidy(climb_model, effects = "fixed", conf.int = TRUE, conf.level = 0.80)
 tidy(climb_model, effects = "ran_pars", conf.int = TRUE, conf.level = 0.80)
 tidy(climb_model, effects = "ran_vals", conf.int = TRUE, conf.level = 0.80)
 
 # Plot 100 posterior plausible alternative models
 climbers %>% 
     add_epred_draws(climb_model, ndraws = 100, re_formula = NA) %>% 
     ggplot(aes(x = age, y = success, color = oxygen_used)) +
     geom_line(aes(y = .epred, group = paste(oxygen_used, .draw)),
               alpha = 0.1) +
     labs(title = "Effect of age and oxygen on success rate for observed expeditions",
          y = "probability of success") +
     theme_minimal()

 # Posterior classification ----
 # New expedition
 new_expedition = data.frame(
     age = c(20, 20, 60, 60), oxygen_used = c(FALSE, TRUE, FALSE, TRUE),
     expedition_id = rep("new", 4))

 new_expedition

 # Posterior predictions of binary outcome
set.seed(84735)
binary_prediction = posterior_predict(climb_model, newdata = new_expedition) 

# First 3 prediction sets
head(binary_prediction, 3)

# Summarise the posterior predictions of Y
# These probabilities incorporate uncertainty in the baseline success rate of the
# new expedition and are more moderate than plot of success rates for expeditions
# that we have observed.
colMeans(binary_prediction)

# Model evaluation
set.seed(84735)
class_summary = classification_summary(data = climbers, model = climb_model, cutoff = 0.5)
{ 
# classification_summary = function(model, data, cutoff = 0.5) {
#     if (!("stanreg" %in% class(model))) {
#         stop("the model must be a stanreg object.")
#     }
#     predictions <- posterior_predict(model, newdata = data)
#     if ("lmerMod" %in% class(model)) {
#         y <-
#             as.data.frame(data %>% dplyr::select(as.character(model$formula)[2]))[,
#                                                                                   1]
#     }
#     else {
#         y <- as.data.frame(data %>% dplyr::select(model$terms[[2]]))[,
#                                                                      1]
#     }
#     classifications <-
#         data.frame(proportion = colMeans(predictions)) %>%
#         mutate(classification = as.numeric(proportion >= cutoff)) %>%
#         mutate(y = y)
#     confusion_matrix <- classifications %>% tabyl(y, classification)
#     if (ncol(confusion_matrix) == 2) {
#         if ("1" %in% names(confusion_matrix)) {
#             confusion_matrix <- confusion_matrix %>% mutate(`0` = rep(0,
#                                                                       nrow(.)))
#         }
#         if ("0" %in% names(confusion_matrix)) {
#             confusion_matrix <- confusion_matrix %>% mutate(`1` = rep(0,
#                                                                       nrow(.)))
#         }
#     }
#     mat <- as.matrix(confusion_matrix[, -1])
#     sensitivity <- mat[2, 2] / sum(mat[2, ])
#     specificity <- mat[1, 1] / sum(mat[1, ])
#     overall_accuracy <- sum(diag(mat)) / sum(mat)
#     accuracy_rates <- data.frame(c(sensitivity, specificity,
#                                    overall_accuracy))
#     row.names(accuracy_rates) <- c("sensitivity", "specificity",
#                                    "overall_accuracy")
#     names(accuracy_rates) <- ""
#     return(list(confusion_matrix = confusion_matrix, accuracy_rates = accuracy_rates))
# }
}

class_summary$confusion_matrix

class_summary$accuracy_rates

# Model successfully predicts the outcomes for 91.7% of climbers
# Given the consequences of misclassification we should prioritise specificity, 
# our ability to anticipate when a climber might not succeed.  92.5%  To increase
# specificity we can increase the cutoff.
classification_summary(data = climbers, model = climb_model, cutoff = 0.65)

# Increasing specificity reduces our ability to predict when a climber will be successful
# i.e. our false negative rate does up

# Hierarchical Poisson and Negative Binomial Regression ----
# Load data 
data("airbnb")

# Number of listings
nrow(airbnb)

# Number of neighbourhoods
airbnb %>% glimpse()
unique(airbnb$neighborhood) %>% length()

airbnb %>% 
    summarise(nlevels(neighborhood))

# Analyse the number of AirBnB reviews between and within neighbourhoods

airbnb %>% 
    ggplot(aes(x = reviews)) +
    geom_histogram(color = "white", breaks = seq(0, 200, by = 10))

airbnb %>% 
    ggplot(aes(x = rating, y = reviews)) +
    geom_jitter()

airbnb %>% 
    ggplot(aes(x = room_type, y = reviews)) +
    geom_violin()

airbnb %>% 
    filter(neighborhood %in% c("Albany Park", "East Garfield Park", "The Loop")) %>% 
    ggplot(aes(x = rating, y = reviews, color =  room_type)) +
    geom_jitter() +
    facet_wrap(~neighborhood)

airbnb_model_1 = stan_glmer(
    reviews ~ rating + room_type + (1 | neighborhood),
    data = airbnb, family = poisson,
    prior_intercept = normal(3, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

pp_check(airbnb_model_1) +
    xlim(0, 200) +
    xlab("reviews")


airbnb_model_2 = stan_glmer(
    reviews ~ rating + room_type + (1 | neighborhood),
    data = airbnb, family = neg_binomial_2,
    prior_intercept = normal(3, 2.5, autoscale = TRUE),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_aux = exponential(1, autoscale = TRUE),
    prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
    chains = 4, iter = 5000*2, seed = 84735
)

pp_check(airbnb_model_2) +
    xlim(0, 200) +
    xlab("reviews")

# > Interpretation of parameters for negative binomial ----
(neg_binom_parameters = tidy(airbnb_model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80))

(beta_1_low = neg_binom_parameters$conf.low[2])
(beta_1_high = neg_binom_parameters$conf.high[2])
print(paste0("There is an 80% chance that reviews will increase by between ", round(exp(beta_1_low)*100 - 100, 1), " and ", 
             round(exp(beta_1_high)*100 - 100, 1), "% for every additional point in rating"))


(beta_3_low = neg_binom_parameters$conf.low[4])
(beta_3_high = neg_binom_parameters$conf.high[4])
print(paste0("There is an 80% chance that reviews for shared room listings will be between ", round(exp(beta_3_low)*100, 1), " and ", 
             round(exp(beta_3_high)*100, 1), "% as high as listings that are entirely private"))

# > Neighborhood specific models and Posterior Predictions ----
tidy(airbnb_model_2, effects = "ran_vals", conf.int = TRUE, conf.level = 0.80) %>% 
    select(-c(group, term)) %>% 
    filter(level %in% c("Albany_Park", "East_Garfield_Park", "The_Loop"))


# Simulate number of reviews for each neighborhood for listings that have a 5 
# star rating are completely private.

airbnb %>% names()
airbnb_model_2$formula
airbnb %>% glimpse()

unique(airbnb$room_type)
sim_reviews = data.frame(
    neighborhood = c("Albany_Park", "East_Garfield_Park", "The_Loop"),
    room_type = factor(rep("Entire home/apt", 3)),
    rating = rep(5, 3))
sim_reviews

predicted_reviews = posterior_predict(airbnb_model_2, newdata = sim_reviews)
mcmc_areas(predicted_reviews, prob = 0.8) +
    ggplot2::scale_y_discrete(labels = c("Albany_Park", "East_Garfield_Park", "The_Loop")) +
    labs(title = "Posterior Predictions of Reviews",
         subtitle = "Given 5 star rating and neigborhood",
         x = "Reviews") +
    xlim(0, 150)

set.seed(84735)
prediction_summary(airbnb_model_2, data = airbnb)      
