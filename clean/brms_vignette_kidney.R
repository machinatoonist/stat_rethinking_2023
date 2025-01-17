### Please note that the numerical results of Stan packages are only
### exactly reproducible with the same version of Stan, the same
### version of the C++ compiler, and the same OS. So minor differences
### in the numerical results are unavoidable.

### load the brms package and the kidney data set
library("brms")
data("kidney", package = "brms")
head(kidney, n = 3)
kidney %>% glimpse()

### make sure that a C++ compiler is installed and can be called within R
### for Rtools
system("g++ -v")
### for Xcode
system("clang++ -v")

### fit the first model to the kidney data
fit1 <- brm(formula = time | cens(censored) ~ age * sex + disease 
            + (1 + age | patient), 
            data = kidney, family = lognormal(),
            prior = c(set_prior("normal(0,5)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 2000, iter = 3000, chains = 4,
            control = list(adapt_delta = 0.95))

### more specific priors on the fixed effects 
(prior <- c(set_prior("normal(0,10)", class = "b", coef = "age"),
            set_prior("cauchy(1,2)", class = "b", coef = "sexfemale")))

### an overview on parameters and parameter classes to define priors on
get_prior(time | cens(censored) ~ age * sex + disease + (1 + age | patient), 
          data = kidney, family = lognormal())

### extract model code and data used to fit the model in Stan
stancode(fit1)
sdata <- standata(fit1)
names(sdata)

### obtain model summaries and plots
summary(fit1, waic = TRUE)
plot(fit1, ask = FALSE)
plot(marginal_effects(fit1), ask = FALSE)

### open shinystan in browser
### to stop shinystan, click 'save & close' on the top left of the browser window
### if you use RStudio, consider setting argument rstudio = TRUE
library(shinystan)
shinystan::launch_shinystan(fit1, rstudio = TRUE)

### compare the random effects standard deviations of Intercept and age
hypothesis(fit1, "Intercept - age > 0", class = "sd", group = "patient")

### fit a second model to the kidney data without a group-specific effect of age
fit2 <- update(fit1, formula. = ~ . - (1 + age | patient) + (1 | patient))

### obtain model summaries and plots
summary(fit2)
plot(fit2, ask = FALSE)

### compare fit1 and fit2 using leave-one-out cross-validation (LOO)
LOO(fit1, fit2)


### load the inhaler data set
data("inhaler", package = "brms")
head(inhaler, n = 1)

### fit a cumulative model to the inhaler data
fit3 <- brm(formula = rating ~ treat + period + carry + (1 | subject), 
            data = inhaler, family = cumulative)

### fit a stopping ratio model with equidistant thresholds 
### and category specific effects
fit4 <- brm(formula = rating ~ period + carry + cse(treat) + (1 | subject),
            data = inhaler, family = sratio(threshold = "equidistant"),
            prior = set_prior("normal(-1,2)", coef = "treat"))

### obtain model summaries and plots
summary(fit4, waic = TRUE)
plot(fit4, ask = FALSE)

# Kidney fit ----
fit_kidney1 = brm(time ~ age + sex, family = gaussian(), data = kidney)

pp_check(fit_kidney1, type = "ecdf_overlay")

fit_kidney2 = brm(time ~ age + sex, family = Gamma("log"), data = kidney)

pp_check(fit_kidney2, type = "ecdf_overlay")

# This example has a problem with prior specification due to exponentiation
fit_kidney3 = brm(time ~ age + sex, family = Gamma("log"), 
                  prior = prior(normal(0, 0.5)),
                  sample_prior = "only",
                  data = kidney)

pp_check(fit_kidney3, type = "ecdf_overlay")


fit_kidney4 = brm(time ~ standardize(age) + sex, 
                  family = Gamma("log"), 
                  data = kidney)

pp_check(fit_kidney4, type = "ecdf_overlay")

# Right censoring where reinfection did not occur during the period of the study
fit_kidney5 = brm(time | cens(censored) ~ age + sex, 
                  family = Gamma("log"), 
                  data = kidney)

pp_check(fit_kidney5, type = "error_scatter_avg")

?pp_check()


# Using splines and Gaussian processes ----

