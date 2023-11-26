# Spurious association ----
library(rethinking)
library(dagitty)

library(ggplot2)
library(dplyr)

data("WaffleDivorce")
d <- WaffleDivorce

# Standardise variables
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

model_fit_5_1 <- quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bA * A,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

# bA = 1 would imply that a change of one standard deviation in age at marriage
# is associated with a change of one standard deviation in divorce

# Simulate the priors using extract.prior and link
set.seed(10)
prior <- extract.prior(model_fit_5_1)
mu <- link(model_fit_5_1, post = prior, data = list(A = c(-2, 2)))
plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for (i in 1:50) lines(c(-2, 2), mu[i, ], col = col.alpha("black", 0.4))

# > Posterior predictions ----
# compute percentile interval of mean
A_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(model_fit_5_1, data = list(A = A_seq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# plot 
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu_mean, lwd = 2)
shade(mu_PI, A_seq)

precis(model_fit_5_1)

# > Divorce rate and marriage rate ----
# Fit a similar regression for divorce rate than marriage rate
model_fit_5_2 = quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bM * M,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

prior_2 <- extract.prior(model_fit_5_2)
mu_2 <- link(model_fit_5_2, post = prior_2, data = list(M = c(-2, 2)))
plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for (i in 1:50) lines(c(-2, 2), mu_2[i, ], col = col.alpha("black", 0.4))

M_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu_2 <- link(model_fit_5_2, data = list(M = M_seq))
mu_2_mean <- apply(mu_2, 2, mean)
mu_2_PI <- apply(mu_2, 2, PI)

plot(D ~ M, data = d, col = rangi2)
lines(M_seq, mu_2_mean, lwd = 2)
shade(mu_2_PI, M_seq)

precis(model_fit_5_2)

# > Drawing a DAG ----
dag5_1 <- dagitty("dag{A -> D; A-> M; M-> D}")
coordinates(dag5_1) <- list(x = c(A = 0, D = 1, M = 2), 
                            y = c(A = 0, D = 1, M = 0))
drawdag(dag5_1)

d %>% glimpse()
dma = data.frame(D = d$D, M = d$M, A = d$A)
dma %>% glimpse()
cor(dma)

# > Conditional independencies ----
DMA_dag2 <- dagitty('dag{D <- A -> M}')
impliedConditionalIndependencies(DMA_dag2)
# All pairs of variables should be associated, before conditioning
# on anything.  D and M should be independent after conditioning on A.

DMA_dag1 <- dagitty("dag{A -> D; A-> M; M-> D}")
impliedConditionalIndependencies(DMA_dag1)
# All pairs of variables should be associated, whatever we condition on

# > Multiple regression for divorce ----
model_fit_5_3 = quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bM * M + bA * A,
        a ~ dnorm(0, 1),
        bM ~ dnorm(0, 1),
        bA ~ dnorm(0, 1),
        sigma ~ dexp(1)
    ), data = d
)

precis(model_fit_5_3)
coeftab(model_fit_5_1, model_fit_5_2, model_fit_5_3)
plot(coeftab(model_fit_5_1, model_fit_5_2, model_fit_5_3), par = c("bA", "bM"))

# Predictor residual plots ----
# The average prediction error when we use all the other PREDICTOR variables
# to model a PREDICTOR of interest

# > Predict marriage rate using age at marriage ----
model_fit_5_4 = quap(
    alist(
        M ~ dnorm(mu, sigma),
        mu <- a + bAM * A,
        a ~ dnorm(0, 0.2),
        bAM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

mu <- link(model_fit_5_4)
mu_mean <- apply(mu, 2, mean)  # Find mean of all columns
length(mu_mean)
mu_resid <- d$M - mu_mean
mu_resid

res_df = data.frame(M = d$M, D = d$D, A = d$A, mu_mean = mu_mean, rM = mu_resid)

res_df %>% glimpse()

res_df %>% 
    ggplot(aes(x = rM, y = D)) +
    geom_point() +
    ylim(-1.7, 2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
        title = "Predictor residual plot",
        subtitle = "States to the right of the dashed line have higher marriage rates"
    ) +
    theme_minimal()


res_df %>% 
    ggplot(aes(x = A, y = M)) +
    geom_point() +
    ylim(-1.7, 2) +
    labs(
        title = "Correlation between M and A predictors"
    ) +
    theme_minimal()

# > Predict age at marriage using marriage rate ----
model_fit_5_4_b = quap(
    alist(
        A ~ dnorm(mu, sigma),
        mu <- a + bMA * M,
        a ~ dnorm(0, 0.2),
        bMA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

mu_a <- link(model_fit_5_4_b)
mu_mean_a <- apply(mu_a, 2, mean)  # Find mean of all columns
mu_resid_a <- d$M - mu_mean_a


res_df = res_df %>% 
    mutate(mu_mean_a = mu_mean_a,
           rA = A - mu_mean_a)

res_df %>% glimpse()

res_df %>% 
    ggplot(aes(x = rA, y = D)) +
    geom_point() +
    ylim(-1.7, 2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
        title = "Predictor residual plot - Fitting A with M",
        subtitle = "States to the right of the dashed line have higher average age at marriage",
        y = "Divorce rate",
        caption = "Predictive value of age at marriage after subtracting out the influence of the other predictors"
    ) +
    theme_minimal()

# TODO Add regression line
# TODO Add vertical lines to regression line to show residuals
res_df %>% 
    ggplot(aes(x = M, y = A)) +
    geom_point() +
    ylim(-1.7, 2) +
    labs(
        title = "Correlation between M and A predictors"
    ) +
    theme_minimal()

# Posterior prediction plots ----
# call link without specifying new data so it uses original data
mu <- link(model_fit_5_3)

# Summarise samples across cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations
D_sim <- sim(model_fit_5_3, n = 1e4)
D_PI <- apply(D_sim, 2, PI)

# par(mfrow = c(1, 1))  # Resetting to default single plot layout

plot(mu_mean ~ d$D, col = rangi2, ylim = range(mu_PI),
     xlab = "Observed divorce", ylab = "Predicted divorce")
abline(a = 0, b = 1, lty = 2)
for (i in 1:nrow(d)) lines(rep(d$D[i], 2), mu_PI[,i], col = rangi2)
# identify(x = d$D, y = mu_mean, labels = d$Loc)

ggplot(data = data.frame(d$D, mu_mean), aes(x = d$D, y = mu_mean)) +
    geom_point(color = rangi2) +
    geom_errorbar(aes(ymin = mu_PI[1,], 
                      ymax = mu_PI[2,]), 
                  width = 0, color = rangi2) +
    labs(title = "Model over-predicts states with low rates and 
under-predicts states with high rates of divorce",
         x = "Observed divorce", 
         y = "Predicted divorce") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    theme_minimal()

# Counterfactual plots ----
d %>% glimpse()

# > Pick a variable to manipulate, the intervention variable ----

model_fit_5_3_A = quap(
    alist(
        ## A -> D <- M
            D ~ dnorm(mu, sigma),
            mu <- a + bM * M + bA * A,
            a ~ dnorm(0, 0.2),
            bM ~ dnorm(0, 0.5),
            bA ~ dnorm(0, 0.5),
            sigma ~ dexp(1),
        ## A -> M
            M ~ dnorm(mu_M, sigma_M),
            mu_M <- aM + bAM * A,
            aM ~ dnorm(0, 0.2),
            bAM ~ dnorm(0, 0.5),
            sigma_M ~ dexp(1)
    ), data = d
)

precis(model_fit_5_3_A)

# > Define the range of values to set the intervention variable to ----
# A list of imaginary interventions
A_seq <- seq(from = -2, to = 2, length.out = 30)

# > For each value of the intervention variable, and for each sample in posterior,
# > Use the causal model to simulate the values of other variables, including the outcome ----
# Simulate both M and D in order.   We have to simulate the influence of A on M
# before we simulate the joint influence of A and M on D.

sim_dat = data.frame(A = A_seq)

# simulate M and then D, using A_seq
s <- sim(model_fit_5_3_A, data = sim_dat, vars = c("M", "D"))

plot(sim_dat$A, colMeans(s$D), ylim = c(-2, 2), type = "l",
     xlab = "manipulated A", ylab = "counterfactual D")
shade(apply(s$D, 2, PI), sim_dat$A)
mtext("Total counterfactual effect of A on D")



res_dat = sim_dat
res_dat$simD = colMeans(s$D)
levels_df = data.frame(t(apply(s$D, 2, PI)))
names(levels_df) = c("lower", "upper")
res_dat = cbind(res_dat, levels_df)

ggplot(res_dat, aes(x = A, y = simD)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "grey70", 
                alpha = 0.5) +
    labs(title = "Total counterfactual effect of A on D") +
    theme_minimal()

res_dat$simM = colMeans(s$M)
m_levels_df = data.frame(t(apply(s$M, 2, PI)))
names(m_levels_df) = c("m_low", "m_high")
res_dat = cbind(res_dat, m_levels_df)
res_dat %>% glimpse()

ggplot(res_dat, aes(x = A, y = simM)) +
    geom_line() +
    geom_ribbon(aes(ymin = m_low, ymax = m_high),
                fill = "grey70", 
                alpha = 0.5) +
    labs(title = "Total counterfactual effect of A on M") +
    theme_minimal()

# Expected causal effect of increasing median age at marriage from 20 to 30
# new data frame, standardised to mean 26.1 and std dev 1.26
(mean_age = mean(d$MedianAgeMarriage))
(sd_age = sd(d$MedianAgeMarriage))
# sim2_dat <- data.frame(A = (c(20, 30) - 26.1)/1.24)
sim2_dat <- data.frame(A = (c(20, 30) - mean_age)/sd_age)
s2 = sim(model_fit_5_3_A, data = sim2_dat, vars = c("M", "D"))
mean(s2$D[,2] - s2$D[, 1])


# Control the values of M, so A no longer influences it
sim_dat = data.frame(M = seq(from = -2, to = 2, length.out = 30), A = 0)

s = sim(model_fit_5_3_A, data = sim_dat, vars = "D")

plot(sim_dat$M, colMeans(s), ylim = c(-2, 2), type = "l",
     xlab = "manipulated M", ylab = "counterfactual D")
shade(apply(s, 2, PI), sim_dat$M)
mtext("Total counterfactual effect of M on D")

interval = data.frame(t(apply(s, 2, PI)))
names(interval) = c("lower", "upper")
typeof(interval)
res_dat = data.frame(
    M = sim_dat$M, 
    D = colMeans(s), 
    lower = interval$lower,
    upper = interval$upper)

ggplot(res_dat, aes(x = M, y = D)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.4) +
    labs(
        title = "Total counterfactual effect of M on D",
        x = "manipulated M",
        y = "counterfactual D") +
    ylim(-2, 2) +
    theme_minimal()

# > Simulating counterfactuals ----
# Define a range of values that we want to assign to A
A_seq = seq(from = -2, to = 2, length.out = 30)

# Extract the posterior samples of model parameters to simulate observations for each set of samples
post = extract.samples(model_fit_5_3_A)
post %>% glimpse()

# First simulate M
M_sim = with(post, sapply(1:30,
                          function(i) rnorm(1e3, 
                                            mean = aM + bAM * A_seq[i], 
                                            sd = sigma_M)))

# Then simulate D
D_sim = with(post, sapply(1:30,
                          function(i) rnorm(1e3,
                                            mean = a + bM * M_sim[i] + bA * A_seq[i],
                                            sd = sigma)))

M_interval = data.frame(t(apply(M_sim, 2, PI)))
names(M_interval) = c("low_m", "high_m")
D_interval = data.frame(t(apply(D_sim, 2, PI)))
names(D_interval) = c("low_d", "high_d")

sim_dat = data.frame(A = A_seq, 
                     M_sim = colMeans(M_sim),
                     low_m = M_interval$low_m,
                     high_m = M_interval$high_m,
                     D_sim = colMeans(D_sim),
                     low_d = D_interval$low_d,
                     high_d = D_interval$high_d)


ggplot(sim_dat, aes(x = A, y = D_sim)) +
    geom_line() +
    geom_ribbon(aes(ymin = low_d, ymax = high_d), fill = "grey70", alpha = 0.4) +
    labs(
        title = "Total counterfactual effect of A on D",
        x = "manipulated A",
        y = "counterfactual D") +
    theme_minimal()

ggplot(sim_dat, aes(x = A, y = M_sim)) +
    geom_line() +
    geom_ribbon(aes(ymin = low_m, ymax = high_m), fill = "grey70", alpha = 0.4) +
    labs(
        title = "Total counterfactual effect of A on M",
        x = "manipulated A",
        y = "counterfactual M") +
    theme_minimal()


ggplot(sim_dat, aes(x = M_sim, y = D_sim)) +
    geom_line() +
    geom_ribbon(aes(ymin = low_d, ymax = high_d), fill = "grey70", alpha = 0.4) +
    labs(
        title = "Simulated associate between M on D",
        x = "counterfactual M",
        y = "counterfactual D") +
    theme_minimal()

# Masked relationships ----
library(rethinking)
library(dplyr)
library(ggplot2)

data(milk)
d = milk
str(d)
unique(d$species)

# Consider kilocalories of energy per gram of milk, mass (average female body mass in kg)
# and percent of total brain mass that is neocortex mass

# Hypothesis: Primates with larger brains produce more energetic milk so brains can grow quickly
d$K = standardize(d$kcal.per.g)
d$N = standardize(d$neocortex.perc)
d$M = standardize(log(d$mass))

# model_fit_5_draft = quap(
#     alist(
#         K ~ dnorm(mu, sigma),
#         mu <- a + bN * N,
#         a ~ dnorm(0, 1),
#         bN ~ dnorm(0, 1),
#         sigma ~ dexp(1)
#     ), data = d
# )

# Error is caused by missing values in the N column
d %>% glimpse()

# Drop incomplete cases
dcc = d[complete.cases(d$K, d$N, d$M), ]


model_fit_5_5_draft = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bN * N,
        a ~ dnorm(0, 1),
        bN ~ dnorm(0, 1),
        sigma ~ dexp(1)
    ), data = dcc
)

# > Extract priors ----
prior = extract.prior(model_fit_5_5_draft)
xseq = c(-2, 2)
mu = link(model_fit_5_5_draft, post = prior, data = list(N = xseq))

mu

plot(NULL, xlim = xseq, ylim = xseq)
for (i in 1:50) lines(xseq, mu[i,], col = col.alpha("black", 0.3))

model_fit_5_5 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bN * N,
        a ~ dnorm(0, 0.2),
        bN ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = dcc
)

prior = extract.prior(model_fit_5_5)
xseq = c(-2, 2)
mu = link(model_fit_5_5, post = prior, data = list(N = xseq))

plot(NULL, xlim = xseq, ylim = xseq)
for (i in 1:50) lines(xseq, mu[i,], col = col.alpha("black", 0.3))

precis(model_fit_5_5)

# > Plot the predicted mean and 89% compatibility interval ----
dim(dcc)
xseq = seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu = link(model_fit_5_5, data = list(N = xseq))
dim(mu)
length(xseq)
mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)
plot(K ~ N, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

mu_PI = data.frame(t(mu_PI))
names(mu_PI) = c("lower_K", "upper_K")

res_dat = data.frame(
    N = xseq,
    K = mu_mean,
    lower_K = mu_PI$lower_K,
    upper_K = mu_PI$upper_K
    
)

dim(res_dat)

ggplot(dcc, aes(x = N, y = K)) +
    geom_point(size = 2) +
    geom_line(data = res_dat, aes(x = N, y = K)) +
    geom_ribbon(data = res_dat, 
                aes(ymin = lower_K, ymax = upper_K),
                fill = "grey70",
                alpha = 0.5) +
    theme_minimal()


# Consider a bivariate relationship between kilocalories and body mass 
model_fit_5_6 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bM * M,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = dcc
)
precis(model_fit_5_6)

xseq = seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu = link(model_fit_5_6, data = list(M = xseq))
mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)
plot(K ~ M, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

mu_PI = data.frame(t(mu_PI))
names(mu_PI) = c("lower_K", "upper_K")

res_dat = data.frame(
    M = xseq,
    K = mu_mean,
    lower_K = mu_PI$lower_K,
    upper_K = mu_PI$upper_K
    
)

ggplot(dcc, aes(x = M, y = K)) +
    geom_point(size = 2) +
    geom_line(data = res_dat, aes(x = M, y = K)) +
    geom_ribbon(data = res_dat, 
                aes(ymin = lower_K, ymax = upper_K),
                fill = "grey70",
                alpha = 0.5) +
    theme_minimal()

# > Multivariate model ----
model_fit_5_7 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bM * M + bN * N,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        bN ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = dcc
)
precis(model_fit_5_7)

plot(coeftab(model_fit_5_5, model_fit_5_6, model_fit_5_7), pars = c("bM", "bN"))

pairs(~K + M + N, dcc)
# M and N are correlated and have opposite correlations with the feature of interest

# > Counterfactual plots ----
xseq = seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu = link(model_fit_5_7, data = data.frame(M = xseq, N = 0))
mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K))
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

mu_PI = data.frame(t(mu_PI))
names(mu_PI) = c("lower_K", "upper_K")

res_dat = data.frame(
    M = xseq,
    K = mu_mean,
    lower_K = mu_PI$lower_K,
    upper_K = mu_PI$upper_K
    
)

ggplot(dcc, aes(x = M, y = K)) +
    geom_point(size = 2) +
    geom_line(data = res_dat, aes(x = M, y = K)) +
    geom_ribbon(data = res_dat, 
                aes(ymin = lower_K, ymax = upper_K),
                fill = "grey70",
                alpha = 0.5) +
    theme_minimal()

xseq = seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu = link(model_fit_5_7, data = data.frame(N = xseq, M = 0))
mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$N), ylim = range(dcc$K))
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

mu_PI = data.frame(t(mu_PI))
names(mu_PI) = c("lower_K", "upper_K")

res_dat = data.frame(
    N = xseq,
    K = mu_mean,
    lower_K = mu_PI$lower_K,
    upper_K = mu_PI$upper_K
    
)

ggplot(dcc, aes(x = N, y = K)) +
    geom_point(size = 2) +
    geom_line(data = res_dat, aes(x = N, y = K)) +
    geom_ribbon(data = res_dat, 
                aes(ymin = lower_K, ymax = upper_K),
                fill = "grey70",
                alpha = 0.5) +
    theme_minimal()


# > Simulating a masking relationship ----
# M -> K <- N
# M -> N
n = 1000
M = rnorm(n)
N = rnorm(n, M)
K = rnorm(n, N - M)
d_sim = data.frame(K = K, N = N, M = M)


model_fit_5_5_s = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bN * N,
        a ~ dnorm(0, 0.2),
        bN ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d_sim
)

precis(model_fit_5_5_s)
prior = extract.prior(model_fit_5_5_s)
xseq = c(-2, 2)
mu = link(model_fit_5_5_s, post = prior, data = list(N = xseq))

plot(NULL, xlim = xseq, ylim = xseq)
for (i in 1:50) lines(xseq, mu[i,], col = col.alpha("black", 0.3))

model_fit_5_6_s = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bM * M,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d_sim
)
precis(model_fit_5_6_s)

xseq = seq(from = min(d_sim$M) - 0.15, to = max(d_sim$M) + 0.15, length.out = 30)
mu = link(model_fit_5_6_s, data = list(M = xseq))
mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)
plot(K ~ M, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

model_fit_5_7_s = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bM * M + bN * N,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        bN ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d_sim
)
precis(model_fit_5_7_s)

plot(coeftab(model_fit_5_5_s, model_fit_5_6_s, model_fit_5_7_s), pars = c("bM", "bN"))

pairs(d_sim)


# Markov equivalence set ----
library(dagitty)
dags_5_7 = dagitty("dag{
                   M -> K <- N
                   M -> N }")
coordinates(dags_5_7) <- list(x = c(M = 0, K = 1, N = 2), y = c(M = 0.5, K = 1, N = 0.5))
MElist = equivalentDAGs(dags_5_7)

drawdag(MElist)
# I think we can eliminate the two dags with arrows coming out of K because mas and neocortex
# percentage weight are unlikely to be listening to the amount of energy in milk since this
# signal would need to pass from the gut through to the genome which is not likely.

# Categorical variables ----
data("Howell1")
d = Howell1
str(d)

# > Using dummy variables ----
# Could fit a model with a term with a dummy variable for male
# When the subject is male M = 1 the parameter bM influences the prediction
# When the subject is female, M = 0 and bM has no effect
# bM represents the expected difference between males and females in height
# a is no longer the average height but just the average female height
# mu <- a + bM * M 

# A consequence of this is that we have to assign a prior to the difference which
# has the consequence of assuming there is more uncertainty about one of the 
# categories
mu_female = rnorm(1e4, 178, 20)
mu_male = rnorm(1e4, 178, 20) + rnorm(1e4, 0, 10)
precis(data.frame(mu_female, mu_male))

# > Using index variables ----
d$sex = ifelse(d$male == 1, 2, 1)
str(d$sex)

# This creates a list of 'a' parameters, one for each unique value in the index variable.
# In this case we end up with two 'a' parameters a[1] and a[2]
# Now the same prior can be assigned to each with neither having more or less uncertainty.
model_fit_5_8 = quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a[sex],
        a[sex] ~ dnorm(178, 20),
        sigma ~ dunif(0, 50)
    ), data = d
)

precis(model_fit_5_8, depth = 2)

str(d)
model_fit_5_8_d = quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + bM * male,
        a ~ dnorm(178, 20),
        bM ~ dnorm(0, 10),
        sigma ~ dunif(0, 50)
    ), data = d
)

precis(model_fit_5_8_d, depth = 2)

# > Expected difference in the height of females and males ----
post = extract.samples(model_fit_5_8)
post$diff_fm = post$a[,1] - post$a[,2]
precis(post, depth = 2)
