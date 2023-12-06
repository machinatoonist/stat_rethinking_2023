library(rethinking)
library(dagitty)

# Simulate Berkson's Paradox ----
# The correlation of two variables that are independent and causally
# related to an outcome that involves selection, such as grant proposals.
set.seed(1914)
N = 200
p = 0.1
# uncorrelated newsworthiness and trustworthiness 
nw = rnorm(N)
tw = rnorm(N)
# select top 10% of combined scores
s = nw + tw # total score
q = quantile(s, probs = 1 - p)
q
selected = ifelse(s >= q, TRUE, FALSE)
plot(nw[selected], tw[selected])
cor(nw[selected], tw[selected])

# Multicollinearity ----
N = 100
set.seed(909)
height = rnorm(N, 10, 2)
leg_prop = runif(N, 0.4, 0.5)
leg_left = leg_prop * height + rnorm(N, 0, 0.02)
leg_right = leg_prop * height + rnorm(N, 0, 0.02)

d = data.frame(height, leg_left, leg_right)

m6.1 = quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + bl * leg_left + br * leg_right,
        a ~ dnorm(10, 100),
        bl ~ dnorm(2, 10),
        br ~ dnorm(2, 10),
        sigma ~ dexp(1)
        
    ), data = d,
    verbose = TRUE
)

precis(m6.1)
plot(precis(m6.1))


# The multiple linear regression answers the question: What is the the value
# of knowing each predictor, after already knowing all of the other predictors?

# i.e. including both leg lengths is redundant,  there is little value in
# knowing each leg's length after already knowing the other leg's length

post = extract.samples(m6.1)

plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)

# The model has produced a good estimate of the sum of bl and br.

# Posterior distribution of the sum:
sum_blbr = with(post, expr = bl + br)
dens(sum_blbr, col = rangi2, lwd = 2, xlab = "Sum of bl and br")

m6.2 = quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu ~ a + bl * leg_left,
        a ~ dnorm(10, 100),
        bl ~ dnorm(2, 10),
        sigma ~ dexp(1)
    ), data = d
)

precis(m6.2)

# Conclusion: When two predictor variables are very strongly correlated
# with each other, including both in a model may lead to confusion.

# Multi-collinear milk ----
# What to do when the collinearity exists in real data that is not anticipated?
library(rethinking)

data("milk")
d = milk
d$K = standardize(d$kcal.per.g)
d$F = standardize(d$perc.fat)
d$L = standardize(d$perc.lactose)

pairs(d)

# kcal.per.g regressed on perc.fat
m6.3 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bF * F,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

# kcal.per.g regessed on perc.lactose
m6.4 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bL * L,
        a ~ dnorm(0, 0.2),
        bL ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

precis(m6.3)
precis(m6.4)

# Both models have narrow posteriors on one size or other of zero
# Fitting with both the posterior means are closer to zero and the
# sd of both are twice as large
m6.4 = quap(
    alist(
        K ~ dnorm(mu, sigma),
        mu <- a + bF * F + bL * L,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        bL ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

pairs(~kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)

precis(m6.4)

post = extract.samples(m6.4)
post

dens(post$bF, dnorm(0, .5))
dens(post)
post$Fp = rnorm(n = length(post$bF), mean = 0, sd = .5)
dens(post)

# Simulating collinearity ----
sim.coll = function(r = 0.9) {
    d$x <- rnorm(nrow(d), mean = r * d$perc.fat,
                 sd = sqrt((1 - r^2) * var(d$perc.fat)))
    m <- lm(kcal.per.g ~ perc.fat + x, data = d)
    sqrt(diag(vcov(m)))[2] # stddev of parameter
}
rep.sim.coll <- function(r = 0.9, n = 100) {
    stddev <- replicate(n, sim.coll(r))
    mean(stddev)
}
r.seq <- seq(from = 0, to = 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r = z, n = 100))
plot(stddev ~ r.seq, type = "l", col = rangi2, lwd = 2, xlab = "correlation")

# Post treatment bias ----
# Simulate some example data where you are growing some plants in a greehouse.
# You want to know the difference in growth under different anti-fungus soil
# treatments, because fungus on the plants tends to reduce their growth.
# Plants are seeded, sprout and their heights are measured.  The different
# soil treatments are applied.  Final measures are the height of the plant 
# and the presence of the fungus.
# There are 4 variables: initial height, final height, treatment, presence of fungus

# If your goal is to make a causal inference about the treatment, you shouldn't
# include the fungus, because it is a post-treatment effect.
set.seed(71)
# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N, 10, 2)

# assign treatments and simulate fungus and growth
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, 5 - 3 * fungus)

# compose a clean data frame
d <- data.frame(h0 = h0, h1 = h1, treatment = treatment, fungus = fungus)
precis(d)

# Defining p as the proportion of p = h1,i/h0,i
# Ensure that p > 0 since plant height cannot be negative.  p can be 
# less than 1 for the case where the plant is killed.
sim_p <- rlnorm(1e4, 0, 0.25)
precis(data.frame(sim_p))

m6.6 <- quap(
    alist(
        h1 ~ dnorm(mu, sigma),
        mu <- h0 * p,
        p ~ dlnorm(0, 0.25),
        sigma ~ dexp(1)
        ), data = d)

precis(m6.6)

# Approximate the posterior
# Including fungus in the model is asking the question: Once we already know
# whether or a not a plant developed fungus, does soil treatment matter?
# The answer is not because soil treatment has its effect on growth through
# reducing fungus.
m6.7 <- quap(
    alist(
        h1 ~ dnorm(mu, sigma),
        mu <- h0 * p,
        p <- a + bt * treatment + bf * fungus,
        a ~ dlnorm(0, 0.2),
        bt ~ dnorm(0, 0.5),
        bf ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

precis(m6.7)
plot(precis(m6.7))

# To answer the question: What is the impact of treatment on growth?
# We must omit the post-treatment variable fungus.
m6.8 = quap(
    alist(
        h1 ~ dnorm(mu, sigma),
        mu <- h0 * p,
        p <- a + bt * treatment,
        a ~ dlnorm(0, 0.2),
        bt ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d
)

# Now the effect of treatment clearly positive, as it should be.
# Controlling for pre-treatment differences, like h0, that can mask the
# causal effect of treatment.  Including post treatment variables can actually
# mask the treatment itself.

# Fungus and d-separation ----
# That including fungus zeros the coefficient for treatment suggests that
# treatment works for exactly the anticipated reasons.
# Inference about the treatment depends on removing the post-treatment variable.

plant_dag <- dagitty( "dag {
                      H_0 -> H_1
                      F -> H_1
                      T -> F
                      }")

coordinates(plant_dag) <- list(x = c(H_0 = 0, T = 2, F = 1.5, H_1 = 1),
                               y = c(H_0 = 0, T = 0, F = 0, H_1 = 0)
)

drawdag(plant_dag)

# Including F in the model blocks the path from T to H1.
# Why?  There is no information in T about H1 that is not also in F.
# Once we know F, learning T provides no additional information about H1.
impliedConditionalIndependencies(plant_dag)
# H_1 is independent of T given F

# Create a data set with a common cause of H1 (final plant height) and fungus (F)
set.seed(71)
N = 1000
h0 = rnorm(N, 10, 2)
treatment = rep(0:1, each = N/2)
M = rbern(N)
# Moisture increases chance of fungus.
# Treatment reduces change of fungus.
fungus = rbinom(N, size = 1, prob = 0.5 - treatment * 0.4 + 0.4 * M)
# Final plant height
h1 = h0 + rnorm(N, 5 + 3 * M)
d2 = data.frame(h0 = h0, h1 = h1, treatment = treatment, fungus = fungus)
d2 %>% glimpse()

# > Refit the models ----

m6.6d <- quap(
    alist(
        h1 ~ dnorm(mu, sigma),
        mu <- h0 * p,
        p ~ dlnorm(0, 0.25),
        sigma ~ dexp(1)
    ), data = d2)

precis(m6.6d)

m6.7d <- quap(
    alist(
        h1 ~ dnorm(mu, sigma),
        mu <- h0 * p,
        p <- a + bt * treatment + bf * fungus,
        a ~ dlnorm(0, 0.2),
        bt ~ dnorm(0, 0.5),
        bf ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d2
)

# Compare new fit with previous
precis(m6.7)
plot(precis(m6.7))

# Now it looks like fungus is positively effecting growth
precis(m6.7d)
plot(precis(m6.7d))

# To answer the question: What is the impact of treatment on growth?
# We must omit the post-treatment variable fungus.
m6.8d = quap(
    alist(
        h1 ~ dnorm(mu, sigma),
        mu <- h0 * p,
        p <- a + bt * treatment,
        a ~ dlnorm(0, 0.2),
        bt ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = d2
)

# Removing fungus from the model previously demonstrated that treatment 
# has a positive effect on growth
precis(m6.8)
plot(m6.8)

# With the latent common cause of fungus and H1 this effect disappears
precis(m6.8d)
plot(m6.8d)

# Collider Bias ----
d <- sim_happiness(seed = 1977, N_years = 1000)
precis(d)
d %>% glimpse()

d2 = d[d$age > 17, ] # Filter only adults
d2$A = (d2$age - 18)/(65 - 18) # Rescale age so that the range from 18 to 65 is one unit

precis(d2)

# Construct the marriage status index variable
d2$mid = d2$married + 1
d2 %>% glimpse()
m6.9 = quap(
    alist(
        happiness ~ dnorm(mu, sigma),
        mu <- a[mid] + bA * A,
        a[mid] ~ dnorm(0, 1),
        bA ~ dnorm(0, 2),
        sigma ~ dexp(1)
    ), data = d2
)
# Age is negatively correlated with happiness! But the simulated data had no
# such relationship.  What went wrong?

precis(m6.9, depth = 2)

# Compare the inferences from this model with a model that omits marriage status
m6.10 = quap(
    alist(
        happiness ~ dnorm(mu, sigma),
        mu <- a + bA * A,
        a ~ dnorm(0, 1),
        bA ~ dnorm(0, 2),
        sigma ~ dexp(1)
    ), data = d2
)

# Now there is no relationship between age and happiness
precis(m6.10)

# When we condition on a collider such as marriage this is exactly what we 
# should expect.  Age and happiness independently influence marriage.  When
# we condition on marriage we induce a spurious association between age and 
# happiness.

# > Haunted DAG ----
# Simulate the effect of Grandparents (G) and Parents (P) on the educational
# achievement of their children (C)
# P is some function of G and U
# C is some function of G, P and U
# G and U are not functions of any other known variables

N <- 200
# Define some arbitrary strength of association
b_GP = 1 # direct effect of G on P
b_GC = 0 # direct effect of G on C
b_PC = 1 # direct effect of P on C
b_U = 2 # direct effect of the neighbourhood (U) on P and C

# Use these slopes to draw random conclusions
set.seed(1)
U <- 2 * rbern(N, 0.5) - 1
G <- rnorm(N)
P <- rnorm(N, b_GP * G + b_U * U)
C <- rnorm(N, b_PC * P + b_GC * G + b_U * U)
d <- data.frame(C = C, P = P, G = G, U = U)

d %>% glimpse()

m6.11 = quap(
    alist(
        C ~ dnorm(mu, sigma),
        mu <- a + b_PC * P + b_GC * G,
        a ~ dnorm(0, 1),
        c(b_PC, b_GC) ~ dnorm(0, 1),
        sigma ~ dexp(1)
    ), data = d
)
precis(m6.11)


# The latent variable U is a collider on P.  Once we know P, learning G
# invisibly tells us about the neighbourhood U, and U is associated with the
# outcome C.  Conditioning on P produces collider bias so we have to measure
# U.

m6.12 = quap(
    alist(
        C ~ dnorm(mu, sigma),
        mu <- a + b_PC * P + b_GC * G + b_U * U,
        a ~ dnorm(0, 1),
        c(b_PC, b_GC, b_U) ~ dnorm(0, 1),
        sigma ~ dexp(1)
    ), data = d
)

# Conditioning on U, P and G extracts this original slopes used in the simulation
precis(m6.12)

# > Adjustment sets ----
library(dagitty)
dag6.1 = dagitty("dag {
                 U [unobserved]
                 X -> Y
                 X <- U <- A -> C -> Y
                 U -> B <- C 
                 }")

adjustmentSets(dag6.1, exposure = "X", outcome = "Y")

# We want to measure the total causal effects of the number of Waffle Houses
# divorce rate in each State.  Find the minimum adjustment set that will 
# block backdoor paths from Waffle House to divorce.
dag6.2 = dagitty("dag {
                 A -> D
                 A -> M -> D
                 A <- S -> M
                 S -> W -> D
                 }")

# Find the elemental confounds
adjustmentSets(dag6.2, exposure = "W", outcome = "D")

# Find conditional independencies
impliedConditionalIndependencies(dag6.2)

# End of chapter practice ----
# > 6M1 ----
library(ggdag)
dag_coords <- tibble(name = c("X", "U", "A", "B", "C", "Y", "V"),
                     x = c(1, 1, 2, 2, 3, 3, 3.5),
                     y = c(1, 2, 2, 1.5, 2, 1, 1.5))

dagify(Y ~ X + C + V,
       X ~ U,
       U ~ A,
       B ~ U + C,
       C ~ A + V,
       coords = dag_coords) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(data = . %>% filter(name %in% c("U", "V")),
                   shape = 1, stroke = 2, color = "black") +
    geom_dag_text(color = "black", size = 10) +
    geom_dag_edges(edge_color = "black", edge_width = 2,
                   arrow_directed = grid::arrow(length = grid::unit(15, "pt"),
                                                type = "closed")) +
    theme_void()

# B and C are colliders so conditioning on these will open the path
new_dag <- dagitty("dag { U [unobserved]
                          V [unobserved]
                          X -> Y
                          X <- U <- A -> C -> Y
                          U -> B <- C
                          C <- V -> Y }")

adjustmentSets(new_dag, exposure = "X", outcome = "Y")

# > 6M2 ----
set.seed(1984)

n <- 1000
dat <- tibble(x = rnorm(n)) %>%
    mutate(z = rnorm(n, mean = x, sd = 0.1),
           y = rnorm(n, mean = z)
           ,across(everything(), standardize)
           )

dat %>% glimpse()
sim_cor <- cor(dat$x, dat$z)
sim_cor

b6m2.1 = quap(
    alist(
        y ~ dnorm(mu, sigma),
        mu <- a + b_x * x + b_z * z,
        a ~ dnorm(0, 0.2),
        b_x ~ dnorm(0, 0.5),
        b_z ~ dnorm(0, 0.5),
        sigma ~ dexp(1)
    ), data = dat
)

precis(b6m2.1)

b6m2 <- brm(y ~ 1 + x + z, data = dat, family = gaussian,
            prior = c(prior(normal(0, 0.2), class = Intercept),
                      prior(normal(0, 0.5), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = here("fits", "chp6", "b6m2"))

as_draws_df(b6m2) %>%
    as_tibble() %>% 
    select(b_Intercept, b_x, b_z, sigma) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = value, y = name)) +
    stat_halfeye(.width = c(0.67, 0.89, 0.97))

# > 6M3 ----
# What do we need to control for to measure the total causal influence of X on Y
dag1 <- dagitty("dag{ X <- Z <- A -> Y <- X; Y <- Z }")
adjustmentSets(dag1, exposure = "X", outcome = "Y")

dag2 <- dagitty("dag{ X -> Z <- A -> Y <- X; X -> Z -> Y }")
adjustmentSets(dag2, exposure = "X", outcome = "Y")

dag3 <- dagitty("dag{ X <- A -> Z <- Y; X -> Z <- Y}")
adjustmentSets(dag3, exposure = "X", outcome = "Y")


dag4 <- dagitty("dag{ X <- A -> Z -> Y; X -> Z -> Y; X -> Y}")
adjustmentSets(dag4, exposure = "X", outcome = "Y")

# > 6H1 ----
waffle_dag <- dagitty("dag { S -> W -> D <- A <- S -> M -> D; A -> M }")
coordinates(waffle_dag) <- list(x = c(A = 1, S = 1, M = 2, W = 3, D = 3),
                                y = c(A = 1, S = 3, M = 2, W = 3, D = 1))

ggplot(waffle_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_text(color = "black", size = 10) +
    geom_dag_edges(edge_color = "black", edge_width = 2,
                   arrow_directed = grid::arrow(length = grid::unit(15, "pt"),
                                                type = "closed")) +
    theme_void()

adjustmentSets(waffle_dag, exposure = "W", outcome = "D")

data("WaffleDivorce")
d = WaffleDivorce
d %>% glimpse()
d$W = standardize(d$WaffleHouses)
d$S = factor(d$Location)

m6h1.1 = quap(
    alist(
        W ~ dnorm(mu, sigma),
        mu <- a + b_W[S] * W,
        b_W[S] ~ dnorm(0, .5),
        sigma ~ dexp(1)
    ), data = d
)

precis(m6h1.1, depth = 2)

plot(precis(m6h1.1, depth = 2))

waffle <- WaffleDivorce %>%
    as_tibble() %>%
    select(D = Divorce,
           A = MedianAgeMarriage,
           M = Marriage,
           S = South,
           W = WaffleHouses) %>%
    mutate(across(-S, standardize),
           S = factor(S))

waff_mod <- brm(D ~ 1 + W + S, data = waffle, family = gaussian,
                prior = c(prior(normal(0, 0.2), class = Intercept),
                          prior(normal(0, 0.5), class = b),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                file = here("fits", "chp6", "b6h1"))

spread_draws(waff_mod, b_W) %>%
    ggplot(aes(x = b_W)) +
    stat_halfeye(.width = c(0.67, 0.89, 0.97)) +
    labs(x = expression(beta[W]), y = "Density")
