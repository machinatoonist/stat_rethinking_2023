data(rugged, package = "rethinking")
d <- rugged
rm(rugged)

# may as well load this, too
library(brms)

library(tidyverse)
library(ggthemes)

theme_set(
    theme_pander() +
        theme(text = element_text(family = "Times"),
              panel.background = element_rect(color = "black")) 
)

# DAGs ----
library(ggdag)

dag_coords <-
    tibble(name = c("R", "G", "C", "U"),
           x    = c(1, 2, 3, 2),
           y    = c(2, 2, 2, 1))

dagify(R ~ U,
       G ~ R + U + C,
       coords = dag_coords) %>%
    
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = name == "U"),
                   alpha = 1/2, size = 6, show.legend = F) +
    geom_point(x = 2, y = 1, 
               size = 6, shape = 1, stroke = 3/4, color = palette_pander(n = 2)[2]) +
    geom_dag_text(color = "black", family = "Times") +
    geom_dag_edges() +
    scale_colour_pander() +
    theme_dag()

dag_coords <-
    tibble(name = c("G", "R", "H", "C", "U"),
           x    = c(1, 1.5, 2.5, 3.5, 1),
           y    = c(3, 2, 2, 2, 1))

dagify(G ~ R + U + H,
       R ~ U,
       H ~ R + U + C,
       coords = dag_coords) %>%
    
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(aes(color = name == "U"),
                   alpha = 1/2, size = 6, show.legend = F) +
    geom_point(x = 1, y = 1, 
               size = 6, shape = 1, stroke = 3/4, color = palette_pander(n = 2)[2]) +
    geom_dag_text(color = "black", family = "Times") +
    geom_dag_edges() +
    scale_colour_pander() +
    theme_dag()

# Data preparation ----
# make the log version of criterion
d <- 
    d %>%
    mutate(log_gdp = log(rgdppc_2000))

# extract countries with GDP data
dd <-
    d %>%
    filter(complete.cases(rgdppc_2000)) %>% 
    # re-scale variables
    mutate(log_gdp_std = log_gdp / mean(log_gdp), 
           rugged_std  = rugged / max(rugged))
library(ggrepel)
library(patchwork)

# African nations
p1 <-
    dd %>% 
    filter(cont_africa == 1) %>% 
    ggplot(aes(x = rugged_std, y = log_gdp_std)) +
    geom_smooth(method = "lm", formula = y ~ x,
                fill = palette_pander(n = 2)[1],
                color = palette_pander(n = 2)[1]) +
    geom_point(color = palette_pander(n = 2)[1]) +
    geom_text_repel(data = . %>% 
                        filter(country %in% c("Lesotho", "Seychelles")),  
                    aes(label = country), 
                    size = 3, family = "Times", seed = 8) +
    labs(subtitle = "African nations",
         x = "ruggedness (standardized)",
         y = "log GDP (as proportion of mean)")

# Non-African nations
p2 <-
    dd %>% 
    filter(cont_africa == 0) %>% 
    ggplot(aes(x = rugged_std, y = log_gdp_std)) +
    geom_smooth(method = "lm", formula = y ~ x,
                fill = palette_pander(n = 2)[2],
                color = palette_pander(n = 2)[2]) +
    geom_point(color = palette_pander(n = 2)[2]) +
    geom_text_repel(data = . %>% 
                        filter(country %in% c("Switzerland", "Tajikistan")),  
                    aes(label = country), 
                    size = 3, family = "Times", seed = 8) +
    xlim(0, 1) +
    labs(subtitle = "Non-African nations",
         x = "ruggedness (standardized)",
         y = "log GDP (as proportion of mean)")

# combine
p1 + p2 + plot_annotation(title = "Figure 8.2. Separate linear regressions inside and outside of Africa")

mean(dd$rugged_std)

# A naïve translation of McElreath’s rethinking code into a brms::brm() formula 
# argument might be log_gdp_std ~ 1 + (rugged_std - 0.215 ). However, this kind 
# of syntax will not work outside of the non-linear syntax. Our approach will be
# to make a mean-centered version of rugged_std.


dd <-
    dd %>%
    mutate(rugged_std_c  = rugged_std - mean(rugged_std))

b8.1 <-
    brm(data = dd, 
        family = gaussian,
        log_gdp_std ~ 1 + rugged_std_c,
        prior = c(prior(normal(1, 1), class = Intercept),
                  prior(normal(0, 1), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 8,
        sample_prior = T,
        file = "fits/b08.01")

prior <- prior_draws(b8.1)

set.seed(8)

p1 <-
    prior %>% 
    slice_sample(n = 50) %>% 
    rownames_to_column() %>% 
    expand_grid(rugged_std_c = c(-2, 2)) %>% 
    mutate(log_gdp_std = Intercept + b * rugged_std_c,
           rugged_std  = rugged_std_c + mean(dd$rugged_std)) %>% 
    
    ggplot(aes(x = rugged_std, y = log_gdp_std, group = rowname)) +
    geom_hline(yintercept = range(dd$log_gdp_std), linetype = 2) +
    geom_line(color = palette_pander(n = 2)[2], alpha = .4) +
    geom_abline(intercept = 1.3, slope = -0.6,
                color = palette_pander(n = 2)[1], linewidth = 2) +
    labs(subtitle = "Intercept ~ dnorm(1, 1)\nb ~ dnorm(0, 1)",
         x = "ruggedness",
         y = "log GDP (prop of mean)") +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(0.5, 1.5))

p1

prior %>%
    summarise(a = sum(abs(b) > abs(-0.6)) / nrow(prior))

b8.1b <-
    brm(data = dd, 
        family = gaussian,
        log_gdp_std ~ 1 + rugged_std_c,
        prior = c(prior(normal(1, 0.1), class = Intercept),
                  prior(normal(0, 0.3), class = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 8,
        sample_prior = T,
        file = "fits/b08.01b")

set.seed(8)

p2 <-
    prior_draws(b8.1b) %>% 
    slice_sample(n = 50) %>% 
    rownames_to_column() %>% 
    expand_grid(rugged_std_c = c(-2, 2)) %>% 
    mutate(log_gdp_std = Intercept + b * rugged_std_c,
           rugged_std  = rugged_std_c + mean(dd$rugged_std)) %>% 
    
    ggplot(aes(x = rugged_std, y = log_gdp_std, group = rowname)) +
    geom_hline(yintercept = range(dd$log_gdp_std), linetype = 2) +
    geom_line(color = palette_pander(n = 2)[2], alpha = .4) +
    scale_y_continuous("", breaks = NULL) +
    labs(subtitle = "Intercept ~ dnorm(1, 0.1)\nb ~ dnorm(0, 0.3)",
         x = "ruggedness") +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(0.5, 1.5))

p1 + p2 + 
    plot_annotation(title = "Simulating in search of reasonable priors for the terrain ruggedness example.",
                    theme = theme(plot.title = element_text(size = 12)))

print(b8.1b)

# Adding an indicator variable ----
dd <- 
    dd %>% 
    mutate(cid = if_else(cont_africa == 1, "1", "2"))

dd %>% 
    mutate(cid = str_c("cid: ", cid)) %>% 
    arrange(cid, country) %>% 
    group_by(cid) %>% 
    mutate(rank = 1:n()) %>% 
    
    ggplot(aes(x = cid, y = rank, label = country)) +
    geom_text(size = 2, hjust = 0, family = "Times") +
    scale_y_reverse() +
    theme_void() +
    facet_wrap(~ cid, scales = "free_x")

# Use an index variable with brms ----
# the conventional brms syntax can accommodate an index variable by 
# simply suppressing the default intercept via the 0 + .... syntax. 
# That will be our approach, here.

b8.2 <-
    brm(data = dd, 
        family = gaussian,
        log_gdp_std ~ 0 + cid + rugged_std_c,
        prior = c(prior(normal(1, 0.1), class = b, coef = cid1),
                  prior(normal(1, 0.1), class = b, coef = cid2),
                  prior(normal(0, 0.3), class = b, coef = rugged_std_c),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 8,
        file = "fits/b08.02")

b8.1b <- add_criterion(b8.1b, "waic")
b8.2  <- add_criterion(b8.2, "waic")

loo_compare(b8.1b, b8.2, criterion = "waic") %>% print(simplify = F)

model_weights(b8.1b, b8.2, weights = "waic") %>% round(digits = 3)

print(b8.2)

# Extract posterior draws ----
# use tidybayes::qi() to compute the percentile-based 
# 89% intervals for the difference.
post <- 
    as_draws_df(b8.2) %>% 
    mutate(diff = b_cid1 - b_cid2)

library(tidybayes)

qi(post$diff, .width = .89)
?qi()

# plot the implications of the model ----
nd <- 
    crossing(cid        = 1:2,
             rugged_std = seq(from = -0.2, to = 1.2, length.out = 30)) %>% 
    mutate(rugged_std_c = rugged_std - mean(dd$rugged_std))

f <-
    fitted(b8.2, 
           newdata = nd,
           probs = c(.015, .985)) %>%
    data.frame() %>%
    bind_cols(nd) %>%
    mutate(cont_africa = ifelse(cid == 1, "Africa", "not Africa"))

# what did we do?
head(f)

dd %>%
    mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")) %>%
    
    ggplot(aes(x = rugged_std, fill = cont_africa, color = cont_africa)) +
    geom_smooth(data = f,
                aes(y = Estimate, ymin = Q1.5, ymax = Q98.5),
                stat = "identity",
                alpha = 1/4, linewidth = 1/2) +
    geom_point(aes(y = log_gdp_std),
               size = 2/3) +
    scale_fill_pander() +
    scale_colour_pander() +
    labs(subtitle = "b8.2",
         x = "ruggedness (standardized)",
         y = "log GDP (as proportion of mean)") +
    coord_cartesian(xlim = c(0, 1)) +
    theme(legend.background = element_blank(),
          legend.direction = "horizontal",
          legend.position = c(.67, .93),
          legend.title = element_blank())

fitted(b8.2, 
       newdata = nd,
       summary = F) %>%
    data.frame() %>%
    pivot_longer(everything()) %>% 
    bind_cols(expand_grid(draws = 1:4000, nd)) %>%
    mutate(cont_africa = ifelse(cid == 1, "Africa", "not Africa")) %>% 
    
    ggplot(aes(x = rugged_std, y = value, fill = cont_africa, color = cont_africa)) +
    stat_lineribbon(.width = seq(from = .03, to = .99, by = .03),
                    alpha = .1, size = 0) +
    geom_point(data = dd %>% 
                   mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")),
               aes(y = log_gdp_std),
               size = 2/3) +
    scale_fill_pander() +
    scale_colour_pander() +
    labs(subtitle = "b8.2",
         x = "ruggedness (standardized)",
         y = "log GDP (as proportion of mean)") +
    coord_cartesian(xlim = c(0, 1)) +
    theme(legend.background = element_blank(),
          legend.direction = "horizontal",
          legend.position = c(.67, .93),
          legend.title = element_blank())

# Modeling interactions for both intercept and slope ----
b8.3 <- 
    brm(data = dd, 
        family = gaussian,
        bf(log_gdp_std ~ 0 + a + b * rugged_std_c, 
           a ~ 0 + cid, 
           b ~ 0 + cid,
           nl = TRUE),
        prior = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                  prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                  prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                  prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 8,
        file = "fits/b08.03")

print(b8.3)

b8.1b <- add_criterion(b8.1b, "loo")
b8.2  <- add_criterion(b8.2, "loo")
b8.3  <- add_criterion(b8.3, c("loo", "waic"))

loo_compare(b8.1b, b8.2, b8.3, criterion = "loo") %>% print(simplify = F)

model_weights(b8.1b, b8.2, b8.3, weights = "loo") %>% round(digits = 2)

loo(b8.3) %>% 
    plot()

countries <- c("Equatorial Guinea", "South Africa", "Seychelles", "Swaziland", "Lesotho", "Rwanda", "Burundi", "Luxembourg", "Greece", "Switzerland", "Lebanon", "Yemen", "Tajikistan", "Nepal")

f <-
    fitted(b8.3, 
           # we already defined `nd`, above
           newdata = nd,
           probs = c(.015, .985)) %>%
    data.frame() %>%
    bind_cols(nd) %>%
    mutate(cont_africa = ifelse(cid == 1, "African nations", "Non-African nations"))

dd %>%
    mutate(cont_africa = ifelse(cont_africa == 1, "African nations", "Non-African nations")) %>%
    
    ggplot(aes(x = rugged_std, y = log_gdp_std, fill = cont_africa, color = cont_africa)) +
    geom_smooth(data = f,
                aes(y = Estimate, ymin = Q1.5, ymax = Q98.5),
                stat = "identity",
                alpha = 1/4, linewidth = 1/2) +
    geom_text_repel(data = . %>% filter(country %in% countries),  
                    aes(label = country), 
                    size = 3, seed = 8, 
                    segment.color = "grey25", min.segment.length = 0) +
    geom_point(aes(y = log_gdp_std),
               size = 2/3) +
    scale_fill_pander() +
    scale_colour_pander() +
    labs(x = "ruggedness (standardized)",
         y = "log GDP (as proportion of mean)") +
    coord_cartesian(xlim = c(0, 1)) +
    theme(legend.position = "none") +
    facet_wrap(~ cont_africa)

# Robust regression with student t ----
# fitting the alternative model using the Student-t likelihood for which  
# ν = 2
b8.3t <- 
    brm(data = dd, 
        family = student,
        bf(log_gdp_std ~ 0 + a + b * rugged_std_c, 
           a ~ 0 + cid, 
           b ~ 0 + cid,
           nu = 2,
           nl = TRUE),
        prior = c(prior(normal(1, 0.1), class = b, coef = cid1, nlpar = a),
                  prior(normal(1, 0.1), class = b, coef = cid2, nlpar = a),
                  prior(normal(0, 0.3), class = b, coef = cid1, nlpar = b),
                  prior(normal(0, 0.3), class = b, coef = cid2, nlpar = b),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 8,
        file = "fits/b08.03t")

b8.3t <- add_criterion(b8.3t, c("loo", "waic"))

loo_compare(b8.3, b8.3t, criterion = "loo") %>% print(simplify = F)

tibble(Normal      = b8.3$criteria$loo$diagnostics$pareto_k,
       `Student-t` = b8.3t$criteria$loo$diagnostics$pareto_k) %>% 
    pivot_longer(everything(),
                 values_to = "pareto_k") %>% 
    
    ggplot(aes(x = pareto_k, y = name)) +
    geom_vline(xintercept = .5, linetype = 2, color = palette_pander(n = 5)[5]) +
    stat_dots(slab_fill = palette_pander(n = 4)[4], 
              slab_color = palette_pander(n = 4)[4]) + 
    annotate(geom = "text",
             x = .485, y = 1.5, label = "threshold", angle = 90, 
             family = "Times", color = palette_pander(n = 5)[5]) +
    ylab(NULL) +
    coord_cartesian(ylim = c(1.5, 2.4))

# Compare the alpha and beta parameters for each model 
fixef(b8.3) %>% round(digits = 2)
fixef(b8.3t) %>% round(digits = 2)

# The difference in GDP between Africa and non-Africa dependent on ruggedness ----
fitted(b8.3, 
       newdata = nd,
       summary = F) %>%
    data.frame() %>%
    pivot_longer(everything()) %>% 
    bind_cols(expand_grid(draws = 1:4000, nd)) %>% 
    select(-name) %>% 
    pivot_wider(names_from = cid, values_from = value) %>% 
    mutate(delta = `1` - `2`) %>% 
    
    ggplot(aes(x = rugged_std, y = delta)) +
    stat_lineribbon(.width = .95, fill = palette_pander(n = 8)[8], alpha = 3/4) +
    geom_hline(yintercept = 0, linetype = 2) +
    annotate(geom = "text",
             x = .2, y = 0,
             label = "Africa higher GDP\nAfrica lower GDP",
             family = "Times") +
    labs(x = "ruggedness (standardized)",
         y = "expected difference log GDP") +
    coord_cartesian(xlim = c(0, 1),
                    ylim = c(-0.3, 0.2))

# Tulips data analysis ----
data(tulips, package = "rethinking")
d <- tulips
rm(tulips)

glimpse(d)

d <-
    d %>% 
    mutate(blooms_std = blooms / max(blooms),
           water_cent = water - mean(water),
           shade_cent = shade - mean(shade))

set.seed(8)

# > Prior predictive checks ----
tibble(a = rnorm(1e4, mean = 0.5, sd = 1)) %>% 
    summarise(proportion_outside_of_the_range = sum(a < 0 | a > 1) / n())

set.seed(8)

tibble(a = rnorm(1e4, mean = 0.5, sd = 0.25)) %>% 
    summarise(proportion_outside_of_the_range = sum(a < 0 | a > 1) / n())

range(d$water_cent)

range(d$shade_cent)

b8.4 <-
    brm(data = d, 
        family = gaussian,
        blooms_std ~ 1 + water_cent + shade_cent,
        prior = c(prior(normal(0.5, 0.25), class = Intercept),
                  prior(normal(0, 0.25), class = b, coef = water_cent),
                  prior(normal(0, 0.25), class = b, coef = shade_cent),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 8,
        file = "fits/b08.04")

print(b8.4)

# Include an interaction between water and shade
b8.5 <-
    brm(data = d, 
        family = gaussian,
        blooms_std ~ 1 + water_cent + shade_cent + water_cent:shade_cent,
        prior = c(prior(normal(0.5, 0.25), class = Intercept),
                  prior(normal(0, 0.25), class = b, coef = water_cent),
                  prior(normal(0, 0.25), class = b, coef = shade_cent),
                  prior(normal(0, 0.25), class = b, coef = "water_cent:shade_cent"),
                  prior(exponential(1), class = sigma)),
        iter = 2000, warmup = 1000, chains = 4, cores = 4,
        seed = 8,
        file = "fits/b08.05")

print(b8.5)

# > Posterior predictive plots ----
# loop over values of `water_c` and plot predictions
for (s in -1:1) {
    # s = -1
    # define the subset of the original data
    dt <- d[d$shade_cent == s, ]
    # defining our new data
    nd <- tibble(shade_cent = s, water_cent = c(-1, 1))
    # use our sampling skills, like before
    f <- 
        fitted(b8.4, 
               newdata = nd,
               summary = F) %>%
        data.frame() %>%
        set_names("-1", "1") %>% 
        slice_sample(n = 20) %>% 
        mutate(row = 1:n()) %>% 
        pivot_longer(-row,
                     names_to = "water_cent",
                     values_to = "blooms_std") %>% 
        mutate(water_cent = as.double(water_cent))
    
    # specify our custom plot
    fig <- 
        ggplot(data = dt,
               aes(x = water_cent, y = blooms_std)) +
        geom_line(data = f,
                  aes(group = row),
                  color = palette_pander(n = 6)[6], alpha = 1/5, linewidth = 1/2) +
        geom_point(color = palette_pander(n = 6)[6]) +
        scale_x_continuous("Water (centered)", breaks = c(-1, 0, 1)) +
        labs(title = paste("Shade (centered) =", s),
             y = "Blooms (standardized)") +
        coord_cartesian(xlim = c(-1, 1), 
                        ylim = c(0, 1))
    
    # plot that joint
    plot(fig)
    
}

# augment the data
points <-
    d %>%
    expand_grid(fit = c("b8.4", "b8.5")) %>%
    mutate(x_grid = str_c("shade_cent = ", shade_cent),
           y_grid = fit)

# redefine `nd`
nd <- crossing(shade_cent = -1:1, 
               water_cent = c(-1, 1))

# use `fitted()`
set.seed(8)

rbind(fitted(b8.4, newdata = nd, summary = F, ndraws = 20),
      fitted(b8.5, newdata = nd, summary = F, ndraws = 20)) %>%
    # wrangle
    data.frame() %>%
    set_names(mutate(nd, name = str_c(shade_cent, water_cent, sep = "_")) %>% pull()) %>%
    mutate(row = 1:n(),
           fit = rep(c("b8.4", "b8.5"), each = n() / 2)) %>%
    pivot_longer(-c(row:fit), values_to = "blooms_std") %>%
    separate(name, into = c("shade_cent", "water_cent"), sep = "_") %>%
    mutate(shade_cent = shade_cent %>% as.double(),
           water_cent = water_cent %>% as.double()) %>%
    # these will come in handy for `ggplot2::facet_grid()`
    mutate(x_grid = str_c("shade_cent = ", shade_cent),
           y_grid = fit) %>%
    
    # plot!
    ggplot(aes(x = water_cent, y = blooms_std)) +
    geom_line(aes(group = row),
              color = palette_pander(n = 6)[6], alpha = 1/5, linewidth = 1/2) +
    geom_point(data = points,
               color = palette_pander(n = 6)[6]) +
    scale_x_continuous("Water (centered)", breaks = c(-1, 0, 1)) +
    scale_y_continuous("Blooms (standardized)", breaks = c(0, .5, 1)) +
    ggtitle("Posterior predicted blooms") +
    coord_cartesian(xlim = c(-1, 1),
                    ylim = c(0, 1)) +
    theme(strip.background = element_rect(fill = alpha(palette_pander(n = 2)[2], 1/3))) +
    facet_grid(y_grid ~ x_grid)

# Plot prior predictions ----
# A limitation of the sample_prior = T method is it will not work if 
# you’re trying to use a fitted()-oriented work flow. We have an 
# alternative. Within brm(), set sample_prior = "only". The resulting fit object 
# will be based solely on the priors.

b8.4p <-
    update(b8.4,
           sample_prior = "only",
           iter = 2000, warmup = 1000, chains = 4, cores = 4,
           seed = 8,
           file = "fits/b08.04p")

b8.5p <-
    update(b8.5,
           sample_prior = "only",
           iter = 2000, warmup = 1000, chains = 4, cores = 4,
           seed = 8,
           file = "fits/b08.05p")

set.seed(8)

rbind(fitted(b8.4p, newdata = nd, summary = F, ndraws = 20),
      fitted(b8.5p, newdata = nd, summary = F, ndraws = 20)) %>%
    # wrangle
    data.frame() %>%
    set_names(mutate(nd, name = str_c(shade_cent, water_cent, sep = "_")) %>% pull()) %>%
    mutate(row = rep(1:20, times = 2),
           fit = rep(c("b8.4", "b8.5"), each = n() / 2)) %>%
    pivot_longer(-c(row:fit), values_to = "blooms_std") %>%
    separate(name, into = c("shade_cent", "water_cent"), sep = "_") %>%
    mutate(shade_cent = shade_cent %>% as.double(),
           water_cent = water_cent %>% as.double()) %>%
    # these will come in handy for `ggplot2::facet_grid()`
    mutate(x_grid = str_c("shade_cent = ", shade_cent),
           y_grid = fit) %>%
    
    # plot!
    ggplot(aes(x = water_cent, y = blooms_std, group = row)) +
    geom_hline(yintercept = 0:1, linetype = 2) +
    geom_line(aes(alpha = row == 1, size = row == 1),
              color = palette_pander(n = 6)[6]) +
    scale_size_manual(values = c(1/2, 1)) +
    scale_alpha_manual(values = c(1/3, 1)) +
    scale_x_continuous("Water (centered)", breaks = c(-1, 0, 1)) +
    scale_y_continuous("Blooms (standardized)", breaks = c(0, .5, 1)) +
    ggtitle("Prior predicted blooms") +
    coord_cartesian(xlim = c(-1, 1),
                    ylim = c(-0.5, 1.5)) +
    theme(legend.position = "none",
          strip.background = element_rect(fill = alpha(palette_pander(n = 2)[2], 1/3))) +
    facet_grid(y_grid ~ x_grid)

# Conditional effects ----
b8.1b$formula

conditional_effects(b8.1b)

conditional_effects(b8.1b) %>% 
    plot(points = T)

conditional_effects(b8.1b,
                    spaghetti = T, 
                    ndraws = 200) %>% 
    plot(points = T,
         point_args = c(alpha = 1/2, size = 1),
         line_args = c(colour = "black"))

b8.2$formula
conditional_effects(b8.2)

b8.2$data %>% 
    glimpse()

b8.3$formula
conditional_effects(b8.3)

conditional_effects(b8.3, effects = "rugged_std_c:cid")

conditional_effects(b8.3, effects = "cid:rugged_std_c") %>% 
    plot(cat_args = list(size = 2))

b8.3$data %>% 
    summarize(mean          = mean(rugged_std_c),
              `mean + 1 sd` = mean(rugged_std_c) + sd(rugged_std_c),
              `mean - 1 sd` = mean(rugged_std_c) - sd(rugged_std_c)) %>% 
    mutate_all(round, digits = 2)

b8.5$formula

conditional_effects(b8.5, effects = "water_cent:shade_cent")

p1 <-
    conditional_effects(b8.5, 
                        effects = "water_cent:shade_cent",
                        int_conditions = list(shade_cent = -1:1))

plot(p1,
     points = T,
     plot = F)[[1]] +
    scale_fill_pander() +
    scale_colour_pander() +
    scale_x_continuous(breaks = -1:1) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    facet_wrap(~ shade_cent, labeller = label_both)

# End of chapter exercises ----
# > 8M4 ----
library(rethinking)
library(here)
library(glue)

data("tulips")

tulip_dat <- tulips %>%
    as_tibble() %>%
    mutate(light = -1 * shade,
           blooms_std = blooms / max(blooms),
           water_cent = water - mean(water),
           shade_cent = shade - mean(shade),
           light_cent = light - mean(light))

here()
b8m4 <- brm(blooms_std ~ 1 + water_cent + light_cent + water_cent:light_cent,
            data = tulip_dat, family = gaussian,
            prior = c(prior(normal(0.5, 0.25), class = Intercept),
                      prior(normal(0, 0.25), class = b, lb = 0),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = here("fits", "chp8", "b8m4"))

summary(b8m4)

posterior_interval(b8m4)

new_tulip <- crossing(water_cent = -1:1, 
                      light_cent = -1:1)

points <- tulip_dat %>%
    expand(nesting(water_cent, light_cent, blooms_std)) %>%
    mutate(light_grid = glue("light_cent = {light_cent}"))

to_string <- as_labeller(c(`-1` = "Light = -1", `0` = "Light = 0", 
                           `1` = "Light = 1"))

new_tulip %>% 
    add_epred_draws(b8m4, ndraws = 50) %>% 
    ungroup() %>% 
    ggplot(aes(x = water_cent, y = .epred)) +
    facet_wrap(~light_cent, nrow = 1,
               labeller = to_string) +
    geom_line(aes(group = .draw), alpha = 0.4) +
    geom_point(data = points, aes(y = blooms_std), color = "#009FB7") +
    scale_x_continuous(breaks = -1:1) +
    labs(x = "Water (centered)", y = "Blooms (standardized)")

# Prior predictive simulation
b8m4p <- update(b8m4, sample_prior = "only",
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
                file = here("fits", "chp8", "b8m4p.rds"))


new_tulip %>% 
    add_epred_draws(b8m4p, ndraws = 50) %>% 
    ungroup() %>% 
    ggplot(aes(x = water_cent, y = .epred)) +
    facet_wrap(~light_cent, nrow = 1,
               labeller = to_string) +
    geom_line(aes(group = .draw), alpha = 0.3) +
    geom_hline(yintercept = c(0, 1), linetype = "dashed") +
    scale_x_continuous(breaks = -1:1) +
    labs(x = "Water (centered)", y = "Blooms (standardized)")

# > 8H1 ----
# The bed variable is already a factor variable in the tulips data, so we can just
# add it to the formula in brm(). To use indicator variables instead of a dummy 
# variable, we remove the separate intercept 

b8h1 <- brm(blooms_std ~ 0 + water_cent + light_cent + bed + 
                water_cent:light_cent,
            data = tulip_dat, family = gaussian,
            prior = c(prior(normal(0, 0.25), class = b),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
            file = here("fits", "chp8", "b8h1.rds"))

summary(b8h1)

b8m4 <- add_criterion(b8m4, criterion = "waic")
b8h1 <- add_criterion(b8h1, criterion = "waic")

loo_compare(b8m4, b8h1, criterion = "waic") %>%
    print(simplify = FALSE)

# > 8H3 ----
data("rugged")
rugged_dat <- rugged %>%
    as_tibble() %>%
    select(country, rgdppc_2000, rugged, cont_africa) %>%
    drop_na(rgdppc_2000) %>%
    mutate(log_gdp = log(rgdppc_2000),
           log_gdp_std = log_gdp / mean(log_gdp),
           rugged_std = rugged / max(rugged),
           rugged_std_cent = rugged_std - mean(rugged_std),
           cid = factor(cont_africa, levels = c(1, 0),
                        labels = c("African", "Not African")))

b8h3 <- brm(
    bf(log_gdp_std ~ 0 + a + b * rugged_std_cent,
       a ~ 0 + cid,
       b ~ 0 + cid,
       nl = TRUE),
    data = rugged_dat, family = gaussian,
    prior = c(prior(normal(1, 0.1), class = b, coef = cidAfrican, nlpar = a),
              prior(normal(1, 0.1), class = b, coef = cidNotAfrican, nlpar = a),
              prior(normal(0, 0.3), class = b, coef = cidAfrican, nlpar = b),
              prior(normal(0, 0.3), class = b, coef = cidNotAfrican, nlpar = b),
              prior(exponential(1), class = sigma)),
    iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
    file = here("fits", "chp8", "b8h3.rds")
)

b8h3 <- add_criterion(b8h3, criterion = c("loo", "waic"))

summary(b8h3)

library(gghighlight)

tibble(pareto_k = b8h3$criteria$loo$diagnostics$pareto_k,
       p_waic = b8h3$criteria$waic$pointwise[, "p_waic"]) %>%
    rowid_to_column(var = "obs") %>%
    left_join(rugged_dat %>%
                  select(country) %>%
                  rowid_to_column(var = "obs"),
              by = "obs") %>%
    ggplot(aes(x = pareto_k, y = p_waic)) +
    geom_vline(xintercept = 0.7, linetype = "dashed") +
    geom_hline(yintercept = 0.4, linetype = "dashed") +
    geom_point() +
    gghighlight(pareto_k > 0.7 | p_waic > 0.4, n = 1, label_key = country,
                label_params = list(size = 3)) +
    labs(x = "Pareto *k*", y = "p<sub>WAIC</sub>")

b8h3_t <- brm(
    bf(log_gdp_std ~ 0 + a + b * rugged_std_cent,
       a ~ 0 + cid,
       b ~ 0 + cid,
       nu = 2,
       nl = TRUE),
    data = rugged_dat, family = student,
    prior = c(prior(normal(1, 0.1), class = b, coef = cidAfrican, nlpar = a),
              prior(normal(1, 0.1), class = b, coef = cidNotAfrican, nlpar = a),
              prior(normal(0, 0.3), class = b, coef = cidAfrican, nlpar = b),
              prior(normal(0, 0.3), class = b, coef = cidNotAfrican, nlpar = b),
              prior(exponential(1), class = sigma)),
    iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
    file = here("fits", "chp8", "b8h3_t.rds")
)

b8h3_t <- add_criterion(b8h3_t, criterion = c("loo", "waic"))

n_diff <- spread_draws(b8h3, b_b_cidAfrican, b_b_cidNotAfrican) %>% 
    mutate(diff = b_b_cidAfrican - b_b_cidNotAfrican)

t_diff <- spread_draws(b8h3_t, b_b_cidAfrican, b_b_cidNotAfrican) %>% 
    mutate(diff = b_b_cidAfrican - b_b_cidNotAfrican)

library(ggtext)
ggplot() +
    geom_density(data = n_diff, aes(x = diff, fill = "Normal"),
                 color = NA, alpha = 0.6) +
    geom_density(data = t_diff, aes(x = diff, fill = "Student's *t*"),
                 color = NA, alpha = 0.6) +
    scale_fill_manual(values = c("#009FB7", "#FED766")) +
    labs(x = "African &minus; Non-African", y = "Density", fill = NULL) +
    theme(legend.text = element_markdown())
??element_markdown()

# > 8H4 ----
data(nettle)

nettle <- nettle %>%
    as_tibble() %>%
    mutate(lang_per_cap = num.lang / k.pop,
           log_lang_per_cap = log(lang_per_cap),
           log_area = log(area),
           lang_per_cap_std = standardize(log_lang_per_cap),
           area_std = standardize(log_area),
           mean_growing_std = standardize(mean.growing.season),
           sd_growing_std = standardize(sd.growing.season))

nettle %>% glimpse()

b8h4a_1 <- brm(lang_per_cap_std ~ mean_growing_std,
               data = nettle, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = here("fits", "chp8", "b8h4a_1.rds"))

b8h4a_2 <- brm(lang_per_cap_std ~ mean_growing_std + area_std,
               data = nettle, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = here("fits", "chp8", "b8h4a_2.rds"))

b8h4a_1 <- add_criterion(b8h4a_1, criterion = "loo")
b8h4a_2 <- add_criterion(b8h4a_2, criterion = "loo")

loo_compare(b8h4a_1, b8h4a_2)

new_nettle <- crossing(area_std = seq(-4, 4, by = 2),
                       mean_growing_std = seq(-4, 4, by = 1),
                       sd_growing_std = seq(-4, 4, by = 1))

to_string <- as_labeller(c(`-4` = "Area = -4", `-2` = "Area = -2",
                           `0` = "Area = 0", 
                           `2` = "Area = 2", `4` = "Area = 4"))


palette_wjake <- c("#009FB7", "#FED766", "#272727", "#696773", "#F0F0F0")

make_color_pal <- function(colors, bias = 1) {
    get_color <- colorRamp(colors, bias = bias)
    function(x) rgb(get_color(x), maxColorValue = 255)
}

ramp_blue <- make_color_pal(c("#FFFFFF", "#009FB7"), bias = 1)

ramp_yellow <- make_color_pal(c("#FFFFFF", "#FED766"), bias = 1)

ramp_yelblu <- make_color_pal(c("#FED766", "#FFFFFF", "#009FB7"), bias = 1)

new_nettle %>% 
    add_epred_draws(b8h4a_2, ndraws = 1000) %>% 
    mean_qi(.width = c(0.67, 0.89, 0.97)) %>% 
    ggplot(aes(x = mean_growing_std, y = .epred, ymin = .lower, ymax = .upper)) +
    facet_wrap(~area_std, nrow = 1, labeller = to_string) +
    geom_lineribbon(color = NA) +
    scale_fill_manual(values = ramp_blue(seq(0.9, 0.2, length.out = 3)),
                      breaks = c("0.67", "0.89", "0.97")) +
    labs(x = "Mean Growing Season (standardized)",
         y = "Log Languages per Capita (standardized)",
         fill = "Interval")

# Now evaluate the hypothesis that language diversity is negatively associated 
# with the standard deviation of length of growing season, sd.growing.season. 
# This hypothesis follows from uncertainty in harvest favoring social insurance 
# through larger social networks and therefore fewer languages. Again, consider 
# log(area) as a covariate (not an interaction).

b8h4b_1 <- brm(lang_per_cap_std ~ sd_growing_std,
               data = nettle, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = here("fits", "chp8", "b8h4b_1.rds"))

b8h4b_2 <- brm(lang_per_cap_std ~ sd_growing_std + area_std,
               data = nettle, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.5), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
               file = here("fits", "chp8", "b8h4b_2.rds"))

b8h4b_1 <- add_criterion(b8h4b_1, criterion = "loo")
b8h4b_2 <- add_criterion(b8h4b_2, criterion = "loo")

loo_compare(b8h4b_1, b8h4b_2)

# > Part c
b8h4_c <- brm(lang_per_cap_std ~ mean_growing_std * sd_growing_std,
              data = nettle, family = gaussian,
              prior = c(prior(normal(0, 0.2), class = Intercept),
                        prior(normal(0, 0.5), class = b),
                        prior(exponential(1), class = sigma)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 1234,
              file = here("fits", "chp8", "b8h4_c.rds"))

summary(b8h4_c)

library(patchwork)

new_nettle <- crossing(mean_growing_std = seq(-2, 2, by = 0.5),
                       sd_growing_std = seq(-2, 4, by = 0.5))

int_preds <- new_nettle %>% 
    add_epred_draws(b8h4_c, ndraws = 1000) %>% 
    mean_qi(.width = 0.97)

facet_levels <- seq(-2, 2, by = 2)
sd_facets <- list_along(facet_levels)
for (i in seq_along(sd_facets)) {
    points <- nettle %>% 
        mutate(diff = sd_growing_std - facet_levels[i])
    
    p <- int_preds %>% 
        filter(sd_growing_std == facet_levels[i]) %>% 
        ggplot(aes(x = mean_growing_std, y = .epred, ymin = .lower,
                   ymax = .upper)) +
        geom_lineribbon(fill = "#99D8E2", color = "black") +
        geom_point(data = points,
                   aes(x = mean_growing_std, y = lang_per_cap_std,
                       alpha = -1 * abs(diff)), size = 0.5,
                   inherit.aes = FALSE, show.legend = FALSE) +
        expand_limits(x = c(-2, 2), y = c(-2.5, 3.5)) +
        labs(x = "Mean growing season", y = "Languages",
             subtitle = glue("SD growing season = {facet_levels[i]}")) +
        theme(plot.subtitle = element_text(size = 10))
    
    if (i == 2) {
        p <- p +
            theme(plot.margin = margin(0, 20, 0, 20))
    } else {
        p <- p +
            theme(plot.margin = margin(0, 0, 0, 0))
    }
    
    sd_facets[[i]] <- p
}

mean_facets <- list_along(facet_levels)
for (i in seq_along(mean_facets)) {
    points <- nettle %>% 
        mutate(diff = mean_growing_std - facet_levels[i])
    
    p <- int_preds %>% 
        filter(mean_growing_std == facet_levels[i]) %>% 
        ggplot(aes(x = sd_growing_std, y = .epred, ymin = .lower,
                   ymax = .upper)) +
        geom_lineribbon(fill = "#99D8E2", color = "black") +
        geom_point(data = points,
                   aes(x = sd_growing_std, y = lang_per_cap_std,
                       alpha = -1 * abs(diff)), size = 0.5,
                   inherit.aes = FALSE, show.legend = FALSE) +
        expand_limits(x = c(-2, 2), y = c(-2.5, 3.5)) +
        labs(x = "SD growing season", y = "Languages",
             subtitle = glue("Mean growing season = {facet_levels[i]}")) +
        theme(plot.subtitle = element_text(size = 10))
    
    if (i == 2) {
        p <- p +
            theme(plot.margin = margin(30, 20, 0, 20))
    } else {
        p <- p +
            theme(plot.margin = margin(30, 0, 0, 0))
    }
    
    mean_facets[[i]] <- p
}

sd_patch <- (sd_facets[[1]] | sd_facets[[2]] | sd_facets[[3]])
mean_patch <- (mean_facets[[1]] | mean_facets[[2]] | mean_facets[[3]])

sd_patch / mean_patch