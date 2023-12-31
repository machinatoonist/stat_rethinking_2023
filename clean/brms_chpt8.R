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
