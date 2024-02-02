library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

check_cmdstan_toolchain()
cmdstanr::check_cmdstan_toolchain(fix = TRUE) 

cmdstanr::install_cmdstan()

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")

install.packages(c("survival","pROC","caret","ranger","gbm","depmixS4", "lubridate"))
