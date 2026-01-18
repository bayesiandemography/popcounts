
## Internal extractor functions


get_agetime_step <- function(popaccount) popaccount$sysmod$agetime_step

get_alpha_lim <- function(popaccount) {
  e0_lim <- get_e0_lim(popaccount)
  lx_std <- get_lx_std(popaccount)
  pr_fem <- get_pr_fem(popaccount)
  e0_min <- e0_lim[[1L]]
  e0_max <- e0_lim[[2L]]
  alpha_min <- e0_to_alpha(e0 = e0_min,
                           lx_std = lx_std,
                           pr_fem = pr_fem)
  alpha_max <- e0_to_alpha(e0 = e0_max,
                           lx_std = lx_std,
                           pr_fem = pr_fem)
  c(alpha_min, alpha_max)
}


get_e0_lim <- function(popaccount)  popaccount$sysmod$e0_lim

get_lx_std <- function(popaccount)  popaccount$sysmod$lx_std

get_n_draw <- function(popaccount) popaccount$control$n_draw

get_pr_fem <- function(popaccount)  {
  sex_ratio <- popaccount$sysmod$sex_ratio
  100 / (100 + sex_ratio)
}



get_popn_obs <- function(popaccount) popaccount$popn_obs

get_total_init_lim <- function(popaccount) popaccount$sysmod$total_init_lim


get_var_age <- function(popaccount) popaccount$control$var_age

get_var_sex <- function(popaccount) popaccount$control$var_sex

get_var_time <- function(popaccount) popaccount$control$var_time


