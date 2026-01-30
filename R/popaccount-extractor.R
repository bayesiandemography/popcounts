
## Internal extractor functions

get_age_asfr_std <- function(popaccount) {
  asfr_std <- get_asfr_std(popaccount)
  nms <- names(asfr_std)
  var_age <- poputils::find_var_age(nms)
  asfr_std[[var_age]]
}

get_age_lx_std <- function(popaccount) {
  lx_std <- get_lx_std(popaccount)
  nms <- names(lx_std)
  var_age <- poputils::find_var_age(nms)
  lx_std[[var_age]]
}

get_age_mid <- function(popaccount) {
  age_true <- get_age_true(popaccount)
  agetime::age_mid(age_true)
}


get_age_open <- function(popaccount) popaccount$control$age_open

get_agetime_step <- function(popaccount) popaccount$control$agetime_step


get_age_true <- function(popaccount) {
  agetime_step <- get_agetime_step(popaccount)
  age_open <- get_age_open(popaccount)
  if (agetime_step == 1L)
    agetime::age_labels_one(lower_last = age_open)
  else
    agetime::age_labels_five(lower_last = age_open)
}


get_alpha_lim <- function(popaccount) {
  e0_lim <- get_e0_lim(popaccount)
  lx_std <- get_lx_std(popaccount)
  pr_fem <- get_pr_fem(popaccount)
  agetime_step <- get_agetime_step(popaccount)
  e0_min <- e0_lim[[1L]]
  e0_max <- e0_lim[[2L]]
  if (agetime_step == 1L)
    e0_to_alpha <- e0_to_alpha_single
  else
    e0_to_alpha <- e0_to_alpha_lt
  alpha_min <- e0_to_alpha(e0 = e0_min,
                           lx_std = lx_std,
                           pr_fem = pr_fem)
  alpha_max <- e0_to_alpha(e0 = e0_max,
                           lx_std = lx_std,
                           pr_fem = pr_fem)
  c(alpha_min, alpha_max)
}

get_asfr_std <- function(popaccount)  popaccount$sysmod$fert$asfr_std


get_e0_lim <- function(popaccount)  popaccount$sysmod$mort$e0_lim

## assumes that dominant sex always comes first
get_i_repr <- function(popaccount) {
  age_asfr <- get_age_asfr_std(popaccount)
  age_true <- get_age_true(popaccount)
  agetime::age_match(age_asfr, age_true)
}
  


get_lx_std <- function(popaccount)  popaccount$sysmod$mort$lx_std

get_n_draw <- function(popaccount) popaccount$control$n_draw

get_pr_fem <- function(popaccount)  {
  sex_ratio <- popaccount$sysmod$fert$sex_ratio
  100 / (100 + sex_ratio)
}


get_prob_obs_lim <- function(popaccount) popaccount$datamod$prob_obs_lim

get_popn_obs <- function(popaccount) popaccount$popn_obs

get_popn_true <- function(popaccount) popaccount$account$popn_true

get_tfr_lim <- function(popaccount)  popaccount$sysmod$fert$tfr_lim

get_total_init_obs <- function(popaccount) {
  popn_obs <- get_popn_obs(popaccount)
  var_time <- get_var_time(popaccount)
  var_popn <- get_var_popn(popaccount)
  if (is.null(var_time))
    val_popn <- popn_obs[[var_popn]]
  else {
    val_time <- popn_obs[[var_time]]
    is_init <- val_time == min(val_time)
    popn_obs_init <- popn_obs[is_init, ]
    val_popn <- popn_obs_init[[var_popn]]
  }
  sum(val_popn)
}

get_val_asfr_std <- function(popaccount) {
  asfr_std <- get_asfr_std(popaccount)
  nms <- names(asfr_std)
  var_age <- poputils::find_var_age(nms)
  var_val <- setdiff(nms, var_age)
  asfr_std[[var_val]]
}


get_val_lx_std <- function(popaccount) {
  lx_std <- get_lx_std(popaccount)
  nms <- names(lx_std)
  var_age <- poputils::find_var_age(nms)
  var_val <- setdiff(nms, var_age)
  lx_std[[var_val]]
}



get_var_age <- function(popaccount) popaccount$control$var_age

get_var_popn <- function(popaccount) popaccount$control$var_popn

get_var_sex <- function(popaccount) popaccount$control$var_sex

get_var_time <- function(popaccount) popaccount$control$var_time


