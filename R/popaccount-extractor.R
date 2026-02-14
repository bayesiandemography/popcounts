
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
  age_true <- get_age(popaccount)
  age_time::age_mid(age_true)
}


get_age_open <- function(popaccount) popaccount$control$age_open

get_time_step <- function(popaccount) popaccount$control$time_step


get_age <- function(popaccount) {
  time_step <- get_time_step(popaccount)
  age_open <- get_age_open(popaccount)
  make_age(time_step = time_step,
           age_open = age_open)
}


get_age_repr <- function(popaccount) {
  age <- get_age(popaccount)
  i_repr <- get_i_repr(popaccount)
  age[i_repr]
}

get_age_sex_disp <- function(popaccount) popaccount$sysmod$popn$age_sex_disp



get_asfr_std <- function(popaccount)  popaccount$sysmod$fert$asfr_std


get_alpha_mean <- function(popaccount)  popaccount$sysmod$mort$alpha_mean

get_alpha_disp <- function(popaccount)  popaccount$sysmod$mort$alpha_disp

## assumes that dominant sex always comes first
get_i_repr <- function(popaccount) {
  age_asfr <- get_age_asfr_std(popaccount)
  age <- get_age(popaccount)
  agetime::age_match(age_asfr, age)
}
  


get_lx_std <- function(popaccount)  popaccount$sysmod$mort$lx_std

get_n_draw <- function(popaccount) popaccount$control$n_draw

get_pr_fem <- function(popaccount)  {
  sex_ratio <- popaccount$sysmod$fert$sex_ratio
  100 / (100 + sex_ratio)
}


get_prob_obs <- function(popaccount) popaccount$datamod$prob_obs

get_popn_obs <- function(popaccount) popaccount$popn_obs

get_popn_true <- function(popaccount) popaccount$account$popn_true

get_tfr_mean <- function(popaccount) popaccount$sysmod$fert$tfr_mean

get_tfr_disp <- function(popaccount) popaccount$sysmod$fert$tfr_disp


get_total_init_mean <- function(popaccount) popaccount$sysmod$popn$total_init_mean

get_total_init_disp <- function(popaccount) popaccount$sysmod$popn$total_init_disp


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


