


set_age_open <- function(popaccount,
                         age_open = NULL) {
  check_is_popaccount(popaccount)
  age_obs <- get_age_obs(popaccount)
  time_step <- get_time_step(popaccount)
  age_open <- make_age_open(age_open = age_open,
                            age_obs = age_obs,
                            time_step = time_step)
  popaccount$control$age_open <- age_open
  popaccount
}


set_time_step <- function(popaccount,
                             time_step = NULL) {
  check_is_popaccount(popaccount)
  age_obs <- get_age_obs(popaccount)
  time_obs <- get_time_obs(popaccount)
  time_step <- make_time_step(time_step = time_step,
                                    age_obs = age_obs,
                                    time_obs = time_obs)
  popaccount$control$time_step <- time_step
  popaccount
}

set_control <- function(popaccount,
                        time_step,
                        age_open) {
  popaccount <- set_var_agesextime(popaccount)
  popaccount <- set_time_step(popaccount)
  popaccount <- set_age_open(popaccount)
  popaccount <- set_n_draw(popaccount)
  popaccount
}
  
  

set_n_draw <- function(popaccount, n_draw = 1000) {
  check_is_popaccount(popaccount)
  poputils::check_n(n = n_draw,
                    nm_n = "n_draw",
                    min = 1L,
                    max = NULL,
                    divisible_by = 1L)
  popaccount$control$n_draw <- n_draw
  popaccount
}
  

set_popn_obs <- function(popaccount,
                         popn_obs,
                         times) {
  check_times(times)
  check_popn_obs(popn_obs = popn_obs,
                 times = times)
  popaccount$popn_obs <- popn_obs
  popaccount
}


set_prior_fert <- function(popaccount,
                           tfr_lim = NULL,
                           asfr_std = NULL,
                           sex_ratio = NULL) {
  fert <- popaccount$sysmod$fert
  if (!is.null(tfr_lim))
    fert$tfr_lim <- tfr_lim
  if (!is.null(asfr_std))
    fert$asfr_std <- asfr_std
  if (!is.null(sex_ratio))
    fert$sex_ration <- sex_ratio
  popaccount$sysmod$fert <- fert
  popaccount  
}


set_prior_mort <- function(popaccount,
                           e0_lim = NULL,
                           lx_std = NULL) {
  mort <- popaccount$sysmod$mort
  if (!is.null(e0_lim))
    mort$e0_lim <- e0_lim
  if (!is.null(lx_std))
    mort$lx_std <- lx_std
  popaccount$sysmod$mort <- mort
  popaccount  
}


set_prior_obs <- function(popaccount,
                          pr_obs_lim = c(0.8, 0.95)) {
}





## set_indices <- function(popaccount) {
##   popn_obs_popn_true <- make_index_popn_obs_popn_true(popaccount)
##   prob_obs_popn_true <- make_index_prob_obs_popn_true(popaccount)
##   indices <- list(popn_obs_popn_true = popn_obs_popn_true,
##                   prob_obs_popn_true = prob_obs_popn_true)
##   popaccount$indices <- indices
##   popaccount
## }




set_popn_true <- function(popaccount,
                          time_step = 5,
                          age_open = NULL, ## default to max from popn_obs
                          sex = NULL,
                          time_min = NULL,
                          time_max = NULL) {
  popn_obs <- get_popn_obs(popaccount)
  if (time_step == 5)
    type <- "five"
  else if (time_step == 1)
    type <- "single"
  else
    cli::cli_abort("{.arg time_step} must be {.val {1}} or {.val {5}}.",
                   i = "Value supplied: {.val {time_step}}.")
  age_open <- make_popn_true_age_open(age_open = age_open,
                                    popn_obs = popn_obs)
  sex <- make_popn_true_sex(sex = sex,
                            popn_obs = popn_obs)
  time_min <- make_popn_true_time_min(time_min = time_min,
                                      popn_obs = popn_obs)
  time_max <- make_popn_true_time_max(time_max = time_max,
                                      popn_obs = popn_obs)
  popn_true <- expand.grid(age = age_labels(type = type, min = 0L, max = age_open),
                           sex = sex,
                           time = seq.int(from = time_min, to = time_max, by = time_step),
                           KEEP.OUT.ATTR = FALSE)
  popn_true$count <- NA_real_
  popn_true <- tibble::tibble(popn_true)
  popaccount$popn_true <- popn_true
  popn_account
}                                   
  
                         
                         
                     
