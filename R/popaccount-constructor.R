
popaccount <- function(popn_obs,
                       e0_lim = c(25, 50),
                       tfr_lim = c(3, 6),
                       pr_obs_lim = c(0.8, 0.95),
                       asfr_std = poputils::booth_standard,
                       lx_std = west_level_12,
                       sex_ratio = 105,
                       agetime_step = NULL,
                       age_open = NULL,
                       n_draw = 1000) {
  ## check individual arguments before passing to 'new_popaccount'
  new_popaccount(popn_obs = popn_obs,
                 e0_lim = e0_lim,
                 tfr_lim = tfr_lim,
                 pr_obs_lim = pr_obs_lim,
                 asfr_std = asfr_std,
                 lx_std = lx_std,
                 sex_ratio = sex_ratio,
                 agetime_step = agetime_step,
                 age_open = age_open,
                 n_draw = n_draw)
}


new_popaccount <- function(popn_obs,
                           e0_lim,
                           tfr_lim,
                           pr_obs_lim,
                           asfr_std,
                           lx_std,
                           sex_ratio,
                           agetime_step,
                           age_open) {
  control <- make_control(popn_obs = popn_obs,
                          agetime_step = agetime_step,
                          age_open = age_open)
  sysmod <- make_sysmod()
  datamod <- make_datamod()
  ans <- list(popn_obs = popn_obs,
              control = control,
              sysmod = sysmod,
              datamod = datamod)
  class(ans) <- "popaccount"
  ans
}


set_indices <- function(popaccount) {
  popn_obs_popn_true <- make_index_popn_obs_popn_true(popaccount)
  prob_obs_popn_true <- make_index_prob_obs_popn_true(popaccount)
  indices <- list(popn_obs_popn_true = popn_obs_popn_true,
                  prob_obs_popn_true = prob_obs_popn_true)
  popaccount$indices <- indices
  popaccount
}




set_popn_true <- function(popaccount,
                          agetime_step = 5,
                          age_open = NULL, ## default to max from popn_obs
                          sex = NULL,
                          time_min = NULL,
                          time_max = NULL) {
  popn_obs <- get_popn_obs(popaccount)
  if (agetime_step == 5)
    type <- "five"
  else if (agetime_step == 1)
    type <- "single"
  else
    cli::cli_abort("{.arg agetime_step} must be {.val {1}} or {.val {5}}.",
                   i = "Value supplied: {.val {agetime_step}}.")
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
                           time = seq.int(from = time_min, to = time_max, by = agetime_step),
                           KEEP.OUT.ATTR = FALSE)
  popn_true$count <- NA_real_
  popn_true <- tibble::tibble(popn_true)
  popaccount$popn_true <- popn_true
  popn_account
}                                   
  

set_datamod <- function(popaccount,
                        pr_f04_lim = c(0.9, 1),
                        pr_m04_lim = c(0.9, 1),
                        pr_f5_lim = c(0.9, 1),
                        pr_m5_lim = c(0.9, 1)) {
  datamod <- list(pr_child_lim = pr_child_lim,
                  pr_female_lim = pr_female_lim,
                  pr_male_lim = pr_male_lim)
  popaccount$datamod <- datamod
  popaccount
}
                         
                         
                     
