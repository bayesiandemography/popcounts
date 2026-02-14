
popaccount <- function(time_step = 5,
                       age_open = 80,
                       prob_obs = NULL) {
  check_time_step(time_step)
  time_step <- as.integer(time_step)
  check_age_open(age_open = age_open,
                 time_step = time_step)
  age_open <- as.integer(age_open)
  age <- make_age(time_step = time_step,
                  age_open = age_open)
  sex <- get_level_sex()
  check_prob_obs(prob_obs = prob_obs,
                 age = age,
                 sex = sex)
  asfr_std <- make_asfr_std(time_step)
  lx_std <- make_lx_std(time_step)
  sysmod <- list(fert = list(tfr_mean = 5,
                             tfr_disp = 0.1,
                             asfr_std = asfr_std,
                             sex_ratio = 105),
                 mort = list(alpha_mean = 0,
                             alpha_disp = 0.1,
                             lx_std = lx_std),
                 popn = list(total_init_mean = 1e5,
                             total_init_disp = 0.1,
                             age_sex_disp = 0))
  datamod <- list(prob_obs = prob_obs)
  control <- list(time_step = time_step,
                  age_open = age_open,
                  n_draw = 1000L)
  ans <- list(sysmod = sysmod,
              datamod = datamod,
              control = control)
  class(ans) <- "popaccount"
  ans
}
  
  


  
