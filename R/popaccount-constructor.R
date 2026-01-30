
popaccount <- function(popn_obs,
                       agetime_step = NULL,
                       age_open = NULL) {
  check_popn_obs(popn_obs)
  if (!is.null(agetime_step))
    poputils::check_n(n = agetime_step,
                      nm_n = "agetime_step",
                      min = 1L,
                      max = NULL,
                      divisible_by = 1L)
  if (!is.null(age_open))
    poputils::check_n(n = age_open,
                      nm_n = "age_open",
                      min = 0L,
                      max = NULL,
                      divisible_by = 1L)
  var_age <- make_var_age(popn_obs)
  var_sex <- make_var_sex(popn_obs)
  var_time <- make_var_time(popn_obs)
  var_popn <- make_var_popn(popn_obs)
  agetime_step <- make_agetime_step(agetime_step = agetime_step,
                                    age_open = age_open,
                                    popn_obs = popn_obs,
                                    var_age = var_age,
                                    var_time = var_time)
  age_open <- make_age_open(age_open = age_open,
                            agetime_step = agetime_step,
                            popn_obs = popn_obs,
                            var_age)
  asfr_std <- make_asfr_std(agetime_step)
  lx_std <- make_lx_std(agetime_step)
  times <- make_times(agetime_step = agetime_step,
                      popn_obs = popn_obs,
                      var_time = var_time)
  popn_true <- make_popn_true(var_age = var_age,
                              var_sex = var_sex,
                              var_time = var_time,
                              var_popn = var_popn,
                              agetime_step = agetime_step,
                              age_open = age_open,
                              times = times)
  ans <- new_popaccount()
  ans$popn_obs <- popn_obs
  ans$sysmod <- list(fert = list(tfr_lim = c(3, 6),
                                 asfr_std = asfr_std,
                                 sex_ratio = 105),
                     mort = list(e0_lim = c(25, 50),
                                 lx_std = lx_std))
  ans$datamod <- list(prob_obs_lim = c(0.8, 0.95)) ## TODO - COMPLETE
  ans$account$popn_true <- popn_true
  ans$control <- list(var_age = var_age,
                      var_sex = var_sex,
                      var_time = var_time,
                      var_popn = var_popn,
                      agetime_step = agetime_step,
                      age_open = age_open,
                      times = times,
                      n_draw = 1000L,
                      is_fitted = FALSE)
  ans
}
  
  

new_popaccount <- function() {
  ans <- list(popn_obs = NULL,
              sysmod = list(fert = list(tfr_lim = NULL,
                                        asfr_std = NULL,
                                        sex_ratio = NULL),
                            mort = list(e0_lim = NULL,
                                        lx_std = NULL)),
              datamod = list(prob_obs_lim = NULL),
              account = list(popn_true = NULL,
                             births = NULL,
                             deaths = NULL),
              control = list(var_age = NULL,
                             var_sex = NULL,
                             var_time = NULL,
                             var_popn = NULL,
                             agetime_step = NULL,
                             age_open = NULL,
                             times = NULL,
                             n_draw = NULL))
  class(ans) <- "popaccount"
  ans
}


  
