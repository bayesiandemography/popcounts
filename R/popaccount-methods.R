
## 'fit' ----------------------------------------------------------------------

#' @importFrom generics fit
#' @export
generics::fit


#' @export
fit.popaccount <- function(object, popn_obs, ...) {
  var_age <- make_var_age(popn_obs)
  var_sex <- make_var_sex(popn_obs)
  var_time <- make_var_time(popn_obs)
  var_popn <- make_var_popn(popn_obs)
  check_time_step_popn_obs(time_step = time_step,
                              popn_obs = popn_obs,
                              var_age = var_age,
                              var_time = var_time)
  check_age_open_popn_obs(age_open = age_open,
                          popn_obs = popn_obs,
                          var_age)
  stop("not written yet")
}


  
## 'generate' -----------------------------------------------------------------

#' @importFrom generics generate
#' @export
generics::generate

#' @export
generate.popaccount <- function(x,
                                n_time = NULL,
                                prob_age_report = NULL,
                                ...) {
  prob_obs <- get_prob_obs(x)
  if (!is.null(n_time)) {
    poputils::check_n(n = n_time,
                      nm_n = "n_time",
                      min = 1L,
                      max = NULL,
                      divisible_by = 1L)
    n_time <- as.integer(n_time)
  }
  Lx <- draw_Lx(x)
  asfr <- draw_asfr(x)
  age_sex_distn_init <- make_age_sex_distn_init(popaccount = x,
                                                Lx = Lx,
                                                asfr = asfr)
  total_init <- draw_total_init(x)
  account <- draw_account(popaccount = x,
                          n_time = n_time,
                          Lx = Lx,
                          asfr = asfr,
                          age_sex_distn_init = age_sex_distn_init,
                          total_init = total_init)
  popn_true <- account$popn
  births_true <- account$births
  deaths_true <- account$deaths
  ans <- tibble::tribble(
    ~component,            ~value,
    "Lx",                  Lx,
    "asfr",                asfr,
    "age_sex_distn_init",  age_sex_distn_init,
    "total_init",          total_init,
    "popn_true",           popn_true,
    "births_true",         births_true,
    "deaths_true",         deaths_true
  )
  ans_obs <- draw_obs(popn_true = popn_true,
                      prob_obs = prob_obs,
                      prob_age_report = prob_age_report)
  if (!is.null(ans_obs))
    ans <- vctrs::vec_rbind(ans, ans_obs)
  ans
}
  



  


