
## 'components' ------------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components


components.popaccount <- function(object, quiet = TRUE, ...) {
  check_has_no_dots(...)
  if (is_fitted(object))
    components_fitted(object)
  else
    cli::cli_abort(c("{.arg object} has not been fitted.",
                     i = "Call {.fun fit} before calling {.fun components}."))
  stop("not written yet")  
}


#' @importFrom generics generate
#' @export
generics::generate


generate.popaccount <- function(x, total_init_obs, ...) {
  



  



components_unfitted <- function(popaccount, ...) {
  Lx <- draw_Lx(popaccount)
  asfr <- draw_asfr(popaccount)
  age_sex_distn_init <- make_age_sex_distn_init(popaccount = popaccount,
                                                Lx = Lx,
                                                asfr = asfr)
  total_init <- draw_total_init(popaccount)
  account <- make_account(Lx = Lx,
                          asfr = asfr,
                          age_sex_distn_init = age_sex_distn_init,
                          total_init = total_init)
  popn_true <- account$popn
  births <- account$births
  deaths <- account$deaths
  prob_obs <- make_prob_obs(popaccount)
  popn_obs <- draw_popn_obs(popn_true = popn_true,
                            prob_obs = prob_obs)
  tibble::tribble(
    ~component,            ~value,
    "Lx",                  Lx,
    "asfr",                asfr,
    "age_sex_distn_init",  age_sex_distn_init,
    "total_init",          total_init,
    "popn_true",           popn_true,
    "births",              births,
    "deaths",              deaths,
    "prob_obs",            prob_obs,
    "popn_obs",            popn_obs
  )
}
