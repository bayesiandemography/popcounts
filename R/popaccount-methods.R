
## 'components' ------------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components


components.popaccount <- function(object, ...) {
  if (!is_fitted(object))
    cli::cli_abort(c("{.arg object} has not been fitted.",
                     i = "Call {.fun fit} before calling {.fun components}."))
  stop("not written yet")  
}


#' @importFrom generics generate
#' @export
generics::generate


generate <- function(x, ...) {
  Lx <- draw_Lx(x)
  asfr <- draw_asfr(x)
  age_sex_distn_init <- make_age_sex_distn_init(popaccount = popaccount,
                                                Lx = Lx,
                                                asfr = asfr)
  total_init <- draw_total_init(x)
  account <- make_account(Lx = Lx,
                          asfr = asfr,
                          total_init = total_init,
                          age_sex_distn_init = age_sex_distn_init)
  popn_true <- account$popn
  births <- account$births
  deaths <- account$deaths
  prob_obs <- make_prob_obs(x)
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


  



  


