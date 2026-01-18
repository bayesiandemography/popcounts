
## Functions to draw random quantities

draw_alpha <- function(popaccount) {
  lim_alpha <- lim_alpha(popaccount)
  n_draw <- n_draw(popaccount)
  draw_shifted_beta(lim = lim_alpha,
                    n_draw = n_draw)
}
  

draw_asfr <- function(popaccount) {
  lim_tfr <- lim_tfr(popaccount)
  n_draw <- n_draw(popaccount)
  tfr <- draw_shifted_beta(lim = lim_tfr,
                           n_draw = n_draw)
  asfr_std <- asfr_std(popaccount)
  agetime_step <- get_agetime_step(popaccount)
  tfr_to_asfr(tfr = tfr,
              asfr_std = afsr_std,
              agetime_step = agetime_step)
}


#' Randomly Generate Value for 'Lx'
#'
#' Randomly generate values for 'Lx'
#' based ulimately on 'e0_lim' and 'lx_std'.
#'
#' Works with age from groups in 'lx_std',
#' and then aggregates at final step.
#'
#' @param popaccount Object of class "popaccount"
#'
#' @returns An rvec
#'
#' @noRd
draw_Lx <- function(popaccount) {
  lx_std <- get_lx_std(popaccount)
  is_single <- get_is_single(popaccount)
  age_open <- get_age_open(popaccount)
  alpha <- draw_alpha(popaccount)
  lx <- alpha_to_lx(alpha = alpha,
                    lx_std = lx_std)
  if (is_single) {
    Lx <- lx_to_Lx_single(lx)
    Lx <- aggregate_ages_Lx_single(Lx = Lx,
                                   age_open = age_open)
  }
  else {
    Lx <- lx_to_Lx_single_lt(lx)
    Lx <- aggregate_ages_Lx_lt(Lx = Lx,
                               age_open = age_open)
  }
  Lx
}


draw_popn_obs <- function(popaccount,
                          popn_true,
                          prob_obs) {
  stop("not written yet")
}


draw_prob_obs <- function(popaccount,
                          prob_obs) {
  stop("not written yet")
}



draw_total_init <- function(popaccount) {
  total_init_lim <- get_total_init_lim(popaccount)
  n_draw <- get_n_draw(popnaccount)
  draw_shifted_beta(lim = total_init_lim,
                    n_draw = n_draw)
}
