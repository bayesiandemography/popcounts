
## Functions to draw random quantities

draw_alpha <- function(popaccount) {
  alpha_lim <- get_alpha_lim(popaccount)
  n_draw <- get_n_draw(popaccount)
  draw_shifted_beta(lim = alpha_lim,
                    n_draw = n_draw)
}
  

draw_asfr <- function(popaccount) {
  asfr_std <- get_asfr_std(popaccount)
  tfr <- draw_tfr(popaccount)
  agetime_step <- get_agetime_step(popaccount)
  tfr_to_asfr(tfr = tfr,
              asfr_std = asfr_std,
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
  agetime_step <- get_agetime_step(popaccount)
  age_open <- get_age_open(popaccount)
  alpha <- draw_alpha(popaccount)
  lx <- alpha_to_lx(alpha = alpha,
                    lx_std = lx_std)
  if (agetime_step == 1L) {
    Lx <- lx_to_Lx_single(lx)
    Lx <- aggregate_Lx_ages_single(Lx = Lx,
                                   age_open = age_open)
  }
  else {
    Lx <- lx_to_Lx_lt(lx)
    Lx <- aggregate_Lx_ages_lt(Lx = Lx,
                               age_open = age_open)
  }
  Lx
}


draw_shifted_beta <- function(lim, n_draw) {
  beta_param <- get_beta_param()
  l <- lim[[1L]]
  u <- lim[[2L]]
  X <- rvec::rbeta_rvec(n = 1L,
                        shape1 = beta_param[[1L]],
                        shape2 = beta_param[[2L]],
                        n_draw = n_draw)
  X * (u - l) + l
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


draw_tfr <- function(popaccount) {
  tfr_lim <- get_tfr_lim(popaccount)
  n_draw <- get_n_draw(popaccount)
  draw_shifted_beta(lim = tfr_lim,
                    n_draw = n_draw)
}


draw_total_init <- function(popaccount) {
  prob_obs_lim <- get_prob_obs_lim(popaccount)
  total_init_obs <- get_total_init_obs(popaccount)
  n_draw <- get_n_draw(popaccount)
  l <- prob_obs_lim[[1L]]
  u <- prob_obs_lim[[2L]]
  ans <- replicate(n = n_draw,
                   draw_x_given_y(y = total_init_obs,
                                  l = l,
                                  u = u))
  ans <- rvec::rvec_dbl(ans)
  ans
}
