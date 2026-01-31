
## Functions to draw random quantities


draw_account <- function(popaccount,
                         Lx,
                         asfr,
                         age_sex_distn_init,
                         total_init) {
  popn_true <- get_popn_true(popaccount)
  var_popn <- get_var_popn(popaccount)
  n_draw <- get_n_draw(popaccount)
  agetime_step <- get_agetime_step(popaccount)
  times <- get_times(popaccount)
  i_repr <- get_i_repr(popaccount)
  n_time <- length(times)
  n_agesex <- length(Lx)
  n_repr <- length(i_repr)
  n2 <- n_agesex %/% 2L
  is_single_time <- is.null(times) || (n_time == 1L)
  popn_true_init <- rvec::rmultinom_rvec(n = 1L,
                                         size = total_init,
                                         prob = age_sex_distn_init)
  if (is_single_time) {
    popn_true[[var_popn]] <- popn_true_init
    ans <- list(popn_true = popn_true,
                births = NULL,
                deaths = NULL)
    return(ans)
  }
  val_popn_true <- rvec::rvec_new_dbl(length = n_agesex * n_time,
                                      n_draw = n_draw)
  val_births <- rvec::rvec_new_dbl(length = 2L * n_repr * (n_time - 1L),
                                   n_draw = n_draw)
  val_deaths <- rvec::rvec_new_dbl(length = n_agesex * (n_time - 1L),
                                   n_draw = n_draw)
  val_popn_true[seq_len(n_agesex)] <- popn_true_init
  for (i_time in seq.int(from = 2L, to = n_time)) {
    ## existing cohorts
    for (i_agesex_to in seq_len(n_agesex)) {
      if ((i_agesex_to != 1L) && (i_agesex_to != n2 + 1L)) {
        i_popn_to <- (i_time - 1L) * n_agesex + i_agesex_to
        i_popn_from <- (i_time - 2L) * n_agesex + i_agesex_to - 1L
        if ((i_agesex_to != n2) && (i_agesex_to != n)) {
          popn_from <- val_popn_true[[i_popn_from]]
          Sx <- Lx[[i_agesex_to]] / Lx[[i_agesex_to - 1L]]
        }
        else {
          popn_from <- (val_popn_true[[i_popn_from]]
            + val_popn_true[[i_popn_from + 1L]])
          Sx <- Lx[[i_agesex_to]] / (Lx[[i_agesex_to - 1L]] + Lx[[i_agesex_to]])
        }
        popn_to <- rvec::rbinom(n = 1L,
                                size = popn_from,
                                prob = Sx)
        val_popn_true[[i_popn_to]] <- popn_to
        deaths <- popn_from - popn_to
        i_deaths <- (i_time - 2L) * n_agesex + i_agesex_to
        val_deaths[[i_deaths]] <- deaths
      }
    }
    ## newborn cohorts
    for (i_mother in seq_len(n_repr)) {
      births_female_total <- 0L
      births_male_total <- 0L
      i_popn_start <- (i_time - 2L) * n_agesex + i_repr[[i_mother]]
      i_popn_end <- (i_time - 1L) * n_agesex + i_repr[[i_mother]]
      popn_start <- val_popn_true[[i_popn_start]]
      popn_end <- val_popn_true[[i_popn_end]]
      exposure <- 0.5 * agetime_step * (popn_start + popn_end)
      lambda_female <- pr_fem * asfr[[i_mother]] * exposure
      lambda_male <- (1 - pr_fem) * asfr[[i_mother]] * exposure
      births_female <- rvec::rpois_rvec(n = 1L, lambda = lambda_female)
      births_male <- rvec::rpois_rvec(n = 1L, lambda = lambda_male)
      births_female_total <- births_female_total + births_female
      births_male_total <- births_male_total + births_male
      i_births_female <- (i_time - 2L) * 2L * n_repr + i_mother
      i_births_male <- i_births_female + n_repr
      val_births[[i_births_female]] <- births_female
      val_births[[i_births_male]] <- births_male
    }
    Sx_female <- Lx[[1L]] / agetime_step
    Sx_male <- Lx[[n2 + 1L]] / agetime_step
    popn_to_female <- rvec::rbinom_rvec(n = 1L,
                                        prob = Sx_female,
                                        size = births_female)
    popn_to_male <- rvec::rbinom_rvec(n = 1L,
                                      prob = Sx_male,
                                      size = births_male)
    i_popn_to_female <- (i_time - 1L) * n_agesex + 1L
    i_popn_to_male <- (i_time - 1L) * n_agesex + n2 + 1L
    val_popn_true[[i_popn_to_female]] <- popn_to_female
    val_popn_true[[i_popn_to_male]] <- popn_to_male
    deaths_female <- births_female - popn_to_female
    deaths_male <- births_male - popn_to_male
    i_deaths_female <- 
        val_deaths[[i_deaths]] <- deaths



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
  beta_param <- get_beta_param()
  l <- prob_obs_lim[[1L]]
  u <- prob_obs_lim[[2L]]
  a <- beta_param[[1L]]
  b <- beta_param[[2L]]
  ans <- replicate(n = n_draw,
                   draw_x_given_y(y = total_init_obs,
                                  l = l,
                                  u = u,
                                  a = a,
                                  b = b))
  ans <- matrix(ans, nrow = 1L)
  ans <- rvec::rvec_dbl(ans)
  ans
}
