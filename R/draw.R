
## Functions to draw random quantities


draw_account <- function(popaccount,
                         n_time,
                         Lx,
                         asfr,
                         age_sex_distn_init,
                         total_init) {
  popn_true <- get_popn_true(popaccount)
  time_step <- get_time_step(popaccount)
  n_draw <- get_n_draw(popaccount)
  age <- get_age(popaccount)
  age_repr <- get_age_repr(popaccount)
  sex <- get_levels_sex()
  if (is.null(n_time))
    popn_true <- expand.grid(age = age,
                             sex = sex,
                             KEEP.OUT.ATTRS = FALSE)
  else {
    time <- seq.int(from = 0L,
                    by = time_step,
                    length.out = n_time)
    popn_true <- expand.grid(age = age,
                             sex = sex,
                             time = time,
                             KEEP.OUT.ATTRS = FALSE)
  }
  popn_true_init <- rvec::rmultinom_rvec(n = 1L,
                                         size = total_init,
                                         prob = age_sex_distn_init)
  is_single_time <- is.null(n_time) || (n_time == 1L)
  if (is_single_time) {
    popn_true$count <- popn_true_init
    ans <- list(popn_true = popn_true,
                births = NULL,
                deaths = NULL)
  }
  else {
    is_age_repr <- popn_true$age %in% age_repr
    is_sex <- names(popn_true) == "sex"
    is_time_last <- popn_true$time == max(popn_true$time)
    births <- popn_true[is_age_repr & !is_time_last, !is_sex, drop = FALSE]
    deaths <- popn_true[!is_time_last, , drop = FALSE]
    ans <- draw_account_multi(popaccount = popaccount,
                              popn_true = popn_true,
                              births = births,
                              deaths = deaths,
                              popn_true_init = popn_true_init,
                              Lx = Lx,
                              asfr = asfr,
                              n_time = n_time)
  }
  ans
}

draw_account_multi <- function(popaccount,
                               popn_true,
                               births,
                               deaths,
                               popn_true_init,
                               Lx,
                               asfr,
                               n_time) {
  time_step <- get_time_step(popaccount)
  pr_fem <- get_pr_fem(popaccount)
  i_repr <- get_i_repr(popaccount)
  n_agesex <- length(Lx)
  n_repr <- length(i_repr)
  n2 <- n_agesex %/% 2L
  val_popn <- rvec::new_rvec_dbl(length = nrow(popn_true))
  val_births <- rvec::new_rvec_dbl(length = nrow(births))
  val_deaths <- rvec::new_rvec_dbl(length = nrow(deaths))
  Sx <- Lx_to_Sx(Lx = Lx, time_step = time_step)
  i_init <- seq_len(n_agesex)
  val_popn[i_init] <- popn_true_init
  for (i_time in seq.int(from = 2L, to = n_time)) {
    ## existing cohorts
    for (i_agesex in seq_len(n_agesex)) {
      is_existing_cohort <- !(i_agesex %in% c(1L, n2 + 1L))
      if (is_existing_cohort) {
        i_popn_from <- (i_time - 2L) * n_agesex + i_agesex - 1L
        i_popn_to <- i_popn_from + n_agesex + 1L
        popn_from <- val_popn[[i_popn_from]]
        is_oldest <- i_agesex %in% c(n2, n_agesex)
        if (is_oldest)
          popn_from <- popn_from + val_popn[[i_popn_from + 1L]]
        Sx_agesex <- Sx[[i_agesex]]
        popn_to <- rvec::rbinom_rvec(n = 1L,
                                     size = popn_from,
                                     prob = Sx_agesex)
        val_popn[[i_popn_to]] <- popn_to
        deaths_agesex <- popn_from - popn_to
        i_deaths <- i_popn_to - n_agesex
        val_deaths[[i_deaths]] <- deaths_agesex
      }
    }
    ## newborn cohorts
    births_female_total <- 0L
    births_male_total <- 0L
    for (i_mother in seq_len(n_repr)) {
      i_popn_start <- (i_time - 2L) * n_agesex + i_repr[[i_mother]]
      i_popn_end <- (i_time - 1L) * n_agesex + i_repr[[i_mother]]
      popn_start <- val_popn[[i_popn_start]]
      popn_end <- val_popn[[i_popn_end]]
      exposure <- 0.5 * time_step * (popn_start + popn_end)
      lambda_female <- pr_fem * asfr[[i_mother]] * exposure
      lambda_male <- (1 - pr_fem) * asfr[[i_mother]] * exposure
      births_female <- rvec::rpois_rvec(n = 1L,
                                        lambda = lambda_female)
      births_male <- rvec::rpois_rvec(n = 1L,
                                      lambda = lambda_male)
      i_births_female <- (i_time - 2L) * 2L * n_repr + i_mother
      i_births_male <- i_births_female + n_repr
      val_births[[i_births_female]] <- births_female
      val_births[[i_births_male]] <- births_male
      births_female_total <- births_female_total + births_female
      births_male_total <- births_male_total + births_male
    }
    Sx_female <- Sx[[1L]]
    Sx_male <- Sx[[n2 + 1L]]
    popn_to_female <- rvec::rbinom_rvec(n = 1L,
                                        prob = Sx_female,
                                        size = births_female_total)
    popn_to_male <- rvec::rbinom_rvec(n = 1L,
                                      prob = Sx_male,
                                      size = births_male_total)
    i_popn_to_female <- (i_time - 1L) * n_agesex + 1L
    i_popn_to_male <- (i_time - 1L) * n_agesex + n2 + 1L
    val_popn[[i_popn_to_female]] <- popn_to_female
    val_popn[[i_popn_to_male]] <- popn_to_male
    deaths_female <- births_female_total - popn_to_female
    deaths_male <- births_male_total - popn_to_male
    i_deaths_female <- (i_time - 2L) * n_agesex + 1L
    i_deaths_male <- (i_time - 2L) * n_agesex + n2 + 1L
    val_deaths[[i_deaths_female]] <- deaths_female
    val_deaths[[i_deaths_male]] <- deaths_male
  }
  popn_true$count <- val_popn
  births$count <- val_births
  deaths$count <- val_deaths
  list(popn_true = popn_true,
       births = births,
       deaths = deaths)
}




## TODO - NEED TO ENSURE THAT FEMALES COME FIRST IN Lx, Population
draw_age_sex_distn_init <- function(popaccount,
                                    Lx,
                                    asfr) {
  pr_fem <- get_pr_fem(popaccount)
  i_repr <- get_i_repr(popaccount)
  age_mid <- get_age_mid(popaccount)
  age_sex_disp <- get_age_sex_disp(popaccount)
  mx <- pr_fem * asfr
  Lx_repr <- Lx[i_repr]
  age_mid_repr <- age_mid[i_repr]
  r <- poputils::.intrinsic_growth_rate(mx = mx,
                                        Lx = Lx_repr,
                                        age_mid = age_mid_repr)
  n2 <- length(Lx) %/% 2L
  pr <- rep(c(pr_fem, 1 - pr_fem), each = n2)
  age_mid <- rep(age_mid, times = 2L)
  ans <- exp(-r * age_mid) * pr * Lx
  ans <- ans / sum(ans)
  if (age_sex_disp > 0) {
    shape <- disp + ans
    ans <- rvec::rgamma_rvec(n = 1L,
                             shape = shape,
                             rate = 1)
    ans <- ans / sum(ans)
  }
  ans
}




draw_alpha <- function(popaccount) {
  mean <- get_alpha_mean(popaccount)
  disp <- get_alpha_disp(popaccount)
  n_draw <- get_n_draw(popaccount)
  rvec::rgamma_rvec(n = 1L,
                    shape = 1 / disp,
                    scale = disp * mean,
                    n_draw = n_draw)
}
  

draw_asfr <- function(popaccount) {
  asfr_std <- get_asfr_std(popaccount)
  tfr <- draw_tfr(popaccount)
  time_step <- get_time_step(popaccount)
  tfr_to_asfr(tfr = tfr,
              asfr_std = asfr_std,
              time_step = time_step)
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
  time_step <- get_time_step(popaccount)
  age_open <- get_age_open(popaccount)
  alpha <- draw_alpha(popaccount)
  lx <- alpha_to_lx(alpha = alpha,
                    lx_std = lx_std)
  if (time_step == 1L) {
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


draw_obs <- function(popn_true,
                     prob_obs,
                     prob_age_report) {
  has_prob_obs <- !is.null(prob_obs)
  has_prob_age_report <- !is.null(prob_age_report)
  if (has_prob_age_report)
    check_prob_age_report(prob_age_report)
  if (has_prob_obs && has_prob_age_report) {
    val_prob_obs <- draw_val_prob_obs(prob_obs)
    popn_obs <- draw_popn_obs(popn_true = popn_true,
                              val_prob_obs = val_prob_obs)
    popn_obs <- draw_popn_obs_age(popn = popn_obs,
                                  prob_report_age = prob_report_age)
    tibble::tribble(
      ~component,            ~value,
      "val_prob_obs",        val_prob_obs,
      "val_prob_age_report", val_prob_age_report,
      "popn_obs",            popn_obs
    )
  }
  else if (has_prob_obs && !has_prob_age_report) {
    val_prob_obs <- draw_val_prob_obs(prob_obs)
    popn_obs <- draw_popn_obs(popn_true = popn_true,
                              val_prob_obs = val_prob_obs)
    tibble::tribble(
      ~component,     ~value,
      "val_prob_obs", val_prob_obs,
      "popn_obs",     popn_obs
    )
  }
  else if (!has_prob_obs && has_prob_age_report) {
    popn_obs <- draw_popn_obs_age(popn = popn_obs,
                                  prob_report_age = prob_report_age)
    tibble::tribble(
      ~component,            ~value,
      "val_prob_age_report", val_prob_age_report,
      "popn_obs",            popn_obs
    )
  }
  else
    NULL
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
  mean <- get_tfr_mean(popaccount)
  disp <- get_tfr_disp(popaccount)
  n_draw <- get_n_draw(popaccount)
  rvec::rgamma_rvec(n = 1L,
                    shape = 1 / disp,
                    scale = disp * mean,
                    n_draw = n_draw)
}


draw_total_init <- function(popaccount) {
  mean <- get_total_init_mean(popaccount)
  disp <- get_total_init_disp(popaccount)
  n_draw <- get_n_draw(popaccount)
  size <- 1 / disp
  prob <- 1 / (1 + disp * mean)
  rvec::rnbinom_rvec(n = 1L,
                     size = size,
                     prob = prob = prob,
                     n_draw = n_draw)
}
