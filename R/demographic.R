
## Deterministic functions doing demographic manipulations


## HAS_TESTS
#' Aggregate Age Groups in 'Lx', Life Table Age Groups
#'
#' Aggregate age groups "0" and "1-4",
#' and age groups from 'age_open' up
#'
#' Values for females are assumed to come
#' before values for males.
#'
#' @param Lx Numeric vector or rvec
#' @param age_open Lower limit of open age group
#'
#' @returns Numeric vector or rvec
#'
#' @noRd
aggregate_Lx_ages_lt <- function(Lx, age_open) {
  n <- length(Lx) %/% 2L
  n_ag <- (age_open %/% 5L) + 1L
  s_ans <- c(1L,
             seq.int(from = 3L, to = n_ag + 1L),
             n + 1L,
             seq.int(from = n + 3L, to = n + n_ag + 1L))
  ans <- Lx[s_ans]
  ans[[1L]] <- ans[[1L]] + Lx[[2L]]
  ans[[n_ag + 1L]] <- ans[[n_ag + 1L]] + Lx[[n + 2L]]
  if (n == n_ag + 1L)
    return(ans)
  s_open_female <- seq.int(from = n_ag + 2L, to = n)
  s_open_male <- seq.int(from = n + n_ag + 2L, to = 2L * n)
  ans[[n_ag]] <- ans[[n_ag]] + sum(Lx[s_open_female])
  ans[[2L * n_ag]] <- ans[[2L * n_ag]] + sum(Lx[s_open_male])
  ans
}


## HAS_TESTS
#' Aggregate Age Groups in 'Lx', Single-Year Age Groups
#'
#' Aggregate age groups from 'age_open' up
#'
#' Values for females are assumed to come
#' before values for males.
#'
#' @param Lx Numeric vector or rvec
#' @param age_open Lower limit of open age group
#'
#' @returns Numeric vector or rvec
#'
#' @noRd
aggregate_Lx_ages_single <- function(Lx, age_open) {
  n <- length(Lx) %/% 2L
  n_ag <- age_open + 1L
  if (n == n_ag)
    return(Lx)
  s_ans <- c(seq.int(from = 1L, to = n_ag),
             seq.int(from = n + 1L, to = n + n_ag))
  ans <- Lx[s_ans]
  s_open_female <- seq.int(from = n_ag + 1L, to = n)
  s_open_male <- seq.int(from = n + n_ag + 1L, to = 2L * n)
  ans[[n_ag]] <- ans[[n_ag]] + sum(Lx[s_open_female])
  ans[[2L * n_ag]] <- ans[[2L * n_ag]] + sum(Lx[s_open_male])
  ans
}


## HAS_TESTS
#' Calculate Life Expectancy at Birth Given Alpha - Life Table Age Groups
#'
#' Life expectancy is for total population.
#'
#' @param alpha Value from Brass logit model.
#' Length-1 numeric vector or rvec.
#' @param lx_std Standard schedule for 'lx'. Numeric vector.
#' @param pr_fem Proportion of births that are
#' female. Numeric scalar.
#'
#' @returns A length-1 vector or rvec.
#'
#' @noRd
alpha_to_e0_lt <- function(alpha, lx_std, pr_fem) {
  lx <- alpha_to_lx(alpha = alpha, lx_std = lx_std)
  Lx <- lx_to_Lx_lt(lx)
  Lx_to_e0(Lx = Lx, pr_fem = pr_fem)
}


## HAS_TESTS
#' Calculate Life Expectancy at Birth Given Alpha - Single-Year Age Groups
#'
#' Life expectancy is for total population.
#'
#' @param alpha Value from Brass logit model.
#' Length-1 numeric vector or rvec.
#' @param lx_std Standard schedule for 'lx'. Numeric vector.
#' @param pr_fem Proportion of births that are
#' female. Numeric scalar.
#'
#' @returns A length-1 vector or rvec.
#'
#' @noRd
alpha_to_e0_single <- function(alpha, lx_std, pr_fem) {
  lx <- alpha_to_lx(alpha = alpha, lx_std = lx_std)
  Lx <- lx_to_Lx_single(lx)
  Lx_to_e0(Lx = Lx, pr_fem = pr_fem)
}


## HAS_TESTS
#' Using Brass Logit Model, Derive Values for 'lx'
#'
#' @param alpha Parameter in Brass logit system.
#' Can be an rvec
#' @param lx_std Standard set of 'lx' values.
#' Cannot be an rvec. Assume that uses age groups
#' 0, 1-4, 5-9, .... Assume ordered by age within sex.
#'
#' @returns A numeric vector or rvec
#'
#' @noRd
alpha_to_lx <- function(alpha, lx_std) {
  lx_std <- lx_std$lx
  lx_std <- lx_std / lx_std[[1L]]
  qx_std <- 1 - lx_std
  logit_qx_std <- poputils::logit(qx_std)
  logit_qx <- alpha + logit_qx_std
  qx <- poputils::invlogit(logit_qx)
  1 - qx
}


#' Calculate Alpha Given Single Value
#' for Life Expectancy at Birth - Life Table Ages
#'
#' Calculate parameter from Brass logit model,
#' given (total) life expectancy,
#' for abridged life table
#'
#' @param e0 A length-1 numeric vector
#' or rvec
#' @param lx_std Numeric vector
#' @param pr_fem Proportion of births that are
#' female. Numeric scalar.
#'
#' @returns A scalar
e0_to_alpha_lt <- function(e0, lx_std, pr_fem) {
  FUN <- function(x)
    e0_to_alpha_lt_inner(e0 = x,
                         lx_std = lx_std,
                         pr_fem = pr_fem)
  if (rvec::is_rvec(e0)) {
    e0 <- as.numeric(e0)
    ans <- vapply(e0, FUN, FUN.VALUE = 1.0, USE.NAMES = FALSE)
    ans <- matrix(ans, nrow = 1L)
    ans <- rvec::rvec(ans)
  }
  else
    ans <- FUN(e0)
  ans
}


#' Calculate Alpha Given Single Value
#' for Life Expectancy at Birth - Single Ages
#'
#' Calculate parameter from Brass logit model,
#' given (total) life expectancy,
#' for complege life table
#'
#' @param e0 A length-1 numeric vector
#' or rvec
#' @param lx_std Numeric vector
#' @param pr_fem Proportion of births that are
#' female. Numeric scalar.
#'
#' @returns A scalar
e0_to_alpha_single <- function(e0, lx_std, pr_fem) {
  FUN <- function(x)
    e0_to_alpha_single_inner(e0 = x,
                             lx_std = lx_std,
                             pr_fem = pr_fem)
  if (rvec::is_rvec(e0)) {
    e0 <- as.numeric(e0)
    ans <- vapply(e0, FUN, FUN.VALUE = 1.0, USE.NAMES = FALSE)
    ans <- matrix(ans, nrow = 1L)
    ans <- rvec::rvec(ans)
  }
  else
    ans <- FUN(e0)
  ans
}


## HAS_TESTS
#' Calculate Alpha Given Single Value
#' for Life Expectancy at Birth - Life Table Age groups
#'
#' Calculate parameter from Brass logit model,
#' given (total) life expectancy
#'
#' @param e0 A scalar
#' @param lx_std Numeric vector
#' @param pr_fem Proportion of births that are
#' female. Numeric scalar.
#'
#' @returns A scalar
e0_to_alpha_lt_inner <- function(e0, lx_std, pr_fem) {
  abs_error <- function(alpha) {
    e0_implied <- alpha_to_e0_lt(alpha = alpha,
                                 lx_std = lx_std,
                                 pr_fem = pr_fem)
    abs(e0_implied - e0)
  }
  val_optim <- stats::optimize(f = abs_error,
                               interval = c(-10, 10))
  val_optim$minimum
}


## HAS_TESTS
#' Calculate Alpha Given Single Value
#' for Life Expectancy at Birth - Single-Year Age groups
#'
#' Calculate parameter from Brass logit model,
#' given (total) life expectancy
#'
#' @param e0 A scalar
#' @param lx_std Numeric vector
#' @param pr_fem Proportion of births that are
#' female. Numeric scalar.
#'
#' @returns A scalar
e0_to_alpha_single_inner <- function(e0, lx_std, pr_fem) {
  abs_error <- function(alpha) {
    e0_implied <- alpha_to_e0_single(alpha = alpha,
                                     lx_std = lx_std,
                                     pr_fem = pr_fem)
    abs(e0_implied - e0)
  }
  val_optim <- stats::optimize(f = abs_error,
                               interval = c(-10, 10))
  val_optim$minimum
}


## HAS_TESTS
#' Calculate 'Lx' from 'lx' for Lifetable Age Groups
#'
#' Assumes ages 0, 1-4, 5-9, ...
#'
#' Assumes values for females given first, then males
#'
#' Uses a0 for high mortality case,
#' from Andreev and Kingkade (2016),
#' as cited on p42 of HMD Methods Protocol, v6.
#' 
#' Open age group 'w' uses l_w / m_w, where
#' m_w = m_{w-1} = d_{w-1} / L_{w-1}.
#' 
#' @param lx Numeric vector or rvec.
#'
#' @returns A numeric vector or rvec
#'
#' @noRd
lx_to_Lx_lt <- function(lx) {
  n <- length(lx)
  if (rvec::is_rvec(lx)) {
    n_draw <- rvec::n_draw(lx)
    Lx <- rvec::new_rvec_dbl(length = n, n_draw = n_draw)
  }
  else
    Lx <- double(length = n)
  n2 <- n %/% 2L
  for (i in seq_len(n)) {
    if (i == 1L) ## 0, female
      Lx[[i]] <- lx[[i + 1L]] + (lx[[i]] - lx[[i + 1L]]) * 0.3141
    else if (i == n2 + 1L) ## 0, male
      Lx[[i]] <- lx[[i + 1L]] + (lx[[i]] - lx[[i + 1L]]) * 0.2991
    else if ((i == 2L) || (i == n2 + 2L)) ## 1-4
      Lx[[i]] <- lx[[i + 1L]] * 4 + (lx[[i]] - lx[[i + 1L]]) * 2
    else if ((i == n2) || (i == n)) ## open age group
      Lx[[i]] <- lx[[i]] * Lx[[i - 1L]] / (lx[[i - 1L]] - lx[[i]])
    else
      Lx[[i]] <- lx[[i + 1L]] * 5 + (lx[[i]] - lx[[i + 1L]]) * 2.5
  }
  Lx
}
  

## HAS_TESTS
#' Calculate 'Lx' from 'lx' for Single-Year Age Groups
#'
#' Assumes values for females given first, then males
#'
#' Uses a0 for high mortality case,
#' from Andreev and Kingkade (2016),
#' as cited on p42 of HMD Methods Protocol, v6.
#' 
#' Open age group 'w' uses l_w / m_w, where
#' m_w = m_{w-1} = d_{w-1} / L_{w-1}.
#' 
#' @param lx Numeric vector or rvec.
#'
#' @returns A numeric vector or rvec
#'
#' @noRd
lx_to_Lx_single <- function(lx) {
  n <- length(lx)
  if (rvec::is_rvec(lx)) {
    n_draw <- rvec::n_draw(lx)
    Lx <- rvec::new_rvec_dbl(length = n, n_draw = n_draw)
  }
  else
    Lx <- double(length = n)
  n2 <- n %/% 2L
  for (i in seq_len(n)) {
    if (i == 1L) ## 0, female
      Lx[[i]] <- lx[[i + 1L]] + (lx[[i]] - lx[[i + 1L]]) * 0.3141
    else if (i == n2 + 1L) ## 0, male
      Lx[[i]] <- lx[[i + 1L]] + (lx[[i]] - lx[[i + 1L]]) * 0.2991
    else if ((i == n2) || (i == n)) ## open age group
      Lx[[i]] <- lx[[i]] * Lx[[i - 1L]] / (lx[[i - 1L]] - lx[[i]])
    else
      Lx[[i]] <- lx[[i + 1L]] + (lx[[i]] - lx[[i + 1L]]) * 0.5
  }
  Lx
}


## HAS_TESTS
#' Given Separate Lx for Females and Males,
#' Calculate Life Expectancy for Total Popn
#'
#' @param Lx Life table function. A numeric
#' vector or rvec.
#' @param pr_fem Proportion of births that
#' are female. A numeric scalar.
#'
#' @returns A length-1 numeric vector
#' or rvec.
#'
#' @noRd
Lx_to_e0 <- function(Lx, pr_fem) {
  n <- length(Lx)
  n2 <- n %/% 2L
  s_female <- seq.int(from = 1L, to = n2)
  s_male <- seq.int(from = n2 + 1L, to = n)
  Lx_female <- Lx[s_female]
  Lx_male <- Lx[s_male]
  e0_female <- sum(Lx_female)
  e0_male <- sum(Lx_male)
  pr_fem * e0_female + (1 - pr_fem) * e0_male
}
  

## HAS_TESTS
#' Calculate Age-Specific Fertility Rates
#' Given the Total Fertility Rate
#'
#' @param tfr Total fertility rate. A length-1
#' numeric vector or rvec.
#' @param asfr_std A standard set of ASFR.
#' Normalised internally. A data frame.
#' @param agetime_step Numeric scalar.
#'
#' @returns A length-1 numeric vector or rvec.
#'
#' @noRd
tfr_to_asfr <- function(tfr, asfr_std, agetime_step) {
  asfr_val <- asfr_std$value
  (tfr * asfr_val) / (agetime_step * sum(asfr_val))
}

