
## If 'make_agetime_step' has been run already,
## and user supplied value for 'age_open',
## then that value guaranteed compatible with
## 'agetime_step'.
make_age_open <- function(age_open,
                          agetime_step,
                          popn_obs,
                          var_age) {
  user_supplied_value <- !is.null(age_open)
  val_age <- popn_obs[[var_age]]
  lower <- agetime::age_lower(val_age)
  upper <- agetime::age_upper(val_age)
  is_open <- is.infinite(upper)
  if (user_supplied_value) {
    is_lt_obs <- !is.na(lower) & age_open < lower
    if (any(is_lt_obs)) {
      age_below <- unique(val_age[is_lt_obs])
      n_below <- length(age_below)
      cli::cli_abort(c(paste("Value for {.arg age_open} less than lower limit",
                             "of age {cli::qty(n_below)} group{?s} in",
                             "{.var {var_age}} variable in {.arg popn_obs}."),
                       i = "{.arg age_open}: {.val {age_open}}.",
                       i = "Age group{?s}: {.val {age_below}}."))
    }
    ans <- age_open
  }
  else {
    max_lower_upper <- max(c(lower, upper[is.finite(upper)]), na.rm = TRUE)
    if (max_lower_upper %% agetime_step == 0L)
      ans <- max_lower_upper
    else
      ans <- ((max_lower_upper %/% agetime_step) + 1L) * agetime_step
  }
  ans <- as.integer(ans)
  ans
}


## TODO - NEED TO ENSURE THAT FEMALES COME FIRST IN Lx, Population
make_age_sex_distn_init <- function(popaccount,
                                    Lx,
                                    asfr) {
  pr_fem <- get_pr_fem(popaccount)
  i_repr <- get_i_repr(popaccount)
  age_mid <- get_age_mid(popaccount)
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
  ans
}


      
#' Make 'agetime_step'
#'
#' Includes check that value supplied
#' for 'agetime_step' is valid.
#' 
#' @param agetime_step Value provided by user or NULL
#' @param age_obs Age variable from 'popn_obs'
#' @param time_obs Time variable from 'popn_obs' or NULL
#'
#' @param returns 1L or 5L
#'
#' @noRd
make_agetime_step <- function(agetime_step,
                              age_open,
                              popn_obs,
                              var_age,
                              var_time) {
  valid_values <- c(1L, 5L)
  user_supplied_value <- !is.null(agetime_step)
  if (user_supplied_value) {
    if (!(agetime_step %in% c(1L, 5L)))
      cli::cli_abort(c("{.arg agetime_step} must be {.val {1}} or {.val {5}}.",
                       "Value supplied: {.val {agetime_step}}."))
  }
  val_age <- popn_obs[[var_age]]
  lower <- agetime::age_lower(val_age)
  upper <- agetime::age_upper(val_age)
  age_uses_5 <- all(lower[!is.na(lower)] %% 5L == 0L) &&
    all(upper[is.finite(upper)] %% 5L == 0L)
  if (is.null(var_time))
    time_uses_5 <- TRUE
  else {
    val_time <- popn_obs[[var_time]]
    time_uses_5 <- all(diff(val_time) %% 5L == 0L)
  }
  age_open_uses_5 <- is.null(age_open) || (age_open %% 5L == 0L)
  if (user_supplied_value) {
    if (agetime_step == 5L) {
      if (!age_uses_5)
        cli::cli_abort(paste("Value for {.arg agetime_step} ({.val {5}})",
                             "not compatible with {.var {var_age}} variable",
                             "in {.arg popn_obs}."))
      if (!time_uses_5)
        cli::cli_abort(paste("Value for {.arg agetime_step} ({.val {5}})",
                             "not compatible with {.var {var_time}} variable",
                             "in {.arg popn_obs}."))
      if (!age_open_uses_5)
        cli::cli_abort(paste("Value for {.arg agetime_step} ({.val {5}})",
                             "not compatible with {.var {var_time}} variable",
                             "in {.arg popn_obs}."))
      ans <- 5L
    }
    else
      ans <- 1L
  }
  else {
    if (age_uses_5 && time_uses_5 && age_open_uses_5)
      ans <- 5L
    else
      ans <- 1L
  }
  ans
}


make_asfr_std <- function(agetime_step) {
  if (agetime_step == 1L)
    booth_standard_1
  else
    booth_standard_5
}

make_lx_std <- function(agetime_step) {
  if (agetime_step == 1L)
    lx_west_7_complete
  else
    lx_west_7_abridged
}

make_popn_true <- function(var_age,
                           var_sex,
                           var_time,
                           var_popn,
                           agetime_step,
                           age_open,
                           times) {
  if (agetime_step == 1L)
    age <- agetime::age_labels_one(lower_last = age_open)
  else
    age <- agetime::age_labels_five(lower_last = age_open)
  classif_vars <- list(age, c("Female", "Male"))
  names(classif_vars) <- c(var_age, var_sex)
  if (!is.null(var_time)) {
    time <- list(times)
    names(time) <- var_time
    classif_vars <- c(classif_vars, time)
  }
  ans <- expand.grid(classif_vars,
                     KEEP.OUT.ATTRS = FALSE)
  ans[[var_popn]] <- NA_real_
  ans <- tibble::tibble(ans)
  ans
}
  
    
make_times <- function(agetime_step,
                       popn_obs,
                       var_time) {
  if (is.null(var_time))
    return(NULL)
  val_time <- popn_obs[[var_time]]
  time_first <- min(val_time, na.rm = TRUE)
  time_last <- max(val_time, na.rm = TRUE)
  seq.int(from = time_first,
          to = time_last,
          by = agetime_step)
}

make_var_age <- function(popn_obs) {
  nms <- names(popn_obs)
  var_age <- poputils::find_var_age(nms)
  if (is.null(var_age))
    cli::cli_abort(paste("Could not identify an age variable",
                         "in {.arg popn_obs}."))
  var_age
}

make_var_popn <- function(popn_obs) {
  nms <- names(popn_obs)
  var_popn <- find_var_popn(nms)
  if (is.null(var_popn))
    cli::cli_abort(paste("Could not identify a population",
                         "variable in {.arg popn_obs}."))
  var_popn
}
  

make_var_sex <- function(popn_obs) {
  nms <- names(popn_obs)
  var_sex <- poputils::find_var_sexgender(nms)
  if (is.null(var_sex))
    cli::cli_abort(paste("Could not identify a sex/gender",
                         "variable in {.arg popn_obs}."))
  var_sex
}


make_var_time <- function(popn_obs) {
  nms <- names(popn_obs)
  poputils::find_var_time(nms)
}






  
## make_index_popn_obs_popn_true <- function(popaccount) {
##   popn_obs <- get_popn_obs(popaccount)
##   popn_true <- popn_true(popaccount)
##   if (is.null(popn_obs))
##     return(seq_len(nrow(popn_true)))
##   var_age <- get_var_age(popaccount)
##   var_sex <- get_var_sex(popaccount)
##   var_time <- get_var_time(popaccount)
##   labels_total <- get_labels_total()
##   age_obs <- popn_obs[[var_age]]
##   age_true <- popn_true[[var_age]]
##   sex_obs <- popn_obs[[var_sex]]
##   sex_true <- popn_obs[[var_sex]]
##   time_obs <- popn_obs[[var_time]]
##   time_true <- popn_true[[var_time]]
##   i_best_matches_age <- make_i_best_matches_age(fine = age_true,
##                                                 coarse = age_obs,
##                                                 labels_total = labels_total)
##   i_best_matches_sex <- make_i_best_matches_sex(fine = sex_true,
##                                                 course = sex_obs,
##                                                 labels_total = labels_total)
##   i_matches_time <- make_i_matches_time(current = time_true,
##                                         target = time_obs)
##   intersect3 <- function(x, y, z) intersect(intersect(x, y), z)
##   i_best_match <- .mapply(intersect3,
##                           dots = list(x = i_best_matches_age,
##                                       y = i_best_matches_sex,
##                                       z = i_matches_time),
##                           MoreArgs = list())
##   is_not_length_1 <- lengths(i_best_match) != 1L
##   if (any(not_length_1L))
##     cli::cli_abort("Internal error: Did not find unique best match.")
##   unlist(i_best_match)
## }






## i_best_matches_age <- function(fine, coarse, labels_total) {
##   n_coarse <- length(coarse)
##   coarse_low <- rep.int(0L, times = n_coarse)
##   coarse_up <- rep.int(Inf, times = n_coarse)
##   is_total <- match(coarse, labels_total, nomatch = 0L) > 0L
##   coarse_low[!is_total] <- agetime::age_lower(coarse[!is_total])
##   coarse_up[!is_total] <- agetime::age_upper(coarse[!is_total])
##   coarse_width <- coarse_up - coarse_low
##   fine_low <- agetime::age_lower(fine)
##   fine_up <- agetime::age_lower(fine)
##   which_in_coarse_lowest_width <- function(x) {
##     i_in_coarse <- which((coarse_low <= x) & (x <= coarse_up))
##     if (length(i_in_coarse) == 0L)
##       cli::cli_abort("Internal error: Failed to find category in coarse.")
##     widths <- coarse_width[i_in_coarse]
##     is_keep <- widths == min(widths)
##     i_in_coarse[is_keep]
##   }
##   lapply(fine, which_in_coarse_lowest_width)
## }

## #' Find Best Matches for Sex
## #'
## #' For each element of 'fine', get indices of
## #' 'coarse' with the most precise match
## #'
## #' @param fine
## #' @param coarse
## #'
## #' @returns A list of integer vectors
## #'
## #' @noRd
## make_i_best_matches_sex <- function(fine, coarse) {
##   labels_total <- get_labels_total()
##   i_total <- which(tolower(coarse) %in% labels_total)
##   which_in_coarse_most_precise <- function(x) {
##     ans <- which(coarse == x)
##     if (length(ans) > 0L)
##       return(ans)
##     i_total
##   }
##   lapply(fine, which_in_coarse_most_precise)
## }


## ## HAS_TESTS
## #' Find Matches for Time
## #'
## #' For each element of 'current', get indices of
## #' 'target' where there is an exact match.
## #'
## #' @param current
## #' @param target
## #'
## #' @returns A list of integer vectors
## #'
## #' @noRd
## make_i_matches_time <- function(current, target) {
##   lapply(current, function(x) which(target == x))
## }

    
  
