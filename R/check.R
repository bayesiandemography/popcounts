
check_time_step <- function(time_step) {
  valid_vals <- c(1L, 5L)
  poputils::check_n(n = time_step,
                    nm_n = "time_step",
                    min = NULL,
                    max = NULL,
                    divisible_by = 1L)
  if (!(time_step %in% valid_vals))
    cli::cli_abort(c("Invalid value for {.arg time_step}.",
                     i = "Value supplied: {.val {time_step}}.",
                     i = "Valid values: {.val {valid_vals}}."))
  invisible(TRUE)
}

check_age_open <- function(age_open, time_step) {
  poputils::check_n(n = age_open,
                    nm_n = "age_open",
                    min = 0L,
                    max = NULL,
                    divisible_by = 1L)
  if (age_open %% time_step != 0L)
    cli::cli_abort(c("{.arg age_open} not a multiple of {.arg age_open}.",
                     i = "{.arg age_open}: {.val {age_open}}.",
                     i = "{.arg time_step}: {.val {time_step}}."))
  invisible(TRUE)
}


                     


check_is_popaccount <- function(x, nm_x) {
  if (!inherits(x, "popaccount"))
    cli::cli_abort(c("{.arg {nm_x}} is not a population account.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  TRUE
}

check_is_data_frame <- function(x, nm_x) {
  if (!inherits(x, "data.frame"))
    cli::cli_abort(c("{.arg {nm_x}} is not a data frame.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  invisible(TRUE)
}

check_data_frame_has_nonzero_rows <- function(x, nm_x) {
  if (identical(nrow(x), 0L))
    cli::cli_abort("{.arg nm_x} has zero rows.")
  invisible(TRUE)
}


#' Check That No Arguments Absorbed By Dots in Function
#'
#' @param dots Arguments absorbed
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_has_no_dots <- function(...) {
  dots <- list(...)
  n_dot <- length(dots)
  if (n_dot > 0L) {
    nms <- names(dots)
    if (is.null(nms))
        cli::cli_abort("Invalid unnamed {cli::qty{n_dot}} argument{?s}.")
    else {
      i_nonblank <- match(TRUE, nzchar(nms))
      nm <- nms[[i_nonblank]]
      cli::cli_abort("{.arg {nm}} is not a valid argument.")
    }
  }
  invisible(TRUE)
}


check_popn <- function(df, nm_df, var_popn) {
  popn <- df[[var_popn]]
  if (!is.numeric(popn))
    cli::cli_abort(c(paste("Variable {.var {var_popn}} in {.arg {nm_df}}",
                           "is non-numeric."),
                     i = "{.var {var_popn}} has class {.cls {class(popn)}}."))
  is_obs <- !is.na(popn)
  if (!any(is_obs))
    cli::cli_abort(paste("Variable {.var {var_popn}} in {.arg {nm_df}}",
                         "has no observations."))
  n_neg <- sum(popn[is_obs] < 0)
  if (n_neg > 0L)
    cli::cli_abort(paste("Variable {.var {var_popn}} in {.arg {nm_df}}",
                         "has negative {cli::qty(n_neg)} value{?s}."))
  invisible(TRUE)
}


check_popn_obs <- function(popn_obs) {
  ## TODO - check age, sex, and time variables are valid
  check_is_data_frame(x = popn_obs, nm_x = "popn_obs")
  check_data_frame_has_nonzero_rows(x = popn_obs, nm_x = "popn_obs")
  nms <- names(popn_obs)
  var_age <- make_var_age(popn_obs)
  var_sex <- make_var_sex(popn_obs)
  var_time <- make_var_time(popn_obs)
  var_popn <- make_var_popn(popn_obs)
  nms_classif_vars <- c(var_age, var_sex, var_time)
  classif_vars <- popn_obs[nms_classif_vars]
  if (anyDuplicated(classif_vars))
    cli::cli_abort(paste("{.arg popn_obs} has multiple rows with the",
                         "same values for {.var {nms_classif_vars}}."))
  check_popn(df = popn_obs,
             nm_df = "popn_obs",
             var_popn = var_popn)
  invisible(TRUE)
}



check_prob_obs <- function(prob_obs, sex, age) {
  if (is.null(prob_obs))
    return(invisible(TRUE))
  if (is.numeric(prob_obs)) {
    if (!identical(length(prob_obs), 1L))
      cli::cli_abort(c("{.arg prob_obs} is numeric but does not have length 1.",
                       i = "{.arg prob_obs} has length {.val {length(prob_obs)}}."))
    if (is.na(prob_obs))
      cli::cli_abort(c("{.arg prob_obs} is NA."))
    if (!((0 < prob_obs) && (prob_obs < 1)))
      cli::cli_abort(c("{.arg prob_obs} not in interval (0, 1).",
                       i = "{.arg prob_obs}: {.val {prob_obs}}."))
    return(invisible(TRUE))
  }
  if (inherits(prob_obs, "data.frame")) {
    if (identical(nrow(prob_obs), 0L))
      cli::cli_abort("{.arg prob_obs} has 0 rows.")
    nms <- names(prob_obs)
    i_dup <- match(TRUE, duplicated(nms), nomatch = 0L)
    if (i_dup > 0L) {
      nm <- nms[[i_dup]]
      cli::cli_abort("{.arg prob_obs} has two variables called {.val {nm}}.")
    }
    for (nm in c("mean", "disp")) {
      i_var <- match(nm, tolower(nms), nomatch = 0L)
      if (i_var == 0L) {
        cli::cli_abort(c("{.arg prob_obs} does not have a variable called {.val {nm}}.",
                         i = "{.arg prob_obs} has variable{?s} {.val {nms}}."))
        val <- prob_obs[[i_var]]
        n_na <- sum(is.na(val))
        if (n_na > 0L)
          cli::cli_abort("Variable {.var {nm}} in {.arg prob_obs} has {cli::qty(n_na)} NA(?s).")
        n_out <- sum(!((0 < val) & (val < 1)))
        if (n_out > 0L)
          cli::cli_abort(paste("Variable {.var {nm}} in {.arg prob_obs} has",
                               "{cli::qty(n_na)} value{?s} outside the interval (0, 1)."))
      }
    }
    i_age <- match("age", tolower(nms), nomatch = 0L)
    if (i_age > 0L) {
      age_prob <- prob_obs[[i_age]]
      val <- agetime::age_check(x = age_prob,
                                no_overlap = TRUE,
                                no_gap = TRUE,
                                no_total = TRUE,
                                no_na = TRUE,
                                include_zero = TRUE,
                                include_open = TRUE)
      if (!val$ok)
        cli::cli_abort("{.var age} variable in {.arg prob_obs} invalid.") ## TODO - ADD MESSAGE FROM age_check
      is_inside <- agetime::matched(age, age_prob, type = "inside")
      i_not_inside <- match(FALSE, is_inside, nomatch = 0L)
      if (i_not_inside > 0L) {
        age_not <- age[[i_not_inside]]
        cli::cli_abort(paste("Age group {.val {age_not}} for true poulation",
                             "does not align with age groups used by",
                             " {.var {nms[[i_age]]}} variable",
                             "in {.arg prob_obs}."))
      }
    }
    i_sex <- match("sex", tolower(nms), nomatch = 0L)
    if (i_sex > 0L) {
      sex_prob <- prob_obs[[i_sex]]
      is_present <- tolower(sex) %in% tolower(sex_prob)
      i_not_present <- match(FALSE, is_present, nomatch = 0L)
      if (i_not_present > 0L) {
        sex_not <- sex[[i_not_present]]
        cli::cli_abort(paste("{.var sex} variable from {.arg prob_obs}",
                             "does not include {.val {sex_not}}."))
      }
    }      
    return(invisible(TRUE))
  }
  cli::cli_abort(c("{.arg prob_obs} not NULL, numeric, or data frame.",
                   i = "{.arg prob_obs} has class {.cls {class(prob_obs)}}."))
}
  
  
  
                     
                   
      
