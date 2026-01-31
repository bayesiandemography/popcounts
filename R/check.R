

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

  
  
  
                     
                   
      
