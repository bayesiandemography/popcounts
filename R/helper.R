
## ## Miscellaneous helper functions

## #' Test Whether Feasible to Use Five-Year Age-Time Steps
## #'
## #' Requires that all age groups have widths divisible by 5
## #' and that, if time is present, time intervals
## #' have widths divisible by 5
## #'
## #' @param age_obs Age variable from 'popn_obs'
## #' @param time_obs Time variable from 'popn_obs',
## #' or (if no time variable present) NULL.
## #'
## #' @returns TRUE or a text string explaining
## #' why five-year age group not feasible
## #'
## #' @noRd
## can_use_agetime_step_five <- function(age_obs, time_obs) {
##   labels_unknown <- get_labels_unknown()
##   is_unknown <- age_obs %in% labels_unknown
##   age_lower_obs <- agetime::age_lower(age_obs[!is_unknown])
##   age_upper_obs <- agetime::age_upper(age_obs[!is_unknown])
##   invalid_lower <- age_lower_obs %% 5L != 0L
##   invalid_upper <- (is.finite(age_upper_obs)
##     & (age_upper_obs %% 5L != 0L))
##   i_invalid_age <- match(TRUE, invalid_lower | invalid_upper, nomatch = 0L)
##   if (i_invalid_age > 0L) {
##     age_invalid <- age_obs[[i_invalid_age]]
##     msg <- paste0("Age group \"", age_invalid, "\" not compatible",
##                   "with 5-year age-time step.")
##     return(msg)
##   }
##   has_time <- !is.null(time_obs)
##   if (has_time) {
##     times <- sort(unique(time_obs))
##     if (length(times) > 1L) {
##       time_steps <- diff(times)
##       is_invalid_time <- time_steps %% 5L != 0L
##       i_invalid_time <- match(TRUE, is_invalid_time, nomatch = 0L)
##       if (i_invalid_time > 0L) {
##         time_invalid_1 <- times[[i_invalid_time]]
##         time_invalid_2 <- times[[i_invalid_time + 1L]]
##         msg <- paste0("Times ", time_invalid_1, " and ", time_invalid_2,
##                       "not separated by 5 years, so not compatible",
##                       "with 5-year age-time step.")
##         return(msg)
##       }
##     }
##   }
##   TRUE
## }
