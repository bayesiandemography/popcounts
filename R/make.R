
make_age_sex <- function(Lx, asfr, pr_fem, offset_repr, age_mid) {
  mx <- pr_fem * asfr
  n_repr <- length(asfr)
  s_repr <- seq.int(from = offset_repr,
                    length.out = n_repr)
  Lx_repr <- Lx[s_repr]
  age_mid_repr <- age_mid[s_repr]
  r <- poputils::.intrinsic_growth_rate(mx = mx,
                                        Lx = Lx_repr,
                                        age_mid = age_mid_repr)
  n2 <- length(Lx) %/% 2L
  pr <- rep(c(pr_fem, 1 - pr_fem), each = n2)
  ans <- exp(-r * age_mid_all) * pr * Lx_all
  ans <- ans / sum(ans)
  ans
}



make_control <- function(popn_obs,
                         agetime_step,
                         age_open,
                         n_draw) {
  agetime_step <- make_agetime_step(agetime_step = agetime_step,
                                    popn_obs)
  age_open <- make_age_open(age_open = age_open,
                          popn_obs)
  type <- if (agetime_step == 1L) "single" else "five"
  age_labels <- poputils::age_labels(type = type, age_open = age_open)
  nms <- names(popn_obs)
  var_age <- poputils::find_var_age(nms)
  var_sex <- poputils::find_var_sexgender(nms)
  var_time <- poputils::find_var_time(nms)
  list(agetime_step = agetime_step,
       age_labels = age_labels,
       n_draw = 1000L,
       is_fitted = FALSE,
       var_age = var_age,
       var_sex = var_sex,
       var_time = var_time)
}  

                         


make_sysmod <- function(tfr_lim,
                        e0_lim,
                        asfr_std,
                        lx_std,
                        sex_ratio) {
  asfr_std <- make_asfr_std(asfr_std)
  lx_std <- make_lx_std(lx_std)
                                  sysmod <- list(tfr_lim = tfr_lim,
                                                 e0_lim = e0_lim,
                                                 total_lim = total_lim,
                                                 asfr_std = asfr_std,
                                                 lx_std = lx_std,
                                                 sex_ratio = sex_ratio)
                                  popaccount$sysmod <- sysmod
                                  popaccount
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
##   coarse_low[!is_total] <- poputils::age_lower(coarse[!is_total])
##   coarse_up[!is_total] <- poputils::age_upper(coarse[!is_total])
##   coarse_width <- coarse_up - coarse_low
##   fine_low <- poputils::age_lower(fine)
##   fine_up <- poputils::age_lower(fine)
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

    
  
