
find_var_popn <- function(nms) {
  p_valid <- "^popn$|^population$|^value$|^count$"
  nms_cleaned <- tolower(nms)
  nms_cleaned <- gsub("[^a-z]", "", nms_cleaned)
  i <- grep(p_valid, nms_cleaned)
  if (identical(length(i), 1L)) 
    nms[[i]]
  else
    NULL
}


#' Draw pi ~ p(pi | y) when x has flat prior and y | x,pi ~ Binom(x,pi)
#'
#' Model:
#'   p(x) \propto 1 for x = 0,1,2,...
#'   y | x, pi ~ Binomial(x, pi)
#'
#' Prior for pi:
#'   pi = l + X*(u - l),  X ~ Beta(2,2),  with 0 < l < u < 1
#'
#' Sampling:
#'   Rejection sampling with proposal X ~ Beta(2,2)
#'   Accept with prob (pi/u)^(y-1), since pi <= u.
#'
#' @param y Integer scalar >= 1
#' @param l Lower bound for pi (0 < l < u)
#' @param u Upper bound for pi (l < u < 1)
#' @param max_tries Maximum rejection iterations before error
#' 
#' @return Numeric scalar pi in (l,u)
#'
#' @noRd
draw_pi_given_y <- function(y, l, u, max_tries = 1e6) {
  beta_param <- get_beta_param()
  shape1 <- beta_param[[1L]]
  shape2 <- beta_param[[2L]]
  # y == 1: posterior over pi is the prior (since (pi)^(y-1) = 1)
  if (y == 1L) {
    x <- stats::rbeta(n = 1L, shape1 = shape1, shape2 = shape2)
    return(l + x * (u - l))
  }
  # Rejection sampling
  for (i in seq_len(max_tries)) {
    x <- stats::rbeta(n = 1L, shape1 = shape1, shape2 = shape2)
    pi <- l + x * (u - l)
    # accept prob = (pi/u)^(y-1); compute on log scale for stability
    log_acc <- (y - 1L) * (log(pi) - log(u))
    # Compare log(U) < log_acc
    if (log(stats::runif(1L)) < log_acc)
      return(pi)
  }
  cli::cli_abort("Internal error: Exceeded 'max_tries' in rejection sampler.")
}


#' Draw x ~ p(x | y) under flat prior p(x) pi prior as shifted beta
#' 
#' Algorithm:
#'   1) Draw pi ~ p(pi | y) using draw_pi_given_y()
#'   2) Draw v = x - y ~ NegBin(size = y+1, prob = pi)
#'      (failures before the (y+1)th success)
#'   3) Return x = y + v
#'
#' @param y Integer scalar >= 1
#' @param l Lower bound for pi (0 < l < u)
#' @param u Upper bound for pi (l < u < 1)
#' @param max_tries Maximum rejection iterations for drawing pi
#' 
#' @return Integer scalar x >= y
#'
#' @noRd
draw_x_given_y <- function(y, l, u, max_tries = 1e6) {
  pi <- draw_pi_given_y(y = y, l = l, u = u, max_tries = max_tries)
  v <- stats::rnbinom(n = 1L, size = y + 1L, prob = pi)
  y + v
}
