
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
#'   p(x) \propto 1 for x = 1,2,...
#'   y | x, pi ~ Binomial(x, pi)
#'
#' Prior for pi:
#'   pi = l + X*(u - l),  X ~ Beta(a,b),  with 0 < l < u < 1
#'
#' Sampling:
#'   Rejection sampling with proposal X ~ Beta(2,2)
#'   Accept with prob (pi/u)^(y-1), since pi <= u.
#'
#' @param y Integer scalar >= 1
#' @param l Lower bound for pi (0 < l < u)
#' @param u Upper bound for pi (l < u < 1)
#' @param a,b Beta distribution parameters
#' @param max_tries Maximum rejection iterations before error
#' 
#' @return Numeric scalar pi in (l,u)
#'
#' @noRd
draw_pi_given_y <- function(y, l, u, a, b, max_tries = 1e6) {
  for (i in seq_len(max_tries)) {
    x <- stats::rbeta(n = 1L, shape1 = a, shape2 = b)
    pi <- l + x * (u - l)
    if (stats::runif(1L) < l / pi)
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
#' @param a,b Beta parameters
#' @param max_tries Maximum rejection iterations for drawing pi
#' 
#' @return Integer scalar x >= y
#'
#' @noRd
draw_x_given_y <- function(y, l, u, a, b, max_tries = 1e6) {
  pi <- draw_pi_given_y(y = y,
                        l = l,
                        u = u,
                        a = a,
                        b = b,
                        max_tries = max_tries)
  v <- stats::rnbinom(n = 1L,
                      size = y + 1L,
                      prob = pi)
  y + v
}
