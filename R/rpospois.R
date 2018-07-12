#' Sample from a positive Poisson distribution
#'
#' @description
#' Generates random samples from a positive Poisson distribution. This distribution
#' ensures non-zero counts for every random sample.
#'
#' @param n The number of independent samples to draw from the distribution
#' @param lambda The mean parameter for a Poisson Distribution.
#'
#' @return A numeric vector with counts given a mean parameter \code{lambda}.
#'
#' @export
#'
#' @examples
#' rpospois(n = 10, lambda = 1)
#'
rpospois <- function(n, lambda) {
  qpois(runif(n, min = dpois(0, lambda), max = 1), lambda)
}
