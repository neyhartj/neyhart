#' Z-transformation
#' 
#' @description 
#' Perform a z-transformation for modeling estimates for correlation coefficients and convert the z-score
#' back to a correlation coefficient.
#' 
#' @param x A correlation coefficient estimate.
#' 
#' @export
#' 
ztrans <- function(x) 0.5 * (log((1 + x) / (1 - x)))


#' 
#' @rdname ztrans
#' 
#' @export
#' 
zexp <- function(x) (exp(2 * x) - 1) / (exp(2 * x) + 1)
