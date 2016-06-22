#' Bootstrap a correlation
#'
#' @param data A two-column matrix of data to correlate
#' @param boot.reps An integer of how many bootstrapping replications to perform.
#' @param CI A floating point that specifies the percentile confidence interval to report from bootstrapping. Default is 0.95.
#' @return A list containing the correlation coefficient (r), the standard deviation of bootstrapping correlation coefficients (r.sd.hat), and the confidence interval surrounding the mean (CI)
#' @export
#' @examples
#' data <- replicate(2, rnorm(100))
#' boot.cor(data = data, boot.reps = 1000)
#'
#' @import boot
#'
#'
#'
boot.cor <- function(data, boot.reps, CI = 0.95) {

  # Define a function for the correlation
  boot.cor <- function(input.data, i) {
    rep.data <- input.data[i,]
    return(cor(rep.data[,1], rep.data[,2]))
  }

  # Perform the bootstrapping
  boot.results <- boot(data = data, statistic = boot.cor, R = boot.reps)

  # Calculate a confidence interval
  CI.ind <- boot.reps * ((1 - CI) / 2)

  CI.upper <- sort(boot.results$t, decreasing = T)[CI.ind]
  CI.lower <- sort(boot.results$t, decreasing = F)[CI.ind]

  # Assemble list
  results.out <- list(r = boot.results$t0, r.sd.hat = sd(boot.results$t), CI = c(CI.lower, CI.upper))

  # Parse the results and return
  return(results.out)
}
