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


#' Z-transformation
#' 
#' @rdname ztrans
#' 
#' @export
#' 
zexp <- function(x) (exp(2 * x) - 1) / (exp(2 * x) + 1)



#' Generalized bootstrapper
#' 
#' @description Computes bootstrap resamples of a statistic
#' 
#' @param x A numeric vector.
#' @param y NULL (default) or a numeric vector for statistics that require a second
#' numeric vector
#' @param fun A character vector of the desired statistic function. For instance,
#' to calculate the variance of values in the numeric vector, pass "var".
#' @param boot.reps The number of bootstrapping replications.
#' @param alpha The significance level for calculating a confidence interval.
#' 
#' @import boot
#' 
#' @export
#' 
bootstrap <- function(x, y = NULL, fun, boot.reps = 1000, alpha = 0.05) {
  
  # Error handling
  boot.reps <- as.integer(boot.reps)
  
  # Error if the function does not exist
  stopifnot(exists(fun))
  # Otherwise get the function
  fun_torun <- get(fun)
  
  # Prob must be between 0 and 1
  alpha_check <- alpha > 0 | alpha < 1
  
  if (!alpha_check)
    stop("'alpha' must be between 0 and 1.")
  
  # Combine the data
  mat <- cbind(x, y)
  # How many columns
  n_col <- ncol(mat)
  
  
  ## Define a function for bootstrapping a single vector
  boot_fun_vec <- function(data, i) {
    # Use the index i to sample the data
    data_i <- data[i,,drop = FALSE]
    # Execute the function
    fun_torun(data_i[,1])
    
  }
  
  ## Define a function for bootstrapping a two vectors
  boot_fun_mat <- function(data, i) {
    # Use the index i to sample the data
    data_i <- data[i,]
    # Execute the function
    fun_torun(data_i[,1], data_i[,2])
    
  }
  
  # Calculate the base statistic
  base_stat <- if (n_col > 1) fun_torun(mat[,1], mat[,2]) else fun_torun(mat[,1])
  
  # If the base is not NA, proceed
  if (!is.na(base_stat)) {
    
    # Perform the bootstrapping
    if (n_col > 1) {
      boot_results <- boot(data = mat, statistic = boot_fun_mat, R = boot.reps)
      
    } else {
      boot_results <- boot(data = mat, statistic = boot_fun_vec, R = boot.reps)
      
    }
    
    # Standard error
    se <- sd(boot_results$t)
    # Bias
    bias <- mean(boot_results$t) - base_stat
    
    
    # Confidence interval
    ci_upper <- quantile(boot_results$t, 1 - (alpha / 2))
    ci_lower <- quantile(boot_results$t, (alpha / 2))
    
  } else {
    
    se <- bias <- ci_lower <- ci_upper <- NA
    
  }
  
  # Assemble list and return
  data.frame(statistic = fun, base = base_stat, se = se, bias = bias,
             ci_lower = ci_lower, ci_upper = ci_upper, row.names = NULL)
}



#' Generate leave-one-out resamples using a grouped \code{data.frame}
#' 
#' @description 
#' Expands the capability of the \code{\link[modelr]{crossv_loo}} function 
#' such that groups in a grouped data frame form the basis of folds to be
#' "left out" of a model training set for cross-validation.
#' 
#' @param data A grouped data frame.
#' @param id Name of variable that gives each training set a unique integer id.
#' 
#' @examples 
#' # Normal crossv_loo
#' cv_iris <- modelr::crossv_loo(iris)
#' 
#' # Note that the number of training paritions is equal to nrow(iris)
#' 
#' # Use the new function
#' cv_iris2 <- crossv_loo_grouped(group_by(iris, Species))
#' 
#' # Note that the number of training partitions is equal to n_groups(data)
#' 
#' 
#' 
#' @import dplyr
#' @import modelr
#' 
#' @export
#' 
crossv_loo_grouped <- function(data, id = ".id") {
  
  stopifnot(is.data.frame(data))
  stopifnot(is_grouped_df(data))
  stopifnot(is.character(id))
  
  # Get the group keys
  grp_keys <- group_keys(data)
  ## Split rows by group
  grp_rows <- group_rows(data)
  
  ## Ungroup the data frame
  df <- ungroup(data)
  
  # For each key, outer join for train and inner join for test
  test_list <- lapply(X = grp_rows, FUN = resample, data = df)
  train_list <- lapply(X = grp_rows, FUN = function(rows) resample(data = df, idx = setdiff(unlist(grp_rows), rows)))
  
  # Package into tibble
  grp_keys[["train"]] <- train_list
  grp_keys[["test"]] <- test_list
  grp_keys[[id]] <- seq_along(grp_keys[[1]])
  
  return(grp_keys)

}
