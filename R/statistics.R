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



#' Mean, standard errors, and confidence intervals of samples
#' 
#' @param x A numeric vector
#' @param mu A numer indicating the true value of the mean
#' @param alternative A character specifying the alternative hypothesis with
#' respect to \code{mu}.
#' @param level The confidence level, or \eqn{1 - \alpha}.
#' @param return.list Logical. Should the output be a list (TRUE) or a vector (FALSE)?
#' 
#' @importFrom stats t.test
#' 
#' @export
#' 
confidInt <- function(x, level = 0.95, return.list = TRUE, na.rm = FALSE) {
  
  stopifnot(is.logical(return.list))
  stopifnot(level >= 0 & level <= 1)
  
  alpha <- 1 - level
  
  x1 <- if (na.rm) x[!is.na(x)] else x
  
  ## Calculate the sample mean
  x_bar <- mean(x = x1)
  se <- sd(x = x1) / sqrt(length(x1))
  
  conf_diff <- se * qt(p = 1 - (alpha / 2), df = length(x1) - 1)
  
  # Output vector
  out <- setNames(object = c(x_bar, se, x_bar - conf_diff, x_bar + conf_diff),
                  nm = c("mean", "se", "lower", "upper"))
  
  if (return.list) as.list(out) else out
  
}







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



#' Generate cross-validation resamples using a grouped \code{data.frame}
#' 
#' @description 
#' Expands the capability of the \code{\link[modelr]{crossv_loo}} and 
#' \code{\link[modelr]{crossv_mc}} functions such that groups in a grouped
#' data frame form the basis of folds to be "left out" of a model training set 
#' for cross-validation.
#' 
#' @param data A grouped data frame.
#' @param id Name of variable that gives each training set a unique integer id.
#' 
#' @examples 
#' 
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


#' 
#' @examples 
#' 
#' library(agridat)
#' wheat <- vargas.wheat2.yield
#' 
#' # Normal crossv_mc
#' cv_wheat <- modelr::crossv_mc(data = wheat, n = 10, test = 0.25)
#' 
#' # Note that partitions are made across environments or genotypes
#' 
#' # Use the new function
#' cv_wheat2 <- crossv_mc_grouped(data = group_by(wheat, env), n = 15)
#' 
#' # Note that partitions are made according to the group
#' 
#' @rdname crossv_loo_grouped
#' 
#' @export
#'
crossv_mc_grouped <- function(data, n, test = 0.25, id = ".id") {
  
  stopifnot(is.data.frame(data))
  stopifnot(is_grouped_df(data))
  stopifnot(is.character(id))
  

  # Get the group keys
  grp_keys <- group_keys(data)
  ## Split rows by group
  grp_rows <- group_rows(data)
  
  ## Ungroup the data frame
  df <- ungroup(data)
  # List of all data rows
  data_rows <- seq_len(nrow(df))
  
  # Generate training sets based on the grp keys
  train_test_list <- crossv_mc(data = tibble(grp_rows), n = n, test = test, id = id)
  # Get the indices
  train_indices <- lapply(X = train_test_list$train, FUN = "[[", "idx")
  train_indices <- lapply(X = train_indices, FUN = function(tindx) unlist(grp_rows[tindx])) 
  
  # Create training resamples
  train_list <- lapply(X = train_indices, FUN = function(rows) resample(data = df, idx = rows))
  test_list <- lapply(X = train_indices, FUN = function(rows) resample(data = df, idx = setdiff(data_rows, rows)))

  out <- tibble(train = train_list, test = test_list, id = train_test_list[[id]])
  names(out)[3] <- id
  
  return(out)
  
}










#' Rapid cross-validation without removing samples
#' 
#' @description 
#' Implements cross-validation according to Gianola and Schon 2016. Returns
#' RMSE and r-squared
#' 
#' @param object A fitted model object
#' @param index A list of indices for leave-n-out cross-validation (i.e. the 
#' training set)
#' @param rapid Logical; should the rapid method be used?
#' 
#' @examples 
#' fit <- lm(Sepal.Length ~ ., iris)
#' 
#' indices <- lapply(X = crossv_loo_grouped(group_by(iris, Species))$train, "[[", "idx")
#' 
#' @import stats
#' @importFrom MASS ginv
#' 
#' @export
#' 
rapid_cv <- function(object, index, rapid = TRUE) {
  
  # Get the model frame
  mf <- model.frame(object)
  # Model matrix and crossprod inverse
  X <- model.matrix(object)
  XtX_inv <- solve(crossprod(X))
  # Response
  y <- model.response(mf)
  # Coefficients
  beta_hat <- coef(object)
  
  if (rapid) {
  
    # Iterate over indices
    cv_out <- lapply(X = index, FUN = function(modelInd) {
      # Subset X for the d testing datapoints
      X_d <- X[-modelInd,,drop = FALSE]
      d <- nrow(X_d)
      # H matrix
      H_d <- tcrossprod(X_d %*% XtX_inv, X_d)
      H_d_inv <- ginv(diag(d) - H_d)
      # Residuals
      e_d <- y[-modelInd] - X_d %*% beta_hat
      
      # New betas
      beta_hat_holdout <- beta_hat - (tcrossprod(XtX_inv, X_d) %*% H_d_inv %*% e_d)
      
      # yhat
      y_hat <- X_d %*% beta_hat_holdout
      # return
      y_hat
      
    })
    
  } else {
    
    # Iterate over indices
    cv_out <- lapply(X = index, FUN = function(modelInd) {
      # Subset X for the d testing datapoints
      X_d <- X[-modelInd,,drop = FALSE]
      d <- nrow(X_d)
      # H matrix
      H_d <- tcrossprod(X_d %*% XtX_inv, X_d)
      H_d_inv <- ginv(diag(d) - H_d)
      # Residuals
      e_d <- y[-modelInd] - X_d %*% beta_hat
      
      # New betas
      beta_hat_holdout <- beta_hat - (tcrossprod(XtX_inv, X_d) %*% H_d_inv %*% e_d)
      
      # yhat
      y_hat <- X_d %*% beta_hat_holdout
      # return
      y_hat
      
    })
    
    
  }
  
  ## Pull out y
  y_hat_all <- unlist(cv_out)
  
  # Calculate metrics and return
  R2 <- cor(y_hat_all, y)^2
  mse <- mean((y - y_hat_all)^2)
  
  c(R2 = R2, MSE = mse, RMSE = sqrt(mse))
  
}




#' ACF function for merMod objects
#' 
#' 
#' 
#' @export
#' 
ACF.lmerMod <- function (object, maxLag, resType = c("pearson", "response",  "normalized"), ...) {
  
  resType <- match.arg(resType)
  res <- resid(object, type = resType, asList = TRUE)
  
  randterms <- as.character(findbars(formula(object))[[1]])[-1:-2]
  res <- split(x = res, model.frame(object)[randterms])
  
  if (missing(maxLag)) {
    maxLag <- min(c(maxL <- max(lengths(res)) - 1, as.integer(10 * log10(maxL + 1))))
  }
  val <- lapply(res, function(el, maxLag) {
    N <- maxLag + 1L
    tt <- double(N)
    nn <- integer(N)
    N <- min(c(N, n <- length(el)))
    nn[1:N] <- n + 1L - 1:N
    for (i in 1:N) {
      tt[i] <- sum(el[1:(n - i + 1)] * el[i:n])
    }
    array(c(tt, nn), c(length(tt), 2))
  }, maxLag = maxLag)
  val0 <- rowSums(sapply(val, function(x) x[, 2]))
  val1 <- rowSums(sapply(val, function(x) x[, 1]))/val0
  val2 <- val1/val1[1L]
  z <- data.frame(lag = 0:maxLag, ACF = val2)
  attr(z, "n.used") <- val0
  class(z) <- c("ACF", "data.frame")
  z
}





