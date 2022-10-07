#' Assign cores to a data.frame
#' 
#' @description 
#' Assigns the core (integer) to each row of a data.frame, given the desired number of cores.
#' 
#' @param df A data.frame.
#' @param n_core The number of cores (integer) to split the data.frame.
#' @param split Logical. Should the data.frame be split by cores?
#' 
#' @export
#' 
assign_cores <- function(df, n_core, split = FALSE) {
  
  # Test classes
  stopifnot(inherits(df, "data.frame"))
  stopifnot(inherits(n_core, "numeric"))
  
  n_core <- as.integer(n_core)
  
  df$core <- sort(rep(seq(n_core), length.out = nrow(df)))
  
  if (split) {
    df1 <- split(df, df$core)
    ## Append class
    structure(df1, class = c(class(df1), "core.list"))
  } else {
    df1 <- df
    ## Append class
    structure(df1, class = c(class(df1), "core.df"))
  }
  
}


#' Apply a function (optionally in parallel) to a list
#' 
#' @description 
#' Apply a function over data.frames assigned to a core. If on a Unix system (Linux or Mac),
#' the function will be applied in parallel. If using a Windows system, the function
#' will not be applied in parallel
#' 
#' @param X A list of data.frames over which to apply a function. Must inherit class
#' \code{core.list} from the \code{\link{assign_cores}} function.
#' @param FUN A function.
#' @param check.cores Logical. Should the number of requested cores (determined as the length
#' of list X) be compared against the number of available cores?
#' 
#' @import parallel
#' 
#' @export
#' 
coreApply <- function(X, FUN, check.cores = FALSE) {
  
  ## Class check
  stopifnot(inherits(X, "core.list"))
  stopifnot(is.function(FUN))
  
  ## Detect the system
  os <- Sys.info()["sysname"]
  
  ## Determine number of cores in the list
  n_core_requested <- length(X)
  
  # Check this against the number of cores in the system, if requested
  if (check.cores) {
    n_cores_avail <- detectCores()
    if (n_core_requested > n_cores_avail) 
      stop ("The number of requested cores is greater than the number of available cores.")
    
  }
  
  
  ## Split stream based on os
  if (os == "Windows") {
    
    # Apply a function via lapply
    lapply(X = X, FUN = FUN)
    
  } else {
    
    ## Apply a function via mclapply
    mclapply(X = X, FUN = FUN, mc.cores = n_core_requested)
    
  }
  
}












