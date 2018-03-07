#' Append data to a RData file
#' 
#' @description 
#' Save one or more \code{R} objects to a RData file already containing objects.
#' 
#' @param ... The names of objects to be saved.
#' @param list A character vector containing the names of objects to be saved.
#' @param file The name of a file where the data will be saved.
#' 
#' @export
#' 
save_append <- function(..., list, file) {
  
  # Check if file exits
  # If the file does not exist, show warning message
  if (!file.exists(file)) warning("'file' does not exist. A new file will be created.")
  
  # Create a new environment to store the data
  data_env <- new.env()
  
  # Load the data from the file into that environment
  load(file = file, envir = data_env, verbose = TRUE)
  
  # Finalize the list of files - this is taken from the 'save' function
  names <- as.character(substitute(list("X", "Y")))[-1]
  if (missing(list) && !length(names)) 
    warning("nothing specified to be save()d")
  list <- c(list, names)
  
  # Determine if any objects in the list are already in the file
  overlap <- list[list %in% ls(data_env)]
  if (length(overlap) > 0) warning(paste("The following objects will be overwritten in 'file':"), overlap)
  
  # Check if the objects exist
  ok <- vapply(list, exists, NA)
  stopifnot(all(ok))
  
  # Assign the objects in the list to the environment
  for (obj in list) assign(x = obj, value = get(obj), envir = data_env)
  
  # Save the file
  save(list = ls(data_env), file = file, envir = data_env)
  
  # End the function
}
  
  
  
  