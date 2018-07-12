#' Find objects by name and class
#' 
#' @description 
#' Wrapper of the \code{\link[utils]{apropos}} function for finding objects, but
#' includes the ability to find objects based on class. "c"find for "class" find.
#' 
#' @param what See \code{\link[utils]{apropos}}
#' @param ignore.case See \code{\link[utils]{apropos}}
#' @param class character string with a class. Objects of class \code{class} will
#' be returned.
#' 
#' @export
#' 
cfind <- function(what, ignore.case = TRUE, class = "any") {
  
  # Error handling
  if (!is.logical(ignore.case)) stop("'ignore.case' must be logical.")
  if (!is.character(class)) stop("'class' must be character.")
  
  # Wrap around the 'apropos' function
  objects <- apropos(what = what, ignore.case = ignore.case, mode = "any")
  
  # Determine the classes of those objects
  object.class <- sapply(X = objects, FUN = function(obj)
    class(get(obj)) )
  
  # If 'any' class is desired, return all objects
  if (class == "any") {
    return(objects)
    
    # Else subset the objects by the desired class
  } else {
    desired.objects <- objects[sapply(object.class, FUN = function(classes) class %in% classes)]
    return(desired.objects)
    
  }
  
} # Close the function