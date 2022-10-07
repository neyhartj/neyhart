#' String functions
#' 
#' @param x A string
#' 
#' @export
#' 
str_add_space <- function(x) gsub(pattern = "([a-z])([A-Z])", replacement = "\\1 \\2", x = x)