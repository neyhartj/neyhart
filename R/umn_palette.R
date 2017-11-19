#' University of Minnesota Color Palette
#'
#' @description A vector of hexadecimal color codes that are designed to work 
#' well together with the University of Minnesota color scheme.
#'
#' @format A nested list of character vectors of hexidecimal codes.
#'
#' @source \url{https://www.ur.umn.edu/brand/assets/pdf/secondary_colors_rgb.pdf}
#'
"umn_palettes"


#' University of Minnesota Color Generator
#'
#' @description Generates hexidecimal colors from the University of Minnesota
#' color palette. By default, the two primary colors are provided. Arguments
#' in this function define the secondary colors to choose.
#'
#' @param name The name of the secondary color tier. Options are "Secondary_Tier1",
#' "Secondary_Tier2", nad  "Secondary_Tier3".
#' @param n The number of desired colors from the palette.
#' @param type The type of palette.
#'
#' @return A \code{palette} vector of hexidecimal color codes from the University
#' of Minnesota color palette (see \code{\link[neyhart]{umn_palette}})
#'
#' @details
#' This function is inspired by \code{\link[wesanderson]{wes_palette}}.
#'
#' @export
#'
umn_palette <- function(name = "Secondary_Tier1", n, type = c("discrete", "continous")) {
  
  type <- match.arg(type)
  
  # Grab the primary colors
  primary <- umn_palettes$Primary
  
  secondary <- umn_palettes[[name]]
  
  if (is.null(secondary)) 
    stop("Palette not found.")
  if (missing(n)) {
    n <- 2 + length(secondary)
  }
  
  # Combine primary and secondary
  pal <- c(primary, umn_palettes[[name]])
  
  if (type == "discrete" & n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type, continuous = colorRampPalette(pal)(n), 
                discrete = pal[1:n])
  
  structure(out, class = "palette", name = name)

}


#' Print a palette object
#' 
#' @description 
#' This is re-exported from \code{\link{wesanderson}}.
#' 
#' @export
#' 
print.palette <- function(x, ...) {
  
    n <- length(x)
    old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
    on.exit(par(old))
    image(1:n, 1, as.matrix(1:n), col = x, ylab = "", xaxt = "n", 
          yaxt = "n", bty = "n")
    rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
    text((n + 1)/2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

