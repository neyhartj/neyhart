#' Convert between TASSEL hapmap file and a fastPHASE input
#' 
#' @description
#' Converts from a TASSEL hapmap file, which typically has diploid encoding of
#' genotypes (i.e. AA or CC or AC) to an input file for fastPHASE, which is formatted
#' line-by-line and contains information that is more haplotype-based.
#'
#' @param hapmap A TASSEL-encoded hapmap file read in to R as a data.frame. See \code{Details} 
#' for information on file format. Mutually exclusive with \code{filein}.
#' 
#' @param filein The path to a TASSEL-encoded hapmap file. See \code{Details} 
#' for information on file format. Mutually exclusive with \code{hapmap}.
#' @param file The output file name.
#'
#' @details 
#' The TASSEL format is as such:
#' The first row is column names. The first 4 columns are marker name, alleles, 
#' chromosome, and position, respectively. The next 7 column are additional information for 
#' TASSEL. The remaining columns are samples. Genotypes should be encoded in 
#' diploid format (i.e. AA, AC, CC) with "NN" denoting missing data.
#' 
#' @return 
#' Writes a fastPHASE-ready input file to the designated filepath.
#'
#'
#' @export
#'
#'
tassel2fastPHASE <- function(hapmap = NULL, filein = NULL, fileout) {
  
  ## Error checking
  # Both hapmap and filein cannot be NULL
  if (all(is.null(hapmap), is.null(filein))) stop("'hapmap' and 'filein' cannot both be NULL.")
  # They also cannot both be provided
  if (!any(is.null(hapmap), is.null(filein))) stop("'hapmap' and 'filein' cannot both be provided.")
  
  # Verify data in
  if (!is.null(hapmap)) {
    hapmap <- as.data.frame(hapmap)
  } else { # Read in data if not provided
    hapmap <- read.table(file = filein, header = TRUE, as.is = TRUE, check.names = FALSE, comment.char = "")
  }
  
  # Extract the positions
  pos <- hapmap[,4]
  # Conver to a format suitable for fastPHASE
  pos.fp <- paste0(c("P", pos), collapse = " ")
  
  # Find the number of sites
  n.sites <- nrow(hapmap)
  
  # Extract the sample names
  sample.names <- colnames(hapmap)[-c(1:11)]
  # Find then number of samples
  n.samples <- length(sample.names)
  
  # Subset the hapmap into a genotype matrix
  geno.mat <- as.matrix(hapmap[,-c(1:11)])
  
  # The number of lines in the output file is equal to
  ## 3 + (3 * n.samples)
  n.out.lines <- 3 + (3 * n.samples)
  
  # Create a list to print
  to.print <- vector("character", n.out.lines)
  # Add the number of sample, number of sites, and positons
  to.print[1] <- n.samples
  to.print[2] <- n.sites
  to.print[3] <- pos.fp
  
  
  # Apply over sample names
  geno.recode <- lapply(X = sample.names, FUN = function(n) {
    
    # Subset the geno.mat
    geno.mat.n <- subset.matrix(x = geno.mat, select = colnames(geno.mat) %in% n)
    
    # Replace NN with ??
    geno.mat.n[geno.mat.n == "NN",] <- "??"
    
    # Split the alleles in geno.mat.n
    geno.mat.n.split <- strsplit(x = geno.mat.n, split = "")
    
    # Return the first alleles
    alleles1 <- sapply(X = geno.mat.n.split, FUN = function(geno) geno[1])
    # Collapse into string
    alleles1 <- paste0(alleles1, collapse = "")
    # Repeat for the second alleles
    alleles2 <- sapply(X = geno.mat.n.split, FUN = function(geno) geno[2])
    alleles2 <- paste0(alleles2, collapse = "")
    
    # Return
    list(paste("#", n), alleles1, alleles2) })
  
  # Add to the to.print list
  to.print[-c(1:3)] <- unlist(geno.recode)
  
  # Open a file
  file.connection <- file(fileout)
  # Write
  writeLines(to.print, file.connection)
  # Close
  close(file.connection)
  
  # Print a message to the user
  print(paste("File written to: ", fileout, ".", sep = ""))
  
} # Close the function