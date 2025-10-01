#' Manhattan plot
#' 
#' 
#' 
#' 
plot_manhattan <- function(data, col.name = "score") {
  
  stopifnot(is.data.frame(data))
  stopifnot(col.name %in% colnames(data))
  
  if (is.null(traits)) {
    traits <- names(data@scores)
  }
  else {
    stopifnot(all(is.element(traits, names(data@scores))))
  }
  n.trait <- length(traits)
  all.models <- colnames(data@scores[[1]])
  if (is.null(models)) {
    models <- all.models
  }
  else {
    dom.models <- models[grep("dom", models, fixed = T)]
    models <- setdiff(models, dom.models)
    if (length(dom.models) > 0) {
      dom.models <- unlist(lapply(as.list(dom.models), 
                                  function(x) {
                                    all.models[grep(x, all.models, fixed = T)]
                                  }))
    }
    models <- union(models, dom.models)
    stopifnot(all(is.element(models, all.models)))
  }
  plotme <- thresh.data <- NULL
  if (is.null(chrom)) {
    x <- get_x(data@map[, 2:3])
    ix <- 1:nrow(data@map)
  }
  else {
    stopifnot(chrom %in% levels(data@map$Chrom))
    ix <- which(as.character(data@map$Chrom) == chrom)
    x <- data@map$Position[ix]/1e+06
  }
  for (k in 1:n.trait) {
    scores <- as.data.frame(data@scores[[traits[k]]][ix, 
                                                     models])
    colnames(scores) <- models
    scores$x <- x
    scores$color <- factor(ifelse(as.integer(factor(data@map[ix, 
                                                             2]))%%2 == 1, 1, 0))
    tmp <- pivot_longer(data = scores, cols = match(models, 
                                                    colnames(scores)), names_to = "model", values_to = "y", 
                        values_drop_na = TRUE)
    tmp$trait <- traits[k]
    if (inherits(data, "GWASpoly.thresh")) {
      thresh.data <- rbind(thresh.data, data.frame(y = max(data@threshold[traits[k], 
                                                                          models]), trait = traits[k]))
    }
    plotme <- rbind(plotme, tmp)
  }
  plotme$trait <- factor(plotme$trait)
  plotme$model <- gsub(pattern = "-ref", replacement = "", 
                       x = plotme$model)
  plotme$model <- gsub(pattern = "-alt", replacement = "", 
                       x = plotme$model)
  plotme$model <- factor(plotme$model)
  p <- ggplot(data = plotme, aes(x = .data$x, y = .data$y, 
                                 colour = .data$color, shape = .data$model)) + ylab(expression(paste("-log"[10], 
                                                                                                     "(p)"))) + guides(colour = "none") + theme_bw() + theme(text = element_text(size = 15), 
                                                                                                                                                             panel.grid = element_blank()) + geom_point() + scale_shape(solid = FALSE) + 
    facet_wrap(~trait)
  if (is.null(chrom)) {
    allchr <- unique(data@map[, 2])
    breaks <- (tapply(x, data@map[, 2], max) + tapply(x, 
                                                      data@map[, 2], min))/2
    p <- p + scale_x_continuous(name = "Chromosome", breaks = breaks, 
                                labels = allchr) + scale_colour_manual(values = c("#21908c", 
                                                                                  "#440154"))
  }
  else {
    p <- p + scale_x_continuous(name = "Position (Mb)") + 
      scale_colour_manual(values = "#440154")
  }
  if (inherits(data, "GWASpoly.thresh")) {
    p <- p + geom_hline(data = thresh.data, mapping = aes(yintercept = .data$y), 
                        linetype = 2, colour = "grey50")
  }
  return(p)
  
  
  
}




