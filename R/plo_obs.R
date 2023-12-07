plo_obs <- function(object, comps = 1:2, col = "black", size = 1.5) {

  indiv <- pls::scores(object)
  class(indiv) <- "matrix"
  indiv <- as.data.frame(indiv)[,comps]
  names(indiv) <- paste0("axis", 1:2)
  p <-
    ggplot2::ggplot(indiv, ggplot2::aes(x = .data$axis1, y = .data$axis2)) +
    ggplot2::geom_hline(yintercept = 0, colour = "gray", linetype = "solid", alpha = 0.6) +
    ggplot2::geom_vline(xintercept = 0, colour = "gray", linetype = "solid", alpha = 0.6) +
    ggplot2::geom_point(color = col, size = size) +
    ggplot2::xlab(paste("Comp", comps[1])) +
    ggplot2::ylab(paste("Comp", comps[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
  p

}
