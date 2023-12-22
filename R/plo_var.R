plo_var <- function(object, comps = 1:2, which = "both",
                    col = NULL, size = 3.88,
                    Yline = TRUE, col.Yline = "firebrick3") {

  dfX <- as.data.frame(object$projection)
  dfY <- pls::Yloadings(object)
  class(dfY) <- "matrix"
  dfY <- as.data.frame(dfY)
  coord <- rbind.data.frame(dfX, dfY)[,comps]
  names(coord) <- paste0("axis", 1:2)
  coord <- cbind(variable = rownames(coord),
                 coord,
                 side = factor(c(rep("X",nrow(dfX)), rep("Y", nrow(dfY)))))

  if(which %in% c("X","Y")) coord <- coord[coord$side == which,]
  coord$side <- factor(coord$side)

  p <-
    ggplot2::ggplot(coord, ggplot2::aes(x = .data$axis1, y = .data$axis2, label = .data$variable, color = .data$side)) +
    ggplot2::geom_hline(yintercept = 0, colour = "gray", linetype = "dashed", alpha = 0.6, linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "gray", linetype = "dashed", alpha = 0.6, linewidth = 0.3) +
    ggrepel::geom_text_repel(max.overlaps = 20, show.legend = FALSE, size = size) +
    ggplot2::xlab(paste("Comp", comps[1])) +
    ggplot2::ylab(paste("Comp", comps[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  if(!is.null(col)) p <- p + ggplot2::scale_color_manual(values = col)

  if(nrow(dfY)==1 & isTRUE(Yline)) {
    p <- p +
      geom_abline(intercept = 0, slope = dfY[1,2]/dfY[1,1],
                  color = col.Yline, alpha = 0.2) +
      geom_abline(intercept = 0, slope = -dfY[1,1]/dfY[1,2],
                  color = col.Yline, alpha = 0.2)
  }

  p

}
