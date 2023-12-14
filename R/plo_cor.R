plo_cor <- function(object, comps = 1:2, which = "both", min.cor = NULL,
                    lim = NULL, circles = NULL, col = NULL, size = 3.88) {

  if(is.data.frame(object$model)) {
    response <- attr(attr(object$model, "terms"), "response")
    Y <- as.data.frame(unclass(object$model[,response]))
    if(is.null(colnames(Y)) | grepl("unclass(object", colnames(Y), fixed = TRUE)) colnames(Y) <- names(object$model)[response]
    X <- as.data.frame(unclass(object$model[,-response]))
  } else {
    X <- object$model[[2]]
    if(is.null(colnames(X))) colnames(X) <- rownames(object$loadings)
    Y <- object$model[[1]]
    if(!is.matrix(Y)) Y <- matrix(Y, nrow = length(Y))
    if(is.null(colnames(Y))) colnames(Y) <- rownames(object$Yloadings)
  }

  coord <- data.frame(stats::cor(data.frame(Y,X), object$scores[,comps]))
  names(coord) <- paste0("axis", 1:2)
  coord <- cbind(variable = names(data.frame(Y,X)),
                 coord,
                 max = apply(coord,1, function(x) max(abs(x))),
                 side = factor(c(rep("Y",ncol(Y)), rep("X", ncol(X)))))

  if(!is.null(min.cor)) coord <- coord[coord$max >= min.cor,]
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

  if(!is.null(lim)) p <- p + ggplot2::xlim(c(-lim,lim)) + ggplot2::ylim(c(-lim,lim))

  if(!is.null(col)) p <- p + ggplot2::scale_color_manual(values = col)

  if(!is.null(circles)) {
    if (!requireNamespace("ggforce", quietly = TRUE))
      stop("ggforce package should be installed to add circles to this plot")
    df <- data.frame(x0 = rep(0, length(circles)),
                     y0 = rep(0, length(circles)),
                     r = circles)
    p <- p + ggforce::geom_circle(data = df,
                                  ggplot2::aes(x0 = .data$x0, y0 = .data$y0, r = .data$r),
                                  inherit.aes = FALSE,
                                  color = "lightgray",
                                  linetype = "dashed",
                                  linewidth = 0.3,
                                  alpha = 0.6) +
      ggplot2::coord_fixed()
  }

  p

}
