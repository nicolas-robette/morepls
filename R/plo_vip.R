plo_vip <- function(object, ncomp = NULL, sort = FALSE, col = "steelblue4", repel = FALSE) {

  if (!requireNamespace("plsVarSel", quietly = TRUE))
    stop("plsVarSel package should be installed to use this type of plot")

  if(is.null(ncomp)) ncomp <- object$ncomp

  vip <- plsVarSel::VIP(object, ncomp)
  if(isTRUE(sort)) vip <- rev(sort(vip))
  df <- data.frame(Variable = factor(names(vip), levels = names(vip)), VIP = vip)

  p <-
    ggplot2::ggplot(df, ggplot2::aes(x = .data$Variable, y = .data$VIP)) +
    ggplot2::geom_hline(yintercept = 1, colour = "gray", linetype = "dashed", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_col(color = "white", fill = col, alpha = 0.6) +
    ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  if(isTRUE(repel)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = .data$Variable),
                                      vjust = 1,
                                      nudge_y = .05,
                                      alpha = 0.6)
  } else {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$Variable),
                                vjust = 1,
                                nudge_y = .05,
                                alpha = 0.6)
  }

  p

}
