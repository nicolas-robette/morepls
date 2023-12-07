plo_ctr <- function(object, comp = 1, sort = FALSE, col = "tomato4", repel = FALSE) {

  ctr <- object$loading.weights
  class(ctr) <- NULL
  ctr <- as.data.frame(ctr)[,comp]
  names(ctr) <- rownames(object$loading.weights)

  if(isTRUE(sort)) ctr <- rev(sort(ctr))
  df <- data.frame(Variable = factor(names(ctr), levels = names(ctr)),
                   Weight = ctr,
                   place = ifelse(ctr>=0, 1, 0))

  seuil <- sqrt(1/length(ctr))

  p <-
    ggplot2::ggplot(df, ggplot2::aes(x = .data$Variable, y = .data$Weight)) +
    ggplot2::geom_hline(yintercept = seuil, colour = "gray", linetype = "dashed", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = 0, colour = "gray", linetype = "solid", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = -seuil, colour = "gray", linetype = "dashed", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_col(color = "white", fill = col, alpha = 0.6) +
    ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
    ggplot2::ylab(paste0("Weight (comp ",comp,")")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  if(isTRUE(repel)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = .data$Variable, y = .data$Weight),
                                      alpha = 0.6)
  } else {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$Variable,
                                             y = .data$Weight+sign(.data$Weight)*0.01,
                                             vjust = .data$place),
                                alpha = 0.6)
  }

  p

}
