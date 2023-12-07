plo_coef <- function(object, y = NULL, ncomp = NULL, sort = FALSE,
                     col = "darkgreen", repel = FALSE,
                     max.pval = NULL, whiskers = FALSE, ci = 0.95) {

  if(is.null(ncomp)) ncomp <- object$ncomp
  if(is.null(y)) y <- rownames(object$Yloadings)[1]

  beta <- object$coefficients[,y,ncomp]

  df <- data.frame(Variable = names(beta),
                   Coef = beta,
                   place = ifelse(beta>=0, 1, 0),
                   alph = rep(0.6, length(beta)))

  if(!is.null(max.pval) | isTRUE(whiskers)) {
    jk <- suppressWarnings(pls::jack.test(object, ncomp = ncomp))
  }

  if(!is.null(max.pval)) {
    pval <- jk$pvalues[,y,1]
    df$alph[pval > max.pval] <- 0.2
  }

  if(isTRUE(whiskers)) {
    df$sd <- jk$sd[,y,1]
  }

  if(isTRUE(sort)) {
    df <- df[order(df$beta, decreasing = TRUE),]
  }

  df$Variable <- factor(df$Variable, levels = df$Variable)

  p <-
    ggplot2::ggplot(df, ggplot2::aes(x = .data$Variable, y = .data$Coef)) +
    ggplot2::geom_hline(yintercept = 0, colour = "gray", linetype = "solid", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_col(ggplot2::aes(alpha = I(.data$alph)), color = "white", fill = col, show.legend = FALSE) +
    ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
    ggplot2::ylab(paste("Coefficients for", y)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())

  if(isTRUE(whiskers)) {
    tci <- stats::qt(1-(1-ci)/2, nrow(object$scores)-1)
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$Coef-tci*.data$sd, ymax = .data$Coef+tci*.data$sd), alpha = 0.5)
  }

  if(isTRUE(repel)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = .data$Variable, y = .data$Coef),
                                      alpha = 0.6)
  } else {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$Variable,
                                             y = .data$Coef+sign(.data$Coef)*0.001,
                                             vjust = .data$place),
                                alpha = 0.6)
  }

  p

}
