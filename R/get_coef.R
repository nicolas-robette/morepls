get_coef <- function(object, y = NULL, ncomp = NULL, ci = 0.95, raw = FALSE) {

  if(is.null(ncomp)) ncomp <- object$ncomp
  if(is.null(y)) y <- rownames(object$Yloadings)[1]

  jk <- suppressWarnings(pls::jack.test(object, ncomp = ncomp))
  coef <- jk$coefficients[,y,1]
  sd <- jk$sd[,y,1]
  tvalues <- jk$tvalues[,y,1]
  pvalues <- jk$pvalues[,y,1]
  tci <- stats::qt(1-(1-ci)/2, nrow(object$scores)-1)
  cimin <- coef - tci*sd
  cimax <- coef + tci*sd

  res <- data.frame(round(coef,5),
                    round(sd,5),
                    round(tvalues,3),
                    round(pvalues,5),
                    round(cimin,5),
                    round(cimax,5))

  if(!is.null(object$scale) & isTRUE(raw)) {
    res$coef <- res$coef / object$scale
    res$sd <- res$sd / object$scale
    res$cimin <- res$cimin / object$scale
    res$cimax <- res$cimax / object$scale
  }

  # coef / object$scale
  # object$coefficients[,y,ncomp] / object$scale
  # diag(1/object$scale) %*% object$coefficients[,y,ncomp]

  colnames(res) <- c("coefficients", "std error", "t-value", "p-value",
                     paste0(as.character(100*(1-ci)/2),"%"),
                     paste0(as.character(100-100*(1-ci)/2),"%"))
  return(res)
}
