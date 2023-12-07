# library(pls)
# data(yarn)
# pls <- mvr(density ~ NIR,
#            ncomp = 5,
#            data = yarn,
#            validation = "CV",
#            method = "oscorespls")
# object = pls
# str(pls::MSEP(object, "train", intercept = TRUE)$val)
# str(object$model)
# nrow(object$loadings)

get_Q2 <- function(object) {

  PRESS <- object$validation$PRESS
  RSS <- pls::MSEP(object, "train", intercept = TRUE)$val[1,,] * nrow(object$loadings)
  if(!is.matrix(RSS)) RSS <- matrix(RSS, nrow = nrow(PRESS), ncol = length(RSS))
  RSS <- matrix(RSS[,-ncol(RSS)], nrow = nrow(RSS))

  Q2kh <- 1 - PRESS/RSS
  Q2h <- 1 - colSums(PRESS)/colSums(RSS)
  Q2cumkh <- t(1 - apply(PRESS/RSS, 1, cumprod))
  Q2cumh <- t(1 - cumprod(colSums(PRESS)/colSums(RSS)))[1,]

  res <- list(Q2kh = Q2kh, Q2h = Q2h, Q2cumkh = Q2cumkh, Q2cumh = Q2cumh)
  return(res)

}
