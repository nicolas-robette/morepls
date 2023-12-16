get_cor <- function(object) {

  if(is.data.frame(object$model)) {
    response <- attr(attr(object$model, "terms"), "response")
    Y <- as.data.frame(unclass(object$model[,response]))
    if(is.null(colnames(Y)) | grepl("unclass(object", colnames(Y), fixed = TRUE)[1]) colnames(Y) <- names(object$model)[response]
    X <- as.data.frame(unclass(object$model[,-response]))
  } else {
    X <- object$model[[2]]
    if(is.null(colnames(X))) colnames(X) <- rownames(object$loadings)
    Y <- object$model[[1]]
    if(!is.matrix(Y)) Y <- matrix(Y, nrow = length(Y))
    if(is.null(colnames(Y))) colnames(Y) <- rownames(object$Yloadings)
  }

  res <- list(Xt = stats::cor(X, object$scores),
              Yt = stats::cor(Y, object$scores),
              Xu = stats::cor(X, object$Yscores),
              Yu = stats::cor(Y, object$Yscores),
              XY = stats::cor(X, Y),
              tu = stats::cor(object$scores, object$Yscores))
  colnames(res$Xt) <- colnames(res$Yt) <- paste0("t", 1:ncol(res$Xt))
  colnames(res$Xu) <- colnames(res$Yu) <- paste0("u", 1:ncol(res$Xu))
  rownames(res$tu) <- paste0("t", 1:nrow(res$tu))
  colnames(res$tu) <- paste0("u", 1:ncol(res$tu))

  return(res)

}
