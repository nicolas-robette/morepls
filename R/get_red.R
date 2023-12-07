get_red <- function(object) {

  res <- get_cor(object)
  res$XY <- NULL
  res$tu <- NULL

  res$Xt <- rbind(colMeans(res$Xt*res$Xt),
                  res$Xt*res$Xt)
  res$Yt <- rbind(colMeans(res$Yt*res$Yt),
                  res$Yt*res$Yt)
  res$Xu <- rbind(colMeans(res$Xu*res$Xu),
                  res$Xu*res$Xu)
  res$Yu <- rbind(colMeans(res$Yu*res$Yu),
                  res$Yu*res$Yu)
  rownames(res$Xt)[1] <- rownames(res$Yt)[1] <-
    rownames(res$Xu)[1] <- rownames(res$Yu)[1] <- "Rd"

  res$Xtcum <- t(apply(res$Xt, 1, cumsum))
  res$Ytcum <- t(apply(res$Yt, 1, cumsum))
  res$Xucum <- t(apply(res$Xu, 1, cumsum))
  res$Yucum <- t(apply(res$Yu, 1, cumsum))

  return(res)

}
