#' Get confidence interval
#'
#' @param x Object of class bissecofit.
#' @param MAXCOEFVAR Threshold value for approximate CIs.
#'
#' @returns dataframe with the estimated values and their confidence interval
#' @export
get_CI <- function(x, MAXCOEFVAR = 1.5){

  stopifnot( inherits(x, 'bissecofit' ))
  vnames <-  c('alpha', 'omega', 'yscale')
  cx <- coef(x)[vnames]
  odf = as.data.frame(cx)
  odf$`2.5%` <- exp( log(coef(x)[vnames])-x$err*1.96 )
  odf$`97.5%` <- exp( log(coef(x)[vnames])+x$err*1.96 )
  odf$`97.5%`[ x$err > MAXCOEFVAR ] <- Inf
  odf <- round( odf, 3 )

  return(odf)

}

