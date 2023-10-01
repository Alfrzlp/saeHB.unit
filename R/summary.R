#' Summary sae HB model
#'
#' @param object sae HB model
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' Xarea <- cornsoybeanmeans %>%
#'    dplyr::select(
#'       County = CountyIndex,
#'       CornPix = MeanCornPixPerSeg,
#'       SoyBeansPix = MeanSoyBeansPixPerSeg
#'    )
#'
#' corn_model <- hb_BHF(
#'    CornHec ~ SoyBeansPix + CornPix,
#'    data_unit = cornsoybean,
#'    data_area = Xarea,
#'    domain = "County",
#'    iter.update = 20
#' )
#'
#' summary(corn_model)
#'
summary.saehb <- function(object, ...){
  stats::printCoefmat(object$coefficient)
}