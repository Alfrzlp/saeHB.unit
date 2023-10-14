#' Autoplot
#'
#' @param object HB model
#' @param ... other argument
#' @return plot
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
#' corn_model <- hb_unit(
#'    CornHec ~ SoyBeansPix + CornPix,
#'    data_unit = cornsoybean,
#'    data_area = Xarea,
#'    domain = "County",
#'    iter.update = 20,
#'    plot = FALSE
#' )
#' autoplot(corn_model)
#'
#' @export
autoplot.saehb <- function(object, ...){
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  graphics::par(mar = c(2, 2, 2, 2))
  coda::autocorr.plot(object$result_mcmc, col = "brown2", lwd = 2)
  plot(object$result_mcmc, col = "brown2", lwd = 2)
}

#' @inherit ggplot2::autoplot
#' @export
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}
