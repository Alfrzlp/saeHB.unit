#' Summary sae HB model
#'
#' @param object sae HB model
#' @param ... other arguments
#'
#' @export
#'
#' @examples
summary.saehb <- function(object, ...){
  stats::printCoefmat(object$coefficient)
}
