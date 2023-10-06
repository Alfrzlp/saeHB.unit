#' cornsoybean
#' @name cornsoybean
#' @title Corn and soy beans survey and satellite data in 12 counties in Iowa
#' @description Survey and satellite data for corn and soy beans in 12 Iowa counties, obtained from the 1978 June Enumerative Survey of the U.S. Department of Agriculture and from land observatory satellites (LANDSAT) during the 1978 growing season.
#' @format A data frame with 37 observations on the following 5 variables.
#' \describe{
#'    \item{\code{County}:}{ numeric county code.}
#'    \item{\code{CornHec}:}{ reported hectares of corn from the survey.}
#'    \item{\code{SoyBeansHec}:}{ reported hectares of soy beans from the survey.}
#'    \item{\code{CornPix}:}{ number of pixels of corn in sample segment within county, from satellite data. }
#'    \item{\code{SoyBeansPix}:}{ number of pixels of soy beans in sample segment within county, from satellite data.}
#' }
#' @source Battesse, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components Model for Prediction of County Crop Areas Using Survey and Satellite Data. Journal of the American Statistical Association 83, 28-36.
"cornsoybean"

#' cornsoybeanmeans
#' @name cornsoybeanmeans
#' @title Corn and soy beans mean number of pixels per segment for 12 counties in Iowa.
#' @description County means of number of pixels per segment of corn and soy beans, from satellite data, for 12 counties in Iowa. Population size, sample size and means of auxiliary variables in data set \code{\link{cornsoybean}}.
#' @format
#' A data frame with 12 observations on the following 6 variables.
#' \describe{
#'      \item{\code{CountyIndex}:}{ numeric county code.}
#'      \item{\code{CountyName}:}{ name of the county.}
#'      \item{\code{SampSegments}:}{ number of sample segments in the county (sample size).}
#'      \item{\code{PopnSegments}:}{ number of population segments in the county (population size).}
#'      \item{\code{MeanCornPixPerSeg}:}{ mean number of corn pixels per segment in the county.}
#'      \item{\code{MeanSoyBeansPixPerSeg}:}{ mean number of soy beans pixels per segment in the county.}
#'  }
#' @source Battesse, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components Model for Prediction of County Crop Areas Using Survey and Satellite Data. Journal of the American Statistical Association 83, 28-36.
"cornsoybeanmeans"

#' dummy_unit
#' @name dummy_unit
#' @title dummy_unit
#' @description dummy data
#' @format A data frame with 1000 observations on the following 4 variables.
#' \describe{
#'    \item{\code{domain}:}{domain code}
#'    \item{\code{y_di}:}{direct estimate of y}
#'    \item{\code{x1}:}{x1}
#'    \item{\code{x2}:}{x2}
#' }
"dummy_unit"

#' dummy_area
#' @name dummy_area
#' @title dummy_area
#' @description dummy data
#' @format A data frame with 30 observations on the following 4 variables.
#' \describe{
#'    \item{\code{domain}:}{domain code}
#'    \item{\code{x1}:}{x1}
#'    \item{\code{x2}:}{x2}
#'    \item{\code{parameter}:}{ true value of y }
#' }
"dummy_area"
