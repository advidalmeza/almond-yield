#' Almond Yield 
#'
#' @param temp_feb Minimum temperature in February (in degree Celsius) 
#' @param precip_jan Precipitation in January (in millimeter)
#'
#' @return Almond yield anomaly (in ton per acre)
#' @export
#'
#' @examples
#' 

almond_yield <- function(temp_feb, precip_jan) {
  
  yield_anom <- (-0.015*temp_feb) + (-0.0046*(temp_feb^2)) + (-0.07*precip_jan) + (0.0043*(precip_jan^2)) + 0.28
  
  return(paste("Almond yield anomaly:", yield_anom, "ton/acre"))
}