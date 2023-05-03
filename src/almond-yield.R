#' Compute Almond Yield Anomaly
#'
#' @param temp_feb Minimum temperature in February (in degree Celsius) 
#' @param precip_jan Precipitation in January (in millimeter)
#' @param param_1 Coefficient for temp_feb 
#' @param param_2 Coefficient for temp_feb squared
#' @param param_3 Coefficient for precip_jan
#' @param param_4 Coefficient for precip_jan squared
#'
#' @return Almond yield anomaly (in ton per acre)
#'
#' @examples
#' almond_yield(temp_feb = 10.7, precip_jan = 0.09)

almond_yield <- function(temp_feb, precip_jan, param_1 = -0.015,
                         param_2 = -0.0046, param_3 = -0.07, param_4 = 0.0043) {
  
  yield_anom <- (param_1*temp_feb) + (param_2*(temp_feb^2)) + 
    (param_3*precip_jan) + (param_4*(precip_jan^2)) + 0.28
  
  # return(paste("Almond yield anomaly:", yield_anom, "ton/acre"))
  return(yield_anom)
  
}
