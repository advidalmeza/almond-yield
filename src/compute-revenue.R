#' Computes Revenue for Almond Production
#' 
#' @param anom_almond_yield_results Results from almond_yield() function
#' @param years Vector of years for results from almond_yield() function
#' @param yield_per_acre Almond yield (in tons per acre); default is 1.02 tons per acre
#' @param price Price (in USD per ton); default is 3,520 USD
#' @param acres Median almond farm area (in acres); default is 75 acres
#' @return Data frame with estimate of profit
#' 
#' @references 
#' Almond yield per acre and almond market price are 2021 values from the USDA's 2022 California Almond Forecast as part of the National Agricultural Statistics Service; median almond farm area determined from California Almond Grower Survey Report from Integrated Crop Pollination Project (2014-15)


compute_revenue <- function(anom_almond_yield_results, years, yield_per_acre = 1.02, 
                           price = 3520, acres = 75) {
  
  # Create data frame 
  yield_df <- data.frame(anom = anom_almond_yield_results)
  
  # Add years column
  yield_df$year <- as.character(years)
  
  # Add revenue for anomaly (USD = ton/acre * USD/ton * acre) to data frame
  yield_df$anom_revenue <- yield_df$anom * price * acres
  
  # Set baseline revenue (USD = ton/acre * USD/ton * acre)
  revenue <- yield_per_acre * price * acres
  
  # Add total revenue (USD = anomaly revenue + baseline revenue) to data frame
  yield_df$total_revenue <- yield_df$anom_revenue + revenue
  
  return(yield_df)
  
}
