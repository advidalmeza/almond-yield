#' Computes profit for California almond production
#' @param anom_almond_yield_results Results from almond_yield() function
#' @param yield_per_acre California almond yield (in tons per acre); default is 1.02
#' @param price Price (in USD per ton); default is 3,520
#' @param acres Median almond farm area (in acres); default is 75
#' @param discount Discount rate; default is 0.12
#' @return Data frame with estimate of profit
#' 
#' @references 
#' Almond yield per acre and almond market price are 2021 values from the USDA's 2022 California Almond Forecast as part of the National Agricultural Statistics Service; median almond farm area determined from California Almond Grower Survey Report from Integrated Crop Pollination Project (2014-15)


compute_profit <- function(anom_almond_yield_results, years, yield_per_acre = 1.02, 
                           price = 3520, acres = 75, discount = 0.12) {
  
  # Create data frame 
  yield_df <- data.frame(anom = anom_almond_yield_results)
  
  # Add years column
  yield_df$year <- as.character(years)
  
  # Determine revenue for anomaly (USD = ton/acre * USD/ton * acre)
  yield_df$anom_revenue <- yield_df$anom * price * acres
  
  # Determine revenue (USD = ton/acre * USD/ton * acre)
  revenue <- yield_per_acre * price * acres
  
  # Determine net revenue (USD = anomaly revenue + revenue)
  yield_df$total_revenue <- yield_df$anom_revenue + revenue
  
  # remember to normalize the year to start year e.g the first year
  # yearprofit <- yearprofit %>% 
    # mutate(netpre = compute_NPV(value = net, time = year-year[1], discount = discount))
  
  return(yield_df)
  
}
