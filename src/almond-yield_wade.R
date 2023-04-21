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
library(tidyverse)
clim <- read_csv("src/clim.txt")
climate <- separate(clim, into = c("day", "month", "year", "wy", "tmax_c", "tmin_c", "precip"), col = "day month year wy tmax_c tmin_c precip", sep = " ")

head(climate)
climate$tmax_c <- as.numeric(climate$tmax_c)
climate$tmin_c <- as.numeric(climate$tmin_c)
climate$precip <- as.numeric(climate$precip)

climate_clean <- climate %>%
  filter(month == c(1, 2)) %>% 
  group_by(year, month) %>% 
  summarize(mean_min_temp = mean(tmin_c),
            mean_precip = mean(precip))

temp_feb <- climate_clean %>% 
  filter(month == 2) %>% 
  select(year, month, mean_min_temp)

precip_jan <- climate_clean %>% 
  filter(month == 1) %>% 
  select(year, month, mean_min_temp)

str(climate)

almond_yield <- function(temp_feb, precip_jan) {
  
  yield_anom <- (-0.015*temp_feb) + (-0.0046*(temp_feb^2)) + (-0.07*precip_jan) + (0.0043*(precip_jan^2)) + 0.28
  
  return(paste("Almond yield anomaly:", round(yield_anom, 2), "ton/acre"))
}

almond_yield(temp_feb = temp_feb$mean_min_temp, precip_jan = precip_jan$mean_min_temp)



a <- precip_jan$mean_min_temp
b <- temp_feb$mean_min_temp
yield_anom <- (-0.015*b) + (-0.0046*(b^2)) + (-0.07*a) + (0.0043*(a^2)) + 0.28

