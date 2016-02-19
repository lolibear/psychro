#' Function to get relative humidity after heating
#'
#' @param T_pre Original Dry bulb temperature
#' @param RH_pre Original relative humidity
#' @param T_post Final Dry bulb temperature
#' @param temp_list The stored data from psychrometric chart

#' @examples
#' heating_air(45, 40, 85, chart_data)
#' [1] 10


heating_air <- function(T_pre, RH_pre, T_post, temp_list) {
  get_humidity_ratio <- function(rh, temp){
    y <- approx(temp$RH, temp$HR, rh)$y
    return(y)
  }
  
  get_relative_humidity <- function(hr, temp){
    y <- approx(temp$HR, temp$RH, hr)$y
    return(round(y, digits = 0))
  }
  HR_pre <- get_humidity_ratio(RH_pre, temp_list[[toString(T_pre)]])
  
  RH_post <- get_relative_humidity(HR_pre, temp_list[[toString(T_post)]])
  
  return(RH_post)
}