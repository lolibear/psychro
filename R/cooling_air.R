#' Function to get relative humidity/condense note after cooling
#'
#' @param T_pre Original Dry bulb temperature
#' @param RH_pre Original relative humidity
#' @param T_post Final Dry bulb temperature
#' @param temp_list The stored data from psychrometric chart

#' @examples
#' cooling_air(78, 45, 56, chart_data)
#' $RH
#' [1] 97

#' $note
#' [1] ""


cooling_air <- function(T_pre, RH_pre, T_post, temp_list) {
  get_humidity_ratio <- function(rh, temp){
    y <- approx(temp$RH, temp$HR, rh)$y
    return(y)
  }
  
  get_relative_humidity <- function(hr, temp){
    y <- approx(temp$HR, temp$RH, hr)$y
    return(round(y, digits = 0))
  }
  HR_pre <- get_humidity_ratio(RH_pre, temp_list[[toString(T_pre)]])
  HR_post_max <- max(temp_list[[toString(T_post)]]$HR)
  
  if(HR_post_max >= HR_pre) {
    RH_post <- get_relative_humidity(HR_pre, temp_list[[toString(T_post)]])
    des <- ""
  }else{
    RH_post <- 100
    des <- "The air has condensed at this point"
  }
  
  return(list(RH = RH_post, note = des))
}