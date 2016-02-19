#' Function to get relative humidity and dry_bulb T after mixing
#'
#' @param T1 Dry bulb temperature for first air flow
#' @param RH1 Relative humidity for first air flow
#' @param T2 Dry bulb temperature for second air flow
#' @param RH2 Relative humidity for second air flow
#' @param T1_percent Percent of first air flow in the mixed air
#' @param temp_list The stored data from psychrometric chart

#' @examples
#' mix_air(41, 50, 68, 60, 0.375, chart_data)
#' $Temp
#' [1] 58

#' $RH
#' [1] 64


mix_air <- function(T1, RH1, T2, RH2, T1_percent, temp_list) {
  get_humidity_ratio <- function(rh, temp){
    y <- approx(temp$RH, temp$HR, rh)$y
    return(y)
  }
  
  get_relative_humidity <- function(hr, temp){
    y <- approx(temp$HR, temp$RH, hr)$y
    return(round(y, digits = 0))
  }
  HR1 <- get_humidity_ratio(RH1, temp_list[[toString(T1)]])
  HR2 <- get_humidity_ratio(RH2, temp_list[[toString(T2)]])
  
  HR_mix <- T1_percent*HR1 + (1 - T1_percent)*HR2
  T_mix <- round(T1_percent*T1 + (1 - T1_percent)*T2, digits = 0)
  
  RH_mix <- get_relative_humidity(HR_mix, temp_list[[toString(T_mix)]])
  
  return(list(Temp = T_mix, RH = RH_mix))
}