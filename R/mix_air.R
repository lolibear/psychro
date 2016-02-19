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