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
  
  return(list(RH_post, des))
}