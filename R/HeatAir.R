#' Function to get relative humidity after heating
#'
#' @param t.pre Original Dry bulb temperature
#' @param rh.pre Original relative humidity
#' @param t.post Final Dry bulb temperature
#' @param temp.list the stored data from psychrometric chart

#' @examples
#' HeatAir(45, 40, 85, data.chart)
#' [1] 10


HeatAir <- function(t.pre, rh.pre, t.post, temp.list) {
  GetHumidityRatio <- function(rh, temp){
    y <- approx(temp$rh, temp$hr, rh)$y
    return(y)
  }
  
  GetRelativeHumidity <- function(hr, temp){
    y <- approx(temp$hr, temp$rh, hr)$y
    return(round(y, digits = 0))
  }
  hr.pre <- GetHumidityRatio(rh.pre, temp.list[[toString(t.pre)]])
  
  rh.post <- GetRelativeHumidity(hr.pre, temp.list[[toString(t.post)]])
  
  return(rh.post)
}