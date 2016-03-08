#' Function to get relative humidity and dry.bulb t after mixing
#'
#' @param t1 Dry bulb temperature for first air flow
#' @param rh1 Relative humidity for first air flow
#' @param t2 Dry bulb temperature for second air flow
#' @param rh2 Relative humidity for second air flow
#' @param t1.percent Percent of first air flow in the mixed air
#' @param temp.list the stored data from psychrometric chart

#' @examples
#' MixAir(41, 50, 68, 60, 0.375, chart.data)
#' $temp
#' [1] 58

#' $rh
#' [1] 64


MixAir <- function(t1, rh1, t2, rh2, t1.percent, temp.list) {
  GetHumidityRatio <- function(rh, temp){
    y <- approx(temp$rh, temp$hr, rh)$y
    return(y)
  }
  
  GetRelativeHumidity <- function(hr, temp){
    y <- approx(temp$hr, temp$rh, hr)$y
    return(round(y, digits = 0))
  }
  hr1 <- GetHumidityRatio(rh1, temp.list[[toString(t1)]])
  hr2 <- GetHumidityRatio(rh2, temp.list[[toString(t2)]])
  
  hr.mix <- t1.percent*hr1 + (1 - t1.percent)*hr2
  t.mix <- round(t1.percent*t1 + (1 - t1.percent)*t2, digits = 0)
  
  rh.mix <- GetRelativeHumidity(hr.mix, temp.list[[toString(t.mix)]])
  
  return(list(temp = t.mix, rh = rh.mix))
}