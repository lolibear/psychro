#' Function to get relative humidity after heating
#'
#' @param t.pre Original Dry bulb temperature
#' @param rh.pre Original relative humidity
#' @param t.post Final Dry bulb temperature
#' @param temp.list the stored data from psychrometric chart

#' @examples
#' HeatOrCoolAir(78, 45, 56, data.chart)
#' $rh
#' [1] 97

#' $note
#' [1] ""

HeatOrCoolAir <- function(t.pre, rh.pre, t.post, temp.list) {
  GetHumidityRatio <- function(rh, temp){
    y <- approx(temp$rh, temp$hr, rh)$y
    return(y)
  }
  
  GetRelativeHumidity <- function(hr, temp){
    y <- approx(temp$hr, temp$rh, hr)$y
    if(is.na(y)) y <- max(temp$rh)
    return(round(y, digits = 2))
  }

  floor.t.pre <- floor(t.pre)
  ceiling.t.pre <- ceiling(t.pre)
  
  floor.t.post <- floor(t.post)
  ceiling.t.post <- ceiling(t.post)
  
  hr.pre <- GetHumidityRatio(rh.pre, temp.list[[toString(ceiling.t.pre)]]) - 
    (GetHumidityRatio(rh.pre, temp.list[[toString(ceiling.t.pre)]]) - GetHumidityRatio(rh.pre, temp.list[[toString(floor.t.pre)]]))*
    (ceiling.t.pre - t.pre)
  
  hr.post.max <- max(temp.list[[toString(ceiling.t.post)]]$hr) - (max(temp.list[[toString(ceiling.t.post)]]$hr) - max(temp.list[[toString(floor.t.post)]]$hr))*(ceiling.t.post - t.post)

  if(hr.post.max >= hr.pre) {
    rh.post <- GetRelativeHumidity(hr.pre, temp.list[[toString(ceiling.t.post)]]) - 
      (GetRelativeHumidity(hr.pre, temp.list[[toString(ceiling.t.post)]]) - GetRelativeHumidity(hr.pre, temp.list[[toString(floor.t.post)]]))*
      (ceiling.t.post - t.post)
    des <- ""
  }else{
    rh.post <- 100
    des <- "the air has condensed at this point"
  }
  
  return(list(rh.post = rh.post, des = des))
}