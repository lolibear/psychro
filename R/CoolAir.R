#' Function to get relative humidity/condense note after cooling
#'
#' @param t.pre Original Dry bulb temperature
#' @param rh.pre Original relative humidity
#' @param t.post Final Dry bulb temperature
#' @param temp.list the stored data from psychrometric chart

#' @examples
#' CoolAir(78, 45, 56, data.chart)
#' $rh
#' [1] 97

#' $note
#' [1] ""


CoolAir <- function(t.pre, rh.pre, t.post, temp.list) {
  GetHumidityRatio <- function(rh, temp){
    y <- approx(temp$rh, temp$hr, rh)$y
    return(y)
  }
  
  GetRelativeHumidity <- function(hr, temp){
    y <- approx(temp$hr, temp$rh, hr)$y
    return(round(y, digits = 0))
  }
  hr.pre <- GetHumidityRatio(rh.pre, temp.list[[toString(t.pre)]])
  hr.post.max <- max(temp.list[[toString(t.post)]]$hr)
  
  if(hr.post.max >= hr.pre) {
    rh.post <- GetRelativeHumidity(hr.pre, temp.list[[toString(t.post)]])
    des <- ""
  }else{
    rh.post <- 100
    des <- "the air has condensed at this point"
  }
  
  return(list(rh = rh.post, note = des))
}