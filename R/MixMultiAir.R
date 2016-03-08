#' Function to get relative humidity and dry.bulb t after multipul mixing
#'
#' @param vec.t A vector of Dry bulb temperatures
#' @param vec.rh A vector of relative humidity corresponding to the vec.t
#' @param temp.list the stored data from psychrometric chart

#' @examples
#' vec.t <- c(40,50,60,70,80)
#' vec.rh <- c(55,60,75,80,85)
#' mix.multi.air(vec.t, vec.rh, temp.list)
#' $temp
#' [1] 60

#' $rh
#' [1] 85


MixMultiAir <- function(vec.t, vec.rh, temp.list) {
  
  length <- length(vec.t)
  fix.p <- 1/length
  initial.p <- fix.p
  
  while(length > 1){
    t1 <- vec.t[[1]]
    rh1 <- vec.rh[[1]]
    
    t2 <- vec.t[[2]]
    rh2 <- vec.rh[[2]]
    
    t1.percent <- initial.p/(initial.p + fix.p)
    
    mix <- MixAir(t1, rh1, t2, rh2, t1.percent, temp.list)
    t.mix <- mix$temp
    rh.mix <- mix$rh
    
    vec.t <- vec.t[-c(1,2)]
    vec.rh <- vec.rh[-c(1,2)]
    vec.t <- append(vec.t, t.mix, 0)
    vec.rh <- append(vec.rh, rh.mix, 0)
    
    length <- length - 1
    initial.p <- initial.p + fix.p
    
  }
  
  return(list(temp = as.numeric(vec.t), rh = as.numeric(vec.rh)))
  
}