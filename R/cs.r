# cs.r
# written by JuG
# August 05 2019


#' Scale and reduce data
#' @authors Michael Schaeffer; JuG
#' @description  Scale and reduce data. Does exactly the same as scale but slightly faster.
#' @param x a numeric vector to be scaled and reduced
#' @details 
#' @examples 
#' x <-rnorm(n = 100, mean = 50, sd = 10)
#' xreduced <- cs(x)
#' c(mean(xreduced), sd(xreduced))
#' xscaled <- scale(x)
#' c(mean(xscaled), sd(xscaled))
#' 
#' x <-rnorm(n = 1e8, mean = 50, sd = 10)
#' system.time(c(mean(cs(x)), sd(cs(x))))
#' system.time(c(mean(scale(x)), sd(scale(x))))
#' @return 
#' @export


cs<-function(x){
  return((x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE))
  }
