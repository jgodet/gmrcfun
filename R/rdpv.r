# rdpv.r
# written by JuG
# August 05 2019


#' round  p-values
#' @author JuG
#' @description round p-values for printing output
#' @param 
#' @details 
#' @examples 
#' rdpv(.1)
#' rdpv(.0001)
#' rdpv(1e-27)
#' format.pval(c(0.1, 0.0001, 1e-27))
#' @return 
#' @export


rdpv<-function(x){
  if(!is.na(x)){
    if(x<0.01){res<-"<0.01"}
    if(x>=0.01){res<-round(x,3)}
  }
  return(res)
  }
