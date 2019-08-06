# ggpoints.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' x <- rnorm(n=20, mean=10)
#' y <- rnorm(n=20, mean=15)
#' ggpoints(x,y, droite = T, nomx = "X-axis")
#'
#' @return 
#' @export


ggpoints<-function(x,y,droite=FALSE,nomx= NULL,nomy=NULL){
  
  if(!require(ggplot2)){install.packages('ggplot2')}
  library(ggplot2)
  
  options(warn=-1)
  if(is.null(nomx)){nomx<-deparse(substitute(x))}
  if( is.null(nomy) ){nomy<-deparse(substitute(y))}

  if(length(x)==length(y)){
    DDD                     <- data.frame(cbind(y,x))
    c                       <- ggplot(DDD, aes(x, y))
    #x11()
    if(droite){        
      return(c + stat_smooth(method = "lm") + geom_point() +xlab(nomx)+ylab(nomy)) 
    }else{  	
      return(c +  geom_point() +xlab(nomx)+ylab(nomy)) 
      }
  }else{   
    return("Les longueurs des vecteurs ne sont pas egales")
  }
  options(warn=0)                 
}
