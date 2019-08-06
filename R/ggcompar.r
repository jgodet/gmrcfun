# ggcompar.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param x a string corresponding to the name of the column in the data frame
#' @param y a string corresponding to the name of the column in the data frame
#' @param DDD a data frame
#' @details 
#' @examples 
#' x <- c(rnorm(n=30, mean=5), rnorm(n=30, mean=10))
#' grp <- rep(LETTERS[1:2], each=30)
#' dtf <- data.frame(x, grp)
#' ggcompar(x = "x", y = "grp", DDD = dtf)
#'
#' @return 
#' @export


ggcompar<-function(x,y, DDD= NULL){
  if(!require(ggplot2)){install.packages('ggplot2')}
  library(ggplot2)
  
  if( is.null(DDD)) {  DDD<-as.data.frame(cbind(x,y)) 
  }else{ DDD<- DDD[,c(x,y)]}
  titre<-paste("Distributions de",x,"en fonction de",y)
  DDD[,2]<- as.factor(DDD[,2])
  return(
    ggplot(DDD,aes(x=DDD[,1], fill=DDD[,2])) + geom_density(alpha=.3)+ ggtitle(titre)+xlab(x)+   guides(fill = guide_legend(title = y))
  )
}
