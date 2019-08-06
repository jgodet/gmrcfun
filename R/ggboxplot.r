# ggboxplot.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' x <- rnorm(n=20, mean=10)
#' y <- rep(LETTERS[1:2], each=20)
#' grp <- rep(LETTERS[3:4], 20)
#' ggboxplot( x=x, y=y)
#' ggboxplot( x=x, y=y, Groupe = grp)
#' @return 
#' @export



ggboxplot<-function(x,y,Groupe=NULL){
  
  if(!require(ggplot2)){install.packages('ggplot2')}
  library(ggplot2)
  
  nomx<-deparse(substitute(x))
  nomy<-deparse(substitute(y))
  if(is.null(Groupe)){
    DDD<-data.frame(x,y)
  }else{
    DDD<-data.frame(x,y,Groupe)
  }
  DDD<-DDD[complete.cases(DDD),]

  if(is.null(Groupe)){
    p <- ggplot(DDD,aes(factor(y),x))+xlab(nomy)+ylab(nomx)
    }else{
      p <- ggplot(DDD,aes(factor(y),x,fill=factor(Groupe)))+xlab(nomy)+ylab(nomx)
    }
  titre<-paste(nomx,"en fonction de",nomy)
  return(p + geom_boxplot()  + ggtitle(titre)+ scale_x_discrete(labels=levels(y)))
}
