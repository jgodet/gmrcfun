# gghist.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' x <- c(rnorm(n=10, mean=5), rnorm(n=10, mean=10))
#' grp <- rep(LETTERS[1:2], each=10)
#' gghist(x1 = x)
#' gghist(x1 = x, Groupe = grp)
#' @return 
#' @export


gghist<-function(x1,Groupe=NULL){
  
  if(!require(ggplot2)){install.packages('ggplot2')}
  library(ggplot2)
  
  if(is.null(Groupe)){
    nomx<-deparse(substitute(x1))
    ggplot(data.frame(cbind(x1)), aes(x=x1,fill=as.factor(rep(1,length(x1))),colour=1)) + 
      geom_histogram(aes(y = ..density..),breaks=pretty(range(x1,na.rm=TRUE), n = nclass.Sturges(x1), min.n = 1))+
      scale_fill_manual( values = c("dodgerblue4"))+
      theme(legend.position = "none")+xlab(nomx)
  }else{
    
    d2<-data.frame(x1,Groupe)
    ggplot(data=d2) +
      aes(x = x1, fill = Groupe) +
      geom_histogram(alpha = 1, position = "dodge",breaks=pretty(range(x1,na.rm=TRUE), n = nclass.Sturges(x1), min.n = 1) )
  }
}
