# correl.r
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
#' z <- 2 * x + rnorm(n=20, sd=2)
#' correl(x,y)
#' correl(x,z)
#' @return 
#' @export


correl<-function(x,y,droite=1, nomx=NULL , nomy = NULL){
  
  
  options(warn=-1)
  if(!require(ggplot2))install.packages('ggplot2'); library(ggplot2)
  if(length(x)==length(y)){
      r<-rbind(       c(round(cor.test(x,y,method = "kendall") $estimate,3),cor.test(x,y,method = "kendall") $p.value),
                      c(round(cor.test(x,y,method = "pearson" )$estimate,3),cor.test(x,y,method = "pearson") $p.value),
                      c(round(cor.test(x,y,method = "spearman")$estimate,3),cor.test(x,y,method = "spearman")$p.value))
      
      r2                      <-cbind(r,ifelse(r[,2]<0.05,"H1","H0"))
      r2[1,2]         <-rdpv(r[1,2])
      r2[2,2]         <-rdpv(r[2,2])
      r2[3,2]         <-rdpv(r[3,2])
      rownames(r2)<-c("kendall","pearson","spearman")
      colnames(r2)<-c("Tau","P.val exacte","Hyp")
      DDD                     <-data.frame(cbind(y,x))
      c                       <- ggplot(DDD, aes(x, y))
      #x11()
      print(noquote(r2))
      if(is.null(nomx)){nomx<-deparse(substitute(x))}
      if(is.null(nomy)){nomy<-deparse(substitute(y))}
      if(droite){        
        return(c + stat_smooth(method = "lm") + geom_point()+ xlab(nomx)+ylab(nomy)) 
      }else{    
         return(c +geom_point()+ xlab(nomx)+ylab(nomy)) 
      }
  }else{   return("Les longueurs des vecteurs ne sont pas egales")
     }
  options(warn=0)                 
}
