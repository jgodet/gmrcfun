# gamm.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' x1 <- rnorm(100)
#' x2 <-   rgamma(n=200, shape = 10, rate = 5)  
#' gamm(x)
#' gamm(x2)
#' @return 
#' @export


gamm<-function(x){

  if(!require(MASS)){install.packages('MASS')}
  library(MASS)
  if(!require(car)){install.packages('car')}
  library(car)
  fd	<-fitdistr(x[!is.na(x)],"gamma")
  shape.X	<-fd$estimate[1]
  rate.X 	<-fd$estimate[2]
  
  par(mfrow=c(2,2))
  
  hist(x,freq=FALSE,breaks=20)
  
  hist(x,freq=FALSE,breaks=20,border="white")
  lines(density(x[!is.na(x)],adjust=2),cex=2,col="red")
  curve(dgamma(x,shape.X,rate.X),0,50,xlab="x",ylab="Pdf",add=TRUE,col="blue")
  legend("topright", legend = c("Histogram", "Theorical Gamma"), col = c("red", "blue"), pch = 15, bty = "n", pt.cex = 2, cex = 0.8, text.col = "forestgreen",  inset = c(0.1, 0.1))
  
  qqPlot(x,distr="gamma",shape=shape.X)
  
  plot(c(0,0),col="white",bty="n",xaxt="n",yaxt="n",col.lab="white",main="Theorical Gamma : Max Lik")
  text(1.2,0.6,"Shape:",cex=1.5,col="blue")
  text(1.2,0.2  ,"Rate:",cex=1.5,col="blue")
  text(1.7,0.6,round(shape.X,2),cex=1.5,col="blue")
  text(1.7,0.2,round(rate.X,2),cex=1.5,col="blue")
  
  text(1.2,-0.5,"a:",cex=1.5,col="purple")
  text(1.2,-0.9  ,"b:",cex=1.5,col="purple")
  text(1.7,-0.5,round(shape.X,2),cex=1.5,col="purple")
  text(1.7,-0.9,round(1/rate.X,2),cex=1.5,col="purple")
  
  par(mfrow=c(1,1))
}
