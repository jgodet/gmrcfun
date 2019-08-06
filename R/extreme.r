# extreme.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description NE fonctionne pas avec valeurs extemement faibles?
#' @param 
#' @details 
#' @examples 
#' x <- rnorm(100, mean = 20, sd=2)
#' x[10] <- 100
#' extreme(x)
#' x[3] <- 3
#' extreme(x)
#' @return 
#' @export

extreme<-function (x, nlab = 2, labs = as.character(1:length(x)), ylab = "Sorted Data", ...) {
  
  if(!is.numeric(x)){print("Erreur: le vecteur n'est pas numerique")}else{
    
    
    
    
    #########	Creation du graphe	######################
    par(mfrow=c(1,2))
    hist(x,main="Histogramme")
    x2<-x
    x <- abs(x)
    labord <- order(x)
    x <- sort(x)
    i <- order(x)
    n <- length(x)
    ui <- qnorm((n + 1:n)/(2 * n + 1))
    plot(ui, x[i],main="Half-normal plot", xlab = "Half-normal quantiles", ylab = ylab, 
         ylim = c(0, max(x)), type = "n")
    if (nlab < n) 
      points(ui[1:(n - nlab)], x[i][1:(n - nlab)])
    text(ui[(n - nlab + 1):n], x[i][(n - nlab + 1):n], labs[labord][(n-nlab + 1):n])
    
    
    #########	Recup des valeurs extremes	###################
    
    val.extr<-as.numeric(labs[labord][(n-nlab + 1):n])
    VE<-cbind(val.extr,x[i][(n - nlab + 1):n])
    colnames(VE)=c("Num","Val")
    Moyenne.avec<-paste(" : ",round(mean(x,na.rm=TRUE),2),"( +-",round(sd(x,na.rm=TRUE)),")")
    Moyenne.sans<-paste(" : ",round(mean(x2[-val.extr],na.rm=TRUE),2),"( +-",round(sd(x2[-val.extr],na.rm=TRUE)),")")
    AUTRE<-rbind(Moyenne.avec,Moyenne.sans)
    
    Liste1<-list(v.ex=VE,desc=noquote(AUTRE))
    par(mfrow=c(1,1))
    return(Liste1)}
}
