# B.cor.r
#Calculer une coefficient de correlation lineaire bayesien
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Calculer une coefficient de correlation lineaire bayesien
#'
#' @description Calculer une coefficient de correlation lineaire bayesien
#' 
#' @param x,y Deux vecteurs de donnees quantitatives a correler
#' @param droite si egal a 1, la droite de correlation est ajoutee au graphique sinon seul le nuage de points est presente
#' @param Niter Le nombre d'iterations de la chaine MCMC
#' @param Nburn Le nombre d'iterations a bruler de la chaine MCMC
#' @param message 
#' 
#' @examples 
#' x <- rnorm(30, 20, 5)
#' y <- 2 * x + rnorm(30, 0,10)
#' B.cor(x,y)
#' @export


B.cor<-function(x,y,droite=1,Niter=55000,Nburn=5000,message = TRUE){

Dxy<-data.frame(x,y)
Dxy<-Dxy[complete.cases(Dxy),]
x<-Dxy$x
y<-Dxy$y

OK=0
if(length(x)==length(y)){OK<-1}
if(OK==1){
  if(message){
    cat("\n ################################### \n" )
    cat(" Coefficient de correlation lineaire " )
    cat("\n ################################### \n" )
    cat("\n")
    cat("NB: Les couples de donnees manquantes ont ete retirees\n")
    cat("\n")
    cat("NB: L'information a priori est vague\n")
    cat("\n")
    cat("NB: Un graphique a ete cree\n")
    cat("\n")
    cat("\n _________________________________ \n" )
    cat("\n")
  }
options(warn=-1)   
library(R2jags)
modelH<-function(){

	for (i in 1:N) {	V[i, 1:M] ~ dmnorm(mu[], Omega[ , ])  }	
	
				Omega[1:M , 1:M]  ~ dwish(R[ , ], 2)		
				Sigma[1:M , 1:M] <- inverse(Omega[ , ])				  	 	

				Rho <-Sigma[1,2]/sqrt(Sigma[1,1]*Sigma[2,2])
				Pr<-step(Rho)
				}
						
####       DATAS      ########

			
donnees<-list(M = 2, 
	mu= c(mean(x,na.rm=TRUE),mean(y,na.rm=TRUE)),
	N = length(x), 
	V = cbind(x,y),
	R = diag(2))
	
	parametre       <-c("Rho","Pr")
	

	
modele  <-jags( data                   	=donnees,
                inits                   =NULL,
                parameters.to.save      =parametre,
                model.file              = modelH,
                n.chains                =1,
                n.iter                  =Niter,
                n.burnin                =Nburn,
                n.thin                  =1,
				progress.bar			="text")
				
par(mfrow=c(1,2))				
for(i in 2:(length(modele$BUGSoutput$mean)-1)){hist(modele$BUGSoutput$sims.list[[i]],breaks=50,xlab="Coefficient de correlation",main=names(modele$BUGSoutput$mean)[i])}
abline(v=0,lwd=4,col="red")
abline(v=-1,lwd=4,col="red")
abline(v=1,lwd=4,col="red")
abline(v=modele$BUGSoutput$summary[1,1],lwd=3,col="blue",lty=3)
abline(v=modele$BUGSoutput$summary[1,3],lwd=3,col="blue",lty=3)
abline(v=modele$BUGSoutput$summary[1,7],lwd=4,col="blue",lty=3)
plot(y~x,pch=16)
mx <- mean(x)	
my <- mean(y)
vx <- var(x)
vy <- var(y)
slopes <- modele$BUGSoutput$sims.list$Rho * sqrt(vy / vx)
intercepts <- my - slopes*mx

for (i in sample(size=1000,1:(Niter-Nburn))){
  abline(b=slopes[i],a=intercepts[i], col=rgb(0,0,1,.02))
} 
points(y~x,pch=16)
abline(lm(y~x),col="red",lwd=3)
options(warn=0)                 		
return(modele$BUGSoutput$summary)
}else{
print("Erreur, les deux vecteurs n'ont pas la meme longueur")}
}
	





