# Comparaison bayesienne de deux moyennes Gaussiennes
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Comparaison bayesienne de deux moyennes Gaussiennes
#'
#' Comparaison bayesienne de deux moyennes par methodes MCMC
#' 
#' @param X1,X2 Deux vecteurs Gaussiens 
#' @param n.iter Le nombre d'iterations de la cha?ne MCMC 
#' @param n.burnin Le nombre d'iterations a bruler de la chaine MCMC 
#' @param n.thin Le nombre d'iterations a filtrer de la chaine MCMC 
#' @param prior.norm Un vecteur de deux valeurs a priori sur les moyennes
#' @param prior.gamma Un vecteur de deux valeurs a priori sur les parametres de precision (Distribution Gamma)
#' @examples 
#' X1 <- rnorm(15,6,1)
#' X2 <- rnorm(10,5,.5)
#' B.mean(X1,X2)
#' @export


B.mean<-function(X1,X2,n.iter=11000,n.burnin=1000,n.thin=1,prior.norm=c(0,0.001,0,0.001),prior.gamma=c(0.5,0.5,0.5,0.5)){
#RES<-"en cours de modifs"
if(missing(X2)){
            
            RES<-B.moy1(X1   ,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin,prior.norm=prior.norm,prior.gamma=prior.gamma)
			nom1		           <-deparse(substitute(X1))
			rownames(RES$MCMC)     <-c(	paste("Moy",nom1),paste("P(",nom1,">0 )")[1],"Tau","Deviance")
			rownames(RES$Stats)    <-c(nom1) 
            cat("\n")
            cat("Estimation MCMC d'une moyenne ( Distribution Gaussienne ) \n")
            cat("Prior specifie: norm(" ,prior.norm[1],",",prior.norm[2],")","\n")
            cat("\n")}else	{
            
            RES<-B.moy2(X1,X2,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin,prior.norm=prior.norm,prior.gamma=prior.gamma)
			nom1		     	   <-deparse(substitute(X1))                                                            
			nom2		     	   <-deparse(substitute(X2))
			rownames(RES$MCMC)	   <-c(	paste("Moy",nom1),paste("Moy",nom2),"difference",paste("P(",nom1,">",nom2,")")[1])
			rownames(RES$Stats)	   <-c(nom1,nom2)
            cat("\n")
            cat("Estimation MCMC de deux moyennes ( Distributions Gaussiennes ) \n")
            cat("Prior : norm(" ,prior.norm[1],",",prior.norm[2],")","&","norm(" ,prior.norm[3],",",prior.norm[4],")"    ,"\n")
            cat("\n")}
return(RES)}

