# Comparaison Bayesienne de deux moyennes de comptage 
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Comparaison Bayesienne de deux moyennes de comptage 
#'
#' Comparaison Bayesienne de deux moyennes de comptage (issus de distributions de Poisson)
#' 
#' @param X1,X2 Deux vecteurs de donnees de Comptage 
#' @param n.iter Le nombre d'iterations de la chaine MCMC 
#' @param n.burnin Le nombre d'iterations a bruler de la cha?ne MCMC 
#' @param n.thin Le nombre d'iterations a filtrer de la chaine MCMC 
#' @param prior.gamma Un vecteur de deux valeurs ? priori sur les parametres de la distribution Gamma pour les Lambda (Disutribution Poisson)
#' @examples 
#' X1 <- rpois(n = 25, lambda = 2)
#' X2 <- rpois(n = 25, lambda = 2)
#' B.pois(X1,X2)
#' @export






B.pois<-function(X1,X2,n.iter=11000,n.burnin=1000,n.thin=1,prior.gamma=c(0.1,0.1,0.1,0.1)){

cat("\n");cat("\n");Stop=0
                                                                                                                        ### ### ### De quoi afficher tous les messages d'erreur.

if(Stop!=1){
if(missing(X2)){
            RES<-B.pois1(X1   ,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin,prior.gamma=prior.gamma)
			nom1		           <-deparse(substitute(X1))
			rownames(RES$MCMC)     <-c(paste("Moy",nom1),"deviance")
			rownames(RES$Stats)    <-c(nom1)
            cat("\n")
            cat("Estimation MCMC d'une moyenne de comptages ( Distribution de Poisson ) \n")
            cat("Prior specifie: gamma(" ,prior.gamma[1],",",prior.gamma[2],")","\n") 
            cat("\n")}else	{
            
            RES<-B.pois2(X1,X2,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin,prior.gamma=prior.gamma)
			nom1		     	   <-deparse(substitute(X1))                                                            
			nom2		     	   <-deparse(substitute(X2))
			rownames(RES$MCMC)	   <-c(	paste("Moy",nom1),paste("Moy",nom2),"difference",paste("P(",nom1,">",nom2,")")[1])
			rownames(RES$Stats)	   <-c(nom1,nom2)
            cat("\n")
            cat("Estimation MCMC de deux moyennes de comptages ( Distributions de Poisson ) \n")
            cat("Prior : gamma(" ,prior.gamma[1],",",prior.gamma[2],")","&","gamma(" ,prior.gamma[3],",",prior.gamma[4],")"    ,"\n")
            cat("\n")}
return(RES)}}



