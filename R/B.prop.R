# Comparaison Bayesienne de deux proportions
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Comparaison Bayesienne de deux proportions par methodes MCMC
#'
#' Comparaison Bayesienne de deux proportions
#' 
#' @param X1,X2 Deux vecteurs de donnees binaires codees 0/1 
#' @param n.iter Le nombre d'iterations de la chaine MCMC 
#' @param n.burnin Le nombre d'iterations a bruler de la cha?ne MCMC 
#' @param n.thin Le nombre d'iterations a filtrer de la chaine MCMC 
#' @param prior.beta Un vecteur (a,b,c,d) de deux valeurs a priori sur les distributions Beta (a,b) et (c,d)
#' @examples 
#' X1 <- rbinom(n = 10,size = 1,prob = .4)
#' X2 <- rbinom(n = 12,size = 1,prob = .6)
#' B.prop(X1,X2)
#' @export




B.prop<-function(X1,X2,n.iter=11000,n.burnin=1000,n.thin=1,prior.beta=c(1,1,1,1)){

cat("\n");cat("\n");Stop=0
                                                                                                                        ### ### ### De quoi afficher tous les messages d'erreur.
if(nlevels(as.factor(X1))==1){cat("Erreur : La variable est constante sur une modalite	     \n");Stop=0}else{
if(nlevels(as.factor(X1)) >2){cat("Erreur : Fonction utilisable uniquement pour des vecteurs binaires\n");Stop=1}else{
if(all(levels(as.factor(X1))==c("0","1"))){nul=0}else{cat("Erreur : La variable doit etre codee 0 / 1\n");Stop=1}}}
if(!missing(X2)){
if(nlevels(as.factor(X2))==1){cat("Erreur : La variable 2 est constante sur une modalite	     \n");Stop=0}else{
if(nlevels(as.factor(X2)) >2){cat("Erreur : Fonction utilisable uniquement pour des vecteurs binaires\n");Stop=1}else{
if(all(levels(as.factor(X2))==c("0","1"))){nul=0}else{cat("Erreur : La variable 2 doit etre codee 0 / 1\n");Stop=1}}}
}
if(Stop!=1){
if(missing(X2)){
            RES                    <-B.prop1(X1   ,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin,prior.beta=prior.beta)
			nom1		           <-deparse(substitute(X1))
			rownames(RES$MCMC)     <-c(	paste("Prop",nom1),paste("P(",nom1,">0.5 )")[1])
			rownames(RES$Stats)    <-c(nom1) 
            cat("\n")
            cat("Estimation MCMC d'une proportion ( Distribution Bernoulli ) \n")
            cat("Prior specifie: beta(" ,prior.beta[1],",",prior.beta[2],")","\n")
            cat("\n")}else	{

            RES<-B.prop2(X1,X2,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin,prior.beta=prior.beta)
			nom1		     	   <-deparse(substitute(X1))                                                            
			nom2		     	   <-deparse(substitute(X2))
			rownames(RES$MCMC)	   <-c(	paste("Prop",nom1),paste("Prop",nom2),"difference",paste("P(",nom1,">",nom2,")")[1],"Odds.ratio")
			rownames(RES$Stats)	   <-c(nom1,nom2)
            cat("\n")
            cat("Estimation MCMC de deux proportions ( Distributions Bernoulli ) \n")
            cat("Prior : beta(" ,prior.beta[1],",",prior.beta[2],")","&","beta(" ,prior.beta[3],",",prior.beta[4],")"    ,"\n")            
            cat("\n")}}
return(RES)}

