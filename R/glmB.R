# Ajustement MCMC d'un modele lineaire generalise, eventuellement mixte
# Date : 2015-06-15
# Original function : Nicolas Meyer
# Refactored by : Mickael Schaeffer - JuG
#

#' Ajuster un modele de type GLM ou GLMER par MCMC 
#'
#' @description Fonction permettant d'ajuster un modele lineaire generalise en bayesien a l'aide de methodes MCMC
#' 
#' @param formula une formule de predicteur lineaire, eventuellement mixte (+1|sujet)
#' @param data la base de donnees a utiliser
#' @param family la famille du GLM (gaussian,binomial,poisson,gamma et beta(NON GLM))
#' @param Niter Le nombre d'iterations souhaite pour la chaine MCMC
#' @param Nburnin Le nombre d'iterations a bruler pour la chaine MCMC
#' @param Nthin Le nombre d'iterations a filtrer pour la chaine MCMC
#' @param Prior Une matice de k colonnes et 2 lignes, de moyennes et precisions (1/var) a priori (pour chaque coef)
#' @param Seuilmonit Un vecteur de x elements, (x parametres), si on souhaite monitorer la Proba d'?tre sup au seuil
#' @param Gammaalea Un vecteur de 2 elements, moyennes et variances a priori de l'effet aleatoire
#' @param diagno Uniquement pour le modele logistique(et mixte), si on souhaite le detail des diagnostiques
#' @param dig Nombre de decimales pour la presentation des resultats
#' @param Graph TRUE par defaut, graphiques des distributions a posteriori
#' @param Alea FALSE par defaut. Si TRUE: monitore et affiche les effets aleatoires
#' 
#' @examples
#' dtf <- data.frame(Y = rbinom(n = 60 ,size=1,prob = .3), X = rnorm(60, 10,2) )
#' glmB(Y ~ X, data= dtf, family = "binomial")
#' 
#' @export

 glmB<-function(formula,data=NULL,family="gaussian",Niter=105000,Nburnin=5000,Nthin=5,Prior=NULL,Seuilmonit=NULL,Gammaalea=c(0.1,0.1),Diagno=TRUE,logiciel="jags",dig=3,Graph=TRUE,Alea=FALSE){
  library(lme4)
   logiciel = "jags"
 if(!is.element('R2jags', installed.packages()[,1]))  {install.packages('R2jags')}
 
 modeleALEATOIRE	<-sum(grepl("\\|",as.character(formula)))
 famillesDISPO		<-c("binomial","gaussian","poisson","beta","gamma")
 
 if(logiciel=="jags" & family=="binomial" 	& modeleALEATOIRE==0)	{RES<-glmB_logit_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Diagno,Graph=Graph)}
 if(logiciel=="jags" & family=="gaussian"	& modeleALEATOIRE==0)	{RES<-glmB_normal_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Graph=Graph)}
 if(logiciel=="jags" & family=="poisson" 	& modeleALEATOIRE==0)	{RES<-glmB_poisson_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Graph=Graph)}
 if(logiciel=="jags" & family=="beta" 		& modeleALEATOIRE==0)	{RES<-glmB_beta_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Graph=Graph)}
 if(logiciel=="jags" & family=="gamma" 		& modeleALEATOIRE==0)	{RES<-glmB_gamma_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Graph=Graph)}
 
 if(logiciel=="jags" & family=="binomial" 	& modeleALEATOIRE==1)	{RES<-glmerB_logit_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Gammaalea,Diagno,Graph=Graph,Alea=Alea)}
 if(logiciel=="jags" & family=="gaussian"   & modeleALEATOIRE==1)	{RES<-glmerB_normal_JAGS(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Gammaalea,Graph=Graph,Alea=Alea)}
 if(logiciel=="jags" & family=="poisson"    & modeleALEATOIRE==1)	{RES<-glmerB_poisson_JAGS(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Gammaalea,Graph=Graph,Alea=Alea)}
 if(logiciel=="jags" & family=="beta"   	& modeleALEATOIRE==1)	{RES<-glmerB_beta_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Gammaalea,Graph=Graph,Alea=Alea)}
 if(logiciel=="jags" & family=="gamma"   	& modeleALEATOIRE==1)	{RES<-glmerB_gamma_JAGS	(formula,data=data,Niter,Nburnin,Nthin,Prior,Seuilmonit,Gammaalea,Graph=Graph,Alea=Alea)}

 if(!is.element(family,famillesDISPO)){RES<-"La famille specifiee doit appartenir a l'ensemble :{binomial,gaussian,poisson,beta,gamma}"}
 RES$Resultats<-round(RES$Resultats,dig)
 return(RES)
 }
