# Utile pour B.pois
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer -JuG
#

#' Utile pour B.pois
#'
#' Utile pour B.pois
#' 
#' @param A definir

# avec inference bayesienne (en choisissant un alpha et un beta par niveaux)


B.pois1<-function(var1,n.iter=11000,n.burnin=1000,n.thin=1,prior.gamma=c(0.1,0.1,0.1,0.1)){ 
library(R2jags)
#--------------------------------------------------------------------------------------------------         
modelH<-function(){
		for (i in 1:N1) {	X[i] ~ dpois(lambda.x)	}								 			
				lambda.x~dgamma(a,b) 	 						    	
}
#--------------------------------------------------------------------------------------------------


donnees<-list(	 N1				=length(var1),
			     X				=dput(as.numeric(var1),		control=NULL),
			     a				=prior.gamma[1],
			     b				=prior.gamma[2] )

parametre	 <-c("lambda.x")                                          
modele	     <-jags( data				=donnees,
			         inits				=NULL,
			         parameters.to.save	=parametre,	
			         model.file			=modelH,
			         n.chains			=1,
			         n.iter			    =n.iter,
			         n.burnin			=n.burnin,
			         n.thin			    =n.thin,
					progress.bar			="text")

nom1		     <-deparse(substitute(var1))                                                           
indexDevi		 <-grep  ("deviance",rownames(modele$BUGSoutput$summary))
indexMux		 <-grep  ("lambda.x",rownames(modele$BUGSoutput$summary))

RES				 <-modele$BUGSoutput$summary[c(indexMux,indexDevi),]
moyennes	     <-round(RES[,1]  ,3)
variances	     <-round(RES[,2]^2,3)
sd		         <-round(RES[,2],3)
q0.025	         <-round(RES[,3],3)
q0.250	         <-round(RES[,4],3)
q0.500	         <-round(RES[,5],3)
q0.750	         <-round(RES[,6],3)
q0.975	         <-round(RES[,7],3)
RES              <-cbind(moyennes,variances,sd,q0.025,q0.250,q0.500,q0.750,q0.975)
rownames(RES)    <-c(paste("Prop",nom1),"dev")  
RES              <-noquote(RES)
colnames(RES)    <-c("Moy","Sd^2","Sd","2.5%","25%","50%","75%","97.5%")
var11			 <-var1
var1             <-var1[!is.na(var1)]
min1             <-min(var1)
max1             <-max(var1)
ete1	         <-max(var1)-min(var1)
moy1		     <-round(mean(var1),3)
NA1              <-round(sum(is.na(var11)),3)
pNA1             <-round(NA1/length(var11),3)*100
N1               <-length(var11)
RES1             <-noquote(cbind(c(N1),c(NA1),c(pNA1),c(min1),c(max1),c(ete1),c(moy1)))
colnames(RES1)   <-c("N","NA","%NA","min","max","etend","moy")
rownames(RES1)   <-c(nom1)

RESULTAT         <-list(Stats=RES1,MCMC=RES)
return(RESULTAT)
}
