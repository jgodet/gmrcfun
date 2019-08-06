# Utile pour B.prop
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Utile pour B.prop
#'
#' Utile pour B.prop
#' 
#' @param A definir





B.prop1<-function(var1,n.iter=11000,n.burnin=1000,n.thin=1,prior.beta=c(1,1,1,1)){ 
library(R2jags)
#--------------------------------------------------------------------------------------------------         
modelH<-function(){
			  	for (i in 1:N1) {  X[i] ~ dbin(mu.x,1)	}
			 		 mu.x~dbeta(a,b)							
					p<-step(mu.x-0.5)		    	
}

donnees<-list(	     N1				=length(var1),	    
			     X				=var1,	     
			     a				=prior.beta[1],
			     b				=prior.beta[2])

parametre	 <-c("mu.x","p")                                          
modeleB1	     <-jags( data			=donnees,
			         inits			=NULL,
			         parameters.to.save	=parametre,	
			         model.file		= modelH,
			         n.chains		=1,
			         n.iter			=n.iter,
			         n.burnin		=n.burnin,
			         n.thin			=n.thin,
					progress.bar			="text")
					 
nom1		     <-deparse(substitute(var1))                                                           ### Autres STATS a faire apparaitre ?galement 
indexDiff	<-grep  ("di",rownames(modeleB1$BUGSoutput$summary))
indexMux	<-grep  ("mu.x",rownames(modeleB1$BUGSoutput$summary))
indexMuy	<-grep  ("mu.y",rownames(modeleB1$BUGSoutput$summary))
indexProb	<-grep  ("p",rownames(modeleB1$BUGSoutput$summary))
indexOR		<-grep  ("or",rownames(modeleB1$BUGSoutput$summary))
RES<-modeleB1$BUGSoutput$summary[c(indexMux,indexMuy,indexDiff,indexProb,indexOR),]
moyennes	     <-round(RES[,1]  ,3)
variances	     	<-round(RES[,2]^2,3)
sd		         <-round(RES[,2],2)
q0.025	         <-round(RES[,3],2)
q0.250	         <-round(RES[,4],2)
q0.500	         <-round(RES[,5],2)
q0.750	         <-round(RES[,6],2)
q0.975	         <-round(RES[,7],2)
RES              <-cbind(moyennes,variances,sd,q0.025,q0.250,q0.500,q0.750,q0.975)[-3,]
rownames(RES)    <-c(	paste("Prop",nom1),paste("P(",nom1,">0.5)")[1])
RES<-noquote(RES)
colnames(RES)    <-c("Mean","Sd^2","Sd","2.5%","25%","50%","75%","97.5%")
var11<-var1
var1<-var1[!is.na(var1)]
succes1          <-sum(var1)
echec1           <-length(var1)-succes1
p.succes1	     <-round(succes1/length(var1),2)
p.echec1	     <-round(echec1/length(var1),2)
NA1              <-round(sum(is.na(var11)),2)
pNA1             <-round(NA1/length(var11),2)*100
N1               <-length(var11)

RES1<-noquote(cbind(N1,NA1,pNA1,succes1,echec1,p.succes1,p.echec1))
colnames(RES1)   <-c("N","NA","%NA","#1","#0","%1","%0")
rownames(RES1)   <-nom1

RESULTAT         <-list(Stats=RES1,MCMC=RES)
return(RESULTAT)
}
