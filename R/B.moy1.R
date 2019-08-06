# -*- ISO-8859-1 -*-

# Utile pour B.mean
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Utile pour B.mean
#'
#' Utile pour B.mean
#' 
#' @param A definir



B.moy1<-function(var1,n.iter=11000,n.burnin=1000,n.thin=1,prior.norm=c(0,0.001,0,0.001),prior.gamma=c(0.5,0.5,0.5,0.5)){ 
library(R2jags)
#--------------------------------------------------------------------------------------------------         # ecriture du modele WINBUGS
modelH<-function(){
for (i in 1:N1) {		X[i] ~ dnorm(mu.x,tau.x)		}											 
			  mu.x	~dnorm (am,bm)
			  tau.x	~dgamma(a,b)                                 		 
			  p		<-step(mu.x)
}

donnees<-list(	N1				=length(var1),
			     X				=var1,
			     a				=prior.gamma[1],
			     b				=prior.gamma[2],
                 am             =prior.norm[1],
                 bm             =prior.norm[2])
parametre	 <-c("mu.x","tau.x","p")                                          ###### Fonction BUGS de R2WB
modele	     <-jags( data				=donnees,
			         inits				=NULL,
			         parameters.to.save	=parametre,	
			         model.file			= modelH,
			         n.chains			=1,
			         n.iter				=n.iter,
			         n.burnin			=n.burnin,
			         n.thin				=n.thin,
					     progress.bar			="text")
 
nom1		     <-deparse(substitute(var1))                                                           ### Autres STATS a faire apparaitre egalement 
indexProb	<-grep  ("p",rownames(modele$BUGSoutput$summary))
indexMux	<-grep  ("mu.x",rownames(modele$BUGSoutput$summary))
indexTau	<-grep  ("tau.x",rownames(modele$BUGSoutput$summary))
indexdevia	<-grep  ("deviance",rownames(modele$BUGSoutput$summary))
RES<-modele$BUGSoutput$summary[c(indexMux,indexProb,indexTau,indexdevia),]

moyennes	     <-round(RES[,1]  ,3)
variances	     <-round(RES[,2]^2,3)
sd		         <-round(RES[,2],2)
q0.025	         <-round(RES[,3],2)
q0.250	         <-round(RES[,4],2)
q0.500	         <-round(RES[,5],2)
q0.750	         <-round(RES[,6],2)
q0.975	         <-round(RES[,7],2)

RES              <-cbind(moyennes,sd,q0.025,q0.250,q0.500,q0.750,q0.975)
colnames(RES)    <-c("Moy","Sd","2.5%","25%","50%","75%","97.5%")
rownames(RES)    <-c(	paste("Moy",nom1),"Tau",paste("P(",nom1,">0 )")[1],"deviance")
min1             <-round(min(var1,na.rm=TRUE),2)
max1             <-round(max(var1,na.rm=TRUE),2)
mo1              <-round(mean(var1,na.rm=TRUE),2)
ete1             <-round(max1-min1,2)
NA1              <-round(sum(is.na(var1)),2)
pNA1             <-round(NA1/length(var1),2)*100
N1               <-length(var1)
if((var(var1,na.rm=TRUE)!=0) & (length(var1)>3)){p.Norm1 <-round(shapiro.test(var1)$p.value,2)
							if(p.Norm1<0.05){pp.Norm1<-"***"}else{pp.Norm1<-""}
							}
	else{p.Norm1<-NA
		pp.Norm1<-""}


RES1<-noquote(cbind(N1,NA1,pNA1,min1,max1,ete1,mo1,p.Norm1,pp.Norm1))
colnames(RES1)   <-c("N","NA","%NA","min","max","etendue","moy","Shapiro","")
rownames(RES1)   <-nom1

RESULTAT         <-list(Stats=RES1,MCMC=RES)
return(RESULTAT)
}
