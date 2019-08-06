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


B.prop2<-function(var1,var2,n.iter=11000,n.burnin=1000,n.thin=1,prior.beta=c(1,1,1,1)){ 

library(R2jags)
#--------------------------------------------------------------------------------------------------         
modelH<-function(){
			  	for (i in 1:N1) {  X[i] ~ dbin(mu.x,1)	}
				for (j in 1:N2) {	 Y[j] ~ dbin(mu.y,1)    } 
			 		 mu.x~dbeta(a,b)
			 		 mu.y~dbeta(c,d)
																
			  	di<-mu.x-mu.y
				p<-step(mu.x-mu.y)
				or<-mu.x*(1-mu.y)/(mu.y*(1-mu.x))
}

donnees<-list(	     N1				=length(var1),
			     N2				=length(var2),
			     X				=dput(as.numeric(var1),		control=NULL),
			     Y				=dput(as.numeric(var2),		control=NULL),
			     a				=prior.beta[1],
			     b				=prior.beta[2],
			     c				=prior.beta[3],
			     d				=prior.beta[4]                            )

parametre	 <-c("mu.x","mu.y","di","p","or")                                          
modeleB2	     <-jags( data				=donnees,
			         inits				=NULL,
			         parameters.to.save	=parametre,	
			         model.file			= modelH,
			         n.chains			=1,
			         n.iter			=n.iter,
			         n.burnin			=n.burnin,
			         n.thin			=n.thin,
					progress.bar			="text")

nom1		     <-deparse(substitute(var1))                                                           ### Autres STATS a faire apparaitre ?galement 
nom2		     <-deparse(substitute(var2))
indexDiff	<-grep  ("di",rownames(modeleB2$BUGSoutput$summary))
indexMux	<-grep  ("mu.x",rownames(modeleB2$BUGSoutput$summary))
indexMuy	<-grep  ("mu.y",rownames(modeleB2$BUGSoutput$summary))
indexProb	<-grep  ("p",rownames(modeleB2$BUGSoutput$summary))
indexOR		<-grep  ("or",rownames(modeleB2$BUGSoutput$summary))


RES<-modeleB2$BUGSoutput$summary[c(indexMux,indexMuy,indexDiff,indexProb,indexOR),]
moyennes	     <-round(RES[,1]  ,3)
variances	     <-round(RES[,2]^2,3)
sd		         <-round(RES[,2],2)
q0.025	         <-round(RES[,3],2)
q0.250	         <-round(RES[,4],2)
q0.500	         <-round(RES[,5],2)
q0.750	         <-round(RES[,6],2)
q0.975	         <-round(RES[,7],2)

RES              <-cbind(moyennes,variances,sd,q0.025,q0.250,q0.500,q0.750,q0.975)[-6,]

rownames(RES)    <-c(	paste("Prop",nom1),paste("Prop",nom2),"Difference",paste("P(",nom1,">",nom2,")")[1],"Odds.Ratio")

diff.signif<-c("","","    ***","","")
if(RES[3,4]<0 & RES[3,8]>0){diff.signif<-c("","","","","")}
RES<-noquote(cbind(RES,diff.signif))
colnames(RES)    <-c("Mean","Var","Sd","2.5%","25%","50%","75%","97.5%","")
RES<-RES[,-2]
var11<-var1
var22<-var2
var1<-var1[!is.na(var1)]
var2<-var2[!is.na(var2)]
succes1          <-sum(var1,na.rm=TRUE)
echec1           <-length(var1)-succes1
succes2          <-sum(var2,na.rm=TRUE)
echec2           <-length(var2)-succes2
p.succes1	     <-round(succes1/length(var1),2)
p.succes2	     <-round(succes2/length(var2),2)
p.echec1	     <-round(echec1/length(var1),2)
p.echec2	     <-round(echec2/length(var2),2)
NA1              <-round(sum(is.na(var11)),2)
NA2              <-round(sum(is.na(var22)),2)
pNA1             <-round(NA1/length(var11),2)*100
pNA2             <-round(NA2/length(var22),2)*100
N1               <-length(var11)
N2               <-length(var22)


RES1<-noquote(cbind(c(N1,N2),c(NA1,NA2),c(pNA1,pNA2),c(succes1,succes2),c(echec1,echec2),c(p.succes1,p.succes2),c(p.echec1,p.echec2)))
colnames(RES1)   <-c("N","NA","%NA","#1","#0","%1","%0")
rownames(RES1)   <-c(nom1,nom2)

RESULTAT         <-list(Stats=RES1,MCMC=RES)
return(RESULTAT)
}