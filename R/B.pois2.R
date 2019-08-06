# Utile pour B.pois
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Utile pour B.pois
#'
#' Utile pour B.pois
#' 
#' @param A definir




B.pois2<-function(var1,var2,n.iter=11000,n.burnin=1000,n.thin=1,prior.gamma=c(0.1,0.1,0.1,0.1)){ 
library(R2jags)
#--------------------------------------------------------------------------------------------------         
modelH<-function(){
		for (i in 1:N1) {	X[i] ~ dpois(lambda.x)	}			# et on a pas la normalite, meme apres transfo					 
	      for (i in 1:N2) { Y[i] ~ dpois(lambda.y) } 									
				lambda.x~dgamma(a,b)
				lambda.y~dgamma(c,d)
			  	di <-lambda.x-lambda.y	
			  	p  <-step(lambda.x-lambda.y)	   	 		
					    	
}
#--------------------------------------------------------------------------------------------------



donnees<-list(	 N1				=length(var1),
			     N2				=length(var2),
			     X				=dput(as.numeric(var1),		control=NULL),
			     Y				=dput(as.numeric(var2),		control=NULL),
			     a				=prior.gamma[1],
			     b				=prior.gamma[2],
			     c				=prior.gamma[3],
			     d				=prior.gamma[4])

parametre	 <-c("lambda.x","lambda.y","di","p")                                          
modele	     <-jags( data				=donnees,
			         inits				=NULL,
			         parameters.to.save	=parametre,	
			         model.file			=modelH,
			         n.chains			= 1,
			         n.iter			    =n.iter,
			         n.burnin			=n.burnin,
			         n.thin			    =n.thin,
					progress.bar			="text")

nom1		     <-deparse(substitute(var1))                                                        
nom2		     <-deparse(substitute(var2))
indexDiff		 <-grep  ("di",rownames(modele$BUGSoutput$summary))
indexMux		 <-grep  ("lambda.x",rownames(modele$BUGSoutput$summary))
indexMuy		 <-grep  ("lambda.y",rownames(modele$BUGSoutput$summary))
indexProb		 <-grep  ("p",rownames(modele$BUGSoutput$summary))
RES				 <-modele$BUGSoutput$summary[c(indexMux,indexMuy,indexDiff,indexProb),]
moyennes	     <-round(RES[,1]  ,3)
variances	     <-round(RES[,2]^2,3)
sd		         <-round(RES[,2],3)
q0.025	         <-round(RES[,3],3)
q0.250	         <-round(RES[,4],3)
q0.500	         <-round(RES[,5],3)
q0.750	         <-round(RES[,6],3)
q0.975	         <-round(RES[,7],3)
RES              <-cbind(moyennes,variances,sd,q0.025,q0.250,q0.500,q0.750,q0.975)[-5,]
rownames(RES)    <-c(	paste("Prop",nom1),paste("Prop",nom2),"difference",paste("P(",nom1,">",nom2,")")[1])
diff.signif      <-c("","","    ***","")
if(RES[3,4]<0 & RES[3,8]>0){diff.signif<-c("","","","")}
RES              <-noquote(cbind(RES,diff.signif))
colnames(RES)    <-c("Moy","Sd^2","Sd","2.5%","25%","50%","75%","97.5%","")
var11<-var1
var22<-var2
var1             <-var1[!is.na(var1)]
var2             <-var2[!is.na(var2)]
min1             <-min(var1)
max1             <-max(var1)
min2             <-min(var2)
max2             <-max(var2)
ete1	         <-max(var1)-min(var1)
ete2	         <-max(var2)-min(var2)
moy1		     <-round(mean(var1),3)
moy2	         <-round(mean(var2),3)
NA1              <-round(sum(is.na(var11)),3)
NA2              <-round(sum(is.na(var22)),3)
pNA1             <-round(NA1/length(var11),3)*100
pNA2             <-round(NA2/length(var22),3)*100
N1               <-length(var11)
N2               <-length(var22)
RES1             <-noquote(cbind(c(N1,N2),c(NA1,NA2),c(pNA1,pNA2),c(min1,min2),c(max1,max2),c(ete1,ete2),c(moy1,moy2)))
colnames(RES1)   <-c("N","NA","%NA","min","max","etend","moy")
rownames(RES1)   <-c(nom1,nom2)
RESULTAT         <-list(Stats=RES1,MCMC=RES)
return(RESULTAT)
}
