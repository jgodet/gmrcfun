# Utile pour B.mean
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Utile pour B.mean
#'
#' @description  Utile pour B.mean
#' 
#' @param A definir
#' @export
#' 
#' @examples
#' aa <- rnorm(30,mean = 100,sd = 20)
#'bb <- rnorm(30,mean = 90,sd = 20)
#'t.test(aa,bb)
#'res <- B.moy2(aa,bb,n.iter=101000)
#'mean(res$sims.list$p)
#'hist(res$sims.list$di)
#' @return 
#' @export

B.moy2<-function(var1,var2,n.iter=11000,n.burnin=1000,n.thin=1,prior.norm=c(0,0.001,0,0.001),prior.gamma=c(0.5,0.5,0.5,0.5)){ 
  if(!require(R2jags)){install.packages('R2jags')
  library(R2jags)
#--------------------------------------------------------------------------------------------------         # Ecriture du modele WINBUGS
modelH<-function(){
for (i in 1:N1) {		X[i] ~ dnorm(mu.x,tau.x)		}											 
for (i in 1:N2) {     	Y[i] ~ dnorm(mu.y,tau.y)  		}	 																							
			  
			  mu.x	~dnorm (am  ,bm)
			  mu.y	~dnorm (cm  ,dm)
			  tau.x	~dgamma(a,b)                                 
			  tau.y	~dgamma(c,d)		 
			  di	<-     mu.x-mu.y
			  p		<-step(mu.x-mu.y)
}


donnees<-list(	 N1				=length(var1),
			     N2				=length(var2),
			     X				=var1,
			     Y				=var2,
			     a				=prior.gamma[1],
			     b				=prior.gamma[2],
			     c				=prior.gamma[3],
			     d				=prior.gamma[4],
                 am             =prior.norm[1],
                 bm             =prior.norm[2],
                 cm             =prior.norm[3],
                 dm             =prior.norm[4]                             )

parametre	 <-c("mu.x","mu.y","tau.x","tau.y","di","p")                                          ###### Fonction BUGS de R2WB
modele	     <-jags( data				=donnees,
			         inits				=NULL,
			         parameters.to.save	=parametre,	
			         model.file			= modelH,
			         n.chains			=1,
			         n.iter				=n.iter,
			         n.burnin			=n.burnin,
			         n.thin				=n.thin,
					progress.bar			="text")


RES<-modele$BUGSoutput
}







nom1		     <-deparse(substitute(var1))                                                           ### Autres STATS a faire apparaitre ?galement 
nom2		     <-deparse(substitute(var2))

indexDiff	<-grep  ("di",rownames(modele$BUGSoutput$summary))
indexMux	<-grep  ("mu.x",rownames(modele$BUGSoutput$summary))
indexMuy	<-grep  ("mu.y",rownames(modele$BUGSoutput$summary))
indexProb	<-grep  ("p",rownames(modele$BUGSoutput$summary))
indexTau	<-grep  ("tau",rownames(modele$BUGSoutput$summary))
indexdevia	<-grep  ("deviance",rownames(modele$BUGSoutput$summary))


moyennes	     <-round(RES[,1]  ,3)
variances	     <-round(RES[,2]^2,3)
sd		         <-round(RES[,2],2)
q0.025	         <-round(RES[,3],2)
q0.250	         <-round(RES[,4],2)
q0.500	         <-round(RES[,5],2)
q0.750	         <-round(RES[,6],2)
q0.975	         <-round(RES[,7],2)

RES              <-cbind(moyennes,variances,sd,q0.025,q0.250,q0.500,q0.750,q0.975)
rownames(RES)    <-c(	paste("Moy",nom1),paste("Moy",nom2),"difference",paste("P(",nom1,">",nom2,")")[1])
diff.signif<-c("","","    ***","")
if(RES[3,4]<0 & RES[3,8]>0){diff.signif<-c("","","","")}
RES<-noquote(cbind(RES,diff.signif))
RES<-RES[,-2]
colnames(RES)    <-c("Moy","Sd","2.5%","25%","50%","75%","97.5%","")

min1             <-round(min(var1,na.rm=TRUE),2)
max1             <-round(max(var1,na.rm=TRUE),2)
min2             <-round(min(var2,na.rm=TRUE),2)
max2             <-round(max(var2,na.rm=TRUE),2)
mo1              <-round(mean(var1,na.rm=TRUE),2)
mo2              <-round(mean(var2,na.rm=TRUE),2)
sd1				 <-round(  sqrt(var(var1,na.rm=TRUE))/sqrt(length(var1[!is.na(var1)]))							,2		)	
sd2				 <-round(  sqrt(var(var2,na.rm=TRUE))/sqrt(length(var2[!is.na(var2)]))							,2		)			
ete1             <-round(max1-min1,2)
ete2             <-round(max2-min2,2)
NA1              <-round(sum(is.na(var1)),2)
NA2              <-round(sum(is.na(var2)),2)
pNA1             <-round(NA1/length(var1),2)*100
pNA2             <-round(NA2/length(var2),2)*100
N1               <-length(var1)
N2               <-length(var2)
if(var(var1,na.rm=TRUE)!=0){p.Norm1 <-round(shapiro.test(var1)$p.value,2)
							if(p.Norm1<0.05){pp.Norm1<-"***"}else{pp.Norm1<-""}
							}	else{p.Norm1<-NA
		pp.Norm1<-""}
if(var(var2,na.rm=TRUE)!=0){p.Norm2 <-round(shapiro.test(var2)$p.value,2)
							if(p.Norm2<0.05){pp.Norm2<-"***"}else{pp.Norm2<-""}
							}	else{p.Norm2<-NA
		pp.Norm2<-""}

RES1<-noquote(cbind(c(N1,N2),c(NA1,NA2),c(pNA1,pNA2),c(min1,min2),c(max1,max2),c(ete1,ete2),c(mo1,mo2),c(sd1,sd2),c(p.Norm1,p.Norm2),c(pp.Norm1,pp.Norm2)))
colnames(RES1)   <-c("N","NA","%NA","min","max","etendue","Moy","Sd","Shapiro","")
rownames(RES1)   <-c(nom1,nom2)

RESULTAT         <-list(Stats=RES1,MCMC=RES)
return(RESULTAT)
}
