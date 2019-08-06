# ggsurvie.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' status <- rbinom(n=50, size=1, prob = .5)
#' time <- rexp(n=50) * 20
#' ggsurvie(x=time, y=status)
#' @return 
#' @export



ggsurvie<-function(x,y,groups=0,latex=0,titre=0){
  
  #if(!require('GGally')){install.packages('GGally')}
  #require(GGally)
  #ggsurv if from gmrcfun package
  
  options(warn=-pi)
  if(length(groups)==1){
    
    if(groups==0 & latex==0){situation=1}			# Pas de GRP		# Pas de LTX
    if(groups==0 & latex!=0){situation=3}			# Pas de GRP		#        LTX
  }
  else{
    if(latex==0){situation=2}			# 	   GRP		# Pas de LTX
    if(latex!=0){situation=4}			# 	   GRP		#        LTX
  }
  if(!require(xtable))      install.packages('xtable')  ;    library(xtable)
  if(!require(survival))    install.packages('survival');    library(survival)
  
  
  nom.y<-deparse(substitute(y))
  if(titre==1){titre.ss.groupe<-paste("Courbe de survie: Variable",nom.y)}
  if(groups!=0){nom.groupes<-deparse(substitute(groups))
  if(titre==1){titre.ac.groupe<-paste("Courbe de survie: Variable",nom.y,"en fonction de:",nom.groupes)}}
  
  if(titre==0){titre.ss.groupe<-titre.ac.groupe<-""}
  if(titre!=0 & titre!=1){titre.ss.groupe<-titre.ac.groupe<-titre}
  
  if(situation==1){		surv2 		<- survfit(Surv(x,y) ~ 1)
  plot2 		<- ggsurv(surv2,main=titre.ss.groupe)+ylim(0,1)
  print(plot2+ylim(0,1))
  print(surv2)
  print(summary(surv2))
  }
  
  if(situation==2){		Groupe		<- groups
  surv2 		<- survfit(Surv(x,y) ~ Groupe)
  plot2 		<- ggsurv(surv2,main=titre.ac.groupe)+ylim(0,1)
  
  print(plot2+ylim(0,1))
  print(surv2)
  print(summary(surv2))
  print(survdiff(Surv(x,y) ~ groups))
  }
  
  if(situation==3){		surv2 		<- survfit(Surv(x,y) ~ 1)
  plot2 		<- ggsurv(surv2,main=titre.ss.groupe)+ylim(0,1)
  print(plot2+ylim(0,1))
  DETAIL		<-cbind(surv2 $ time , surv2 $ n.risk,  surv2 $ n.event,  surv2 $ n.censor, surv2$surv ,surv2 $ std.err  ,surv2 $ upper,surv2 $ lower   )
  colnames(DETAIL)	<-c("time","n.risk","n.event","n.censor","surv","std.err","upper95%","lower95%")
  print(xtable(DETAIL))
  }
  
  if(situation==4){		Groupe		<- groups
  surv2 		<- survfit(Surv(x,y) ~ Groupe)
  plot2 		<- ggsurv(surv2,main=titre.ac.groupe)+ylim(0,1)
  print(plot2+ylim(0,1))
  DETAIL		<-cbind(surv2 $ time , surv2 $ n.risk,  surv2 $ n.event,  surv2 $ n.censor, surv2$surv ,surv2 $ std.err  ,surv2 $ upper,surv2 $ lower   )
  colnames(DETAIL)	<-c("time","n.risk","n.event","n.censor","surv","std.err","upper95%","lower95%")
  ST			<-survdiff(Surv(x,y) ~ groups)
  df			<-nlevels(as.factor(groups))-1
  p.val			<-1-pchisq(ST$chisq,df)
  cat(paste("La p.valeur associee au test de comparaison des courbes de survie (Test du log-Rank) est de:",round(p.val,2)))
  cat(" \n")				
  print(xtable(DETAIL))			
  }
}
