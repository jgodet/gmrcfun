# HosLem.r
# written by JuG
# August 05 2019


#' The Hosmer-Lemeshow goodness of fit test for logistic regression
#' @author JuG
#' @description  Hosmer-Lemeshow goodness of fit test for logistic regression. 
#' @param modele
#' @details If the p-value is small, this is indicative of poor fit
#' @examples 
#'
#'
#' @return 
#' @export


HosLem<-function(modele){
  Y<-modele$y
  p0<-0.5
  p<-predict(modele,type="response")
  Y.hat<-ifelse(p>p0,1,0)
  bornes<-quantile(p,probs=seq(0,1,0.1))        # Decoupage en une dizaine de classes a partir des quantiles empiriques
  if(length(table(bornes))<10) print("Erreur, ex-aequo dans les quantiles, prob de decoupage en classes") 
  L<-length(table(bornes))-1                    # Preferez 0:10 a 1:11
  classes<-rep(0,length(p))                     # Creation d'un vecteur de classes 
  for(k in 1:L)                                 # Vecteur de classes 
  {
    bo<-bornes[k+1];
    for(j in 1:(length(p)))
      if((p[j]<=bornes[k+1]) & (p[j]> bornes[k])) {classes[j]<-k+1}
  }
  classes<-classes-1                            
  classes[classes==-1]<-1                       # Le premier Y est attribue a une classe seule -> Jonction
  donnees<-cbind(p,Y,Y.hat,classes)             # Lecture de nos donnees
  donnees<-donnees[order(donnees[,1]),]
  Obs<-matrix(99,L,2)                           # Creation des observes par classe
  for(k in 1:L)
  {
    Obs[k,1]<-   sum(Y[classes==k])
    Obs[k,2]<-length(Y[classes==k])-sum(Y[classes==k])
  }
  Exp<-matrix(99,L,2)                           # Creation des attendus par classe
  for(k in 1:L)
  {
    Exp[k,1]<-   sum(p[classes==k])
    Exp[k,2]<-length(p[classes==k])-sum(p[classes==k])
  }                                             # Statistique du Khi-2
  HR<-sum((Obs[,1]-Exp[,1])^2/(Exp[,1])+(Obs[,2]-Exp[,2])^2/(Exp[,2]))
  p.val<-1-pchisq(HR,(L-2))                     # P-valeur du test
  if(HR>qchisq(1-0.05,df=(L-2)))
  {Hosmer_LS="H1->Rejet"}else
  {Hosmer_LS="H0->Non-Rejet"}
  res<-list(Test=Hosmer_LS,p.valeur=p.val)
  return(res)
}