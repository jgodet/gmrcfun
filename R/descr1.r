# descr1.r
# written by JuG
# August 05 2019


#' Descriptive statistics for quantitative data
#' @author JuG
#' @description 
#' @param Y numeric vector 
#' @param Tap boolean (tri à plat)
#' @details 
#' @examples 
#' Y <- runif(n = 100)
#' descr1(Y)
#' @return 
#' @export


descr1<-function(Y,Tap=FALSE){
  if(Tap){res<-list(Descriptif=NULL,TestNormalite=NULL,Tap=NULL)}else{res<-list(Descriptif=NULL,TestNormalite=NULL)}
  nomY<-deparse(substitute(Y))
  library(moments)
  aze<-matrix(NA,ncol=1,nrow=24)
  rownames(aze)<-c("Effectifs presents","Proportions de presents %","Effectifs manquants","Proportions de manquants %",
                   "Moyenne","Ecart-type","Variance","Erreur standard (s.e.m)","Minimum","Maximum","Percentile 2,5","Percentile 5","Q1 ",
                   "Mediane","Q3","Percentile 95","Percentile 97,5","Ecart inter-quartiles","IC valeurs borne inf","IC valeurs borne sup",
                   "IC moyenne borne inf","IC moyenne borne sup","coefficient d'asymetrie","Kurtosis")
  
  
  colnames(aze)<-nomY ###,levels(X))
  qtl<-function(x){quantile(x,probs = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975), na.rm = T)}
  nbm<-function(x){sum(is.na(x))}
  nbp<-function(x){sum(!is.na(x))}
  nbmpct<-function(x){sum(is.na(x))*100/length(x)}
  nbppct<-function(x){sum(!is.na(x))*100/length(x)}
  nbmqt<-sum(is.na(Y))
  ddl<-nbp(Y)-1
  errstm<-sd(Y,na.rm=TRUE)/sqrt(nbp(Y))
  liminfy<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  limsupy<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  liminfy2<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  limsupy2<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  asymetrie<-skewness(Y,na.rm=TRUE)
  kurt<-kurtosis(Y,na.rm=TRUE)
  aze[1,]<-nbp(Y)
  aze[2,]<-nbppct(Y)
  aze[3,]<-nbm(Y)
  aze[4,]<-nbmpct(Y)
  aze[5,]<-mean(Y,na.rm=TRUE)
  aze[6,]<-sd(Y,na.rm=TRUE)
  aze[7,]<-var(Y,na.rm=TRUE)
  aze[8,]<-errstm
  aze[9,]<-min(Y,na.rm=TRUE)
  aze[10,]<-max(Y,na.rm=TRUE)
  aze[11:17,1]<-qtl(Y)
  aze[18,]<-IQR(Y,na.rm=TRUE)
  aze[19,]<-liminfy
  aze[20,]<-limsupy
  aze[21,]<-liminfy2
  aze[22,]<-limsupy2
  aze[23,]<-asymetrie
  aze[24,]<-kurt
  aze<-round(aze,digits=4)
  
  pvalnorm<-matrix(c(NA,NA),ncol=1)
  rownames(pvalnorm)<-c("Test de normalite de Shapiro-Wilk       : p =","Test de normalite de Kolmogorov-Smirnov : p =")
  colnames(pvalnorm)<-c("")
  if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value}else{pvalnorm[1]<-NA}
  pvalnorm[2]<-ks.test(Y,"pnorm",mean(Y,na.rm=T),sd(Y,na.rm=T))$p.value
  pvalnorm<-round(pvalnorm,digits=4)
  
  if(Tap){long<-length(table(Y))
  nbval<-sum(!is.na(Y))#length(Y)
  triap<-matrix(NA,ncol=4,nrow=long)
  rownames(triap)<-unique(sort(Y))
  colnames(triap)<-c("Eff.","Eff. cum.","Prop.","Prop. cum")
  triap[,1]<-table(Y)
  triap[,2]<-cumsum(table(Y))
  triap[,3]<-round(table(Y)*100/nbval,digits=2)
  triap[,4]<-round(cumsum(table(Y))*100/nbval,digits=2)
  res <- list(Descriptif = aze, TestNormalite = pvalnorm, Triaplat=triap )
  }
  if(!Tap){res <- list(Descriptif = aze, TestNormalite = pvalnorm)}
  return(res)
}
