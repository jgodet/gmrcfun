# descr3.r
# written by JuG
# August 05 2019


#' Descriptive statistics for quantitative data as a function of a qualitative data
#' @author JuG
#' @description 
#' @param X factor (or group) vector
#' @param Y numeric vector 
#' @param Tap boolean (tri à plat)
#' @param nom X label
#' @param nomY Y label
#' @param latex  boolean latex output
#' @details 
#' @examples 
#' X <- sample(x = letters[1:2],size = 100, replace = T)
#' Y <- runif(n = 100)
#' descr3(Y, X)
#' descr3(Y, X, latex = T)
#' @return 
#' @export


descr3<-function(Y,X,Tap=FALSE,nom=NULL, nomY =NULL, latex=0){
  
  if(Tap){    res<-list(Descriptif=NULL,TestNormalite=NULL,Testpv=NULL,TestsNPv=NULL,Tests_de_Student=NULL,TestsNP=NULL,Triaplat=NULL,CommqT=NULL)}else{
    res<-list(Descriptif=NULL,TestNormalite=NULL,Testpv=NULL,TestsNPv=NULL,Tests_de_Student=NULL,TestsNP=NULL)}
  if(is.null(nom)){nom<-deparse(substitute(X))}
  if(!is.factor(X)){X<-as.factor(X)}
  if(is.null(nomY)){nomY<-deparse(substitute(Y))}
  nbnv<-nlevels(X)
  library(moments)
  
  aze<-matrix(NA,ncol=1+nbnv,nrow=27)
  rownames(aze)<-c("Effectifs presents","Proportions de presents","Effectifs manquants","Proportions de manquants",
                   "Moyenne","Ecart-type","Variance","Erreur standard (s.e.m)","Err. Std (basee sur l'ANOVA)","Minimum","Maximum","Percentile 2,5","Percentile 5","Q1 ","Mediane","Q3",
                   "Percentile 95","Percentile 97,5","Ecart inter-quartiles","IC valeurs borne inf","IC valeurs borne sup","IC moyenne borne inf",
                   "IC moyenne borne sup","IC moyenne borne inf (ANOVA)","IC moyenne borne sup (ANOVA)","coefficient d'asymetrie","Kurtosis")
  
  
  colnames(aze)<-c(nomY,paste(" ",nom,"=",levels(X)))
  
  qtl<-function(x){quantile(x,probs = c(0.025,0.05,0.25,0.5,0.75,0.95,0.975), na.rm = T)}
  nbm<-function(x){sum(is.na(x))}
  nbp<-function(x){sum(!is.na(x))}
  nbmpct<-function(x){sum(is.na(x))*100/length(x)}
  nbppct<-function(x){sum(!is.na(x))*100/length(x)}
  nbmqt<-sum(is.na(Y))
  ddl<-length(Y)-1
  ddlg<-tapply(Y,X,nbp)-1
  aa<-summary(aov(Y~X))
  carmoy<-aa[[1]][2,3]
  ddlaov<-aa[[1]][2,1]
  nbvalg<-tapply(Y,X,nbp)
  aze[1,]<-c(nbp(Y),nbvalg)
  aze[2,]<-c(nbppct(Y),tapply(Y,X,nbppct))
  aze[3,]<-c(nbm(Y),tapply(Y,X,nbm))
  aze[4,]<-c(nbmpct(Y),tapply(Y,X,nbmpct))
  aze[5,]<-c(mean(Y,na.rm=TRUE),tapply(Y,X,mean,na.rm=TRUE))
  aze[6,]<-c(sd(Y,na.rm=TRUE),tapply(Y,X,sd,na.rm=T))
  aze[7,]<-c(var(Y,na.rm=TRUE),tapply(Y,X,var,na.rm=T))
  
  errstm<-sd(Y,na.rm=TRUE)/sqrt(ddl)
  errstmg<-tapply(Y,X,sd,na.rm=T)/sqrt(ddlg+1)
  aze[8,]<-c(errstm,errstmg)
  errstgr<-(carmoy/nbvalg)^0.5
  aze[9,]<-c(NA,errstgr)
  aze[10,]<-c(min(Y,na.rm=TRUE),tapply(Y,X,min,na.rm=T))
  aze[11,]<-c(max(Y,na.rm=TRUE),tapply(Y,X,max,na.rm=T))
  aze[12:18,1]<-qtl(Y)
  stock<-tapply(Y,X,qtl)
  for(i in 1:nbnv){aze[12:18,1+i]<-stock[[i]]}
  
  aze[19,]<-c( stats::IQR(Y,na.rm=TRUE),tapply(Y,X, stats::IQR,na.rm=T))
  
  liminfy<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  liminfyx<-tapply(Y,X,mean,na.rm=T)-tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)
  limsupy<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)
  limsupyx<-tapply(Y,X,mean,na.rm=T)+tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)
  aze[20,]<-c(liminfy,liminfyx)
  aze[21,]<-c(limsupy,limsupyx)
  
  liminfy2<-mean(Y,na.rm=TRUE)-qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  liminfyx2<-tapply(Y,X,mean,na.rm=T)-tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)/sqrt(ddlg+1)
  limsupy2<-mean(Y,na.rm=TRUE)+qt(0.975,ddl)*sd(Y,na.rm=TRUE)/sqrt(ddl+1)
  limsupyx2<-tapply(Y,X,mean,na.rm=T)+tapply(Y,X,sd,na.rm=T)*qt(0.975,ddlg)/sqrt(ddlg+1)
  aze[22,]<-c(liminfy2,liminfyx2)
  aze[23,]<-c(limsupy2,limsupyx2)
  
  liminfg<-tapply(Y,X,mean,na.rm=TRUE)-qt(0.975,df=ddlaov)*errstgr
  limsupg<-tapply(Y,X,mean,na.rm=TRUE)+qt(0.975,df=ddlaov)*errstgr
  
  aze[24,]<-c(NA,liminfg)
  aze[25,]<-c(NA,limsupg)
  
  aze[26,]<-c(skewness(Y,na.rm=TRUE),tapply(Y,X,skewness,na.rm=T))
  aze[27,]<-c(kurtosis(Y,na.rm=TRUE),tapply(Y,X,kurtosis,na.rm=T))
  
  
  if(nlevels(X)==2){pvaleur<-format.pval(wilcox.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvaleur<-format.pval(kruskal.test(Y~X)$p.value,digits=4)}}
  
  if(nlevels(X)==2){testnp<-paste("Test de Mann & Whitney : p =",pvaleur)}else{if(nlevels(X)>2){testnp<-paste("Test de Kruskal & Wallis : p =",pvaleur)}}
  
  pvalstud<-matrix(c(NA,NA),ncol=1)
  rownames(pvalstud)<-c("Test de Student, variances egales   : p =","Test de Student, variances inegales : p =")
  colnames(pvalstud)<-c("")
  
  if(nlevels(X)==2)
  {pvalstud[1]<-t.test(Y~X,var.equal = TRUE)$p.value}
  else
  {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}
  
  if(nlevels(X)==2)
  {pvalstud[2]<-t.test(Y~X,var.equal = FALSE)$p.value}
  else
  {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}
  
  
  if(nlevels(X)==2){testp<-round(pvalstud,digits=4)}else{if(nlevels(X)>2){testp<-paste("Analyse de la Variance : p =",pval)}}
  
  if(nlevels(X)==2){pvartest<-format.pval(var.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvartestpg<-format.pval(bartlett.test(Y~X)$p.value,digits=4)}}
  
  if(nlevels(X)==2){testpv<-paste(list(paste("Test parametrique d'egalite de deux variances (Fisher): p =",pvartest)
  ))
  }else{if(nlevels(X)>2){testpv<-paste("Test parametrique d'egalite de plus de deux variances (Bartlett) : p =",pvartestpg)}}
  
  pvalnorm<-matrix(c(NA,NA),ncol=1)
  rownames(pvalnorm)<-c("Test de normalite de Shapiro-Wilk       : p =","Test de normalite de Kolmogorov-Smirnov : p =")
  colnames(pvalnorm)<-c("")
  if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value}else{pvalnorm[1]<-NA}
  pvalnorm[2]<-ks.test(Y,"pnorm",mean(Y,na.rm=T),sd(Y,na.rm=T))$p.value
  pvalnorm<-round(pvalnorm,digits=4)
  
  if(nlevels(X)==2){pvalfl2g<-format.pval(ansari.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvalfl3g<-format.pval(fligner.test(Y~X)$p.value,digits=4)}}
  
  if(nlevels(X)==2){testnpv<-paste(list(paste("Test non param. d'egalite de deux variances (Ansari) : p =",pvalfl2g)
  ))
  }else{if(nlevels(X)>2){testnpv<-paste("Test non param. d'egalite de plus de deux variances (Fligner) : p =",pvalfl3g)}}
  
  if(Tap){long<-length(table(Y))
  nbval<-sum(!is.na(Y))#length(Y)
  triap<-matrix(NA,ncol=5+nbnv,nrow=long)
  rownames(triap)<-unique(sort(Y))
  colnames(triap)<-c("Eff.","Eff. cum.","Prop.","Prop. cum",paste(" ",nom,"=",levels(X)),"Tot.Compl")
  triap[,1]<-table(Y)
  triap[,2]<-cumsum(table(Y))
  triap[,3]<-round(table(Y)*100/nbval,digits=2)
  triap[,4]<-round(cumsum(table(Y))*100/nbval,digits=2)
  triap[,5:(5+nbnv-1)]<-matrix(table(Y,X))
  triap[,(5+nbnv)]<-matrix(apply(table(Y,X),1,sum))
  }
  
  
  
  if(Tap){
    res$Descriptif<-round(aze,digits=3)
    res$TestNormalite<-pvalnorm
    res$Tests_de_Student<-testp
    res$TestsNP<-testnp
    res$Testpv<-testpv
    res$TestsNPv<-testnpv
    res$Triaplat<-triap
    if(sum(res$Triaplat[,1]!=res$Triaplat[,(5+nbnv)])){commqt<-paste("ATTENTION : Il y a des valeurs manquantes dans le tableau croise.");res$CommqT<-commqt}
    #res$CommqT<-commqt
  }
  
  if(!Tap){
    res$Descriptif<-round(aze,digits=3)
    res$TestNormalite<-pvalnorm
    res$Tests_de_Student<-testp
    res$TestsNP<-testnp
    res$Testpv<-testpv
    res$TestsNPv<-testnpv
  }
  
  if(latex==1){
    library(xtable)
    if(pvalnorm[1]>=0.05){pvalTEX<-testp[1]}else{pvalTEX<-as.numeric(gsub(".* ([0-9.]+).*", "\\1",testnp[1]))}
    cat(paste("Croisement de la variable", nomY, "en fonction de" ,nom))
    cat("\n")
    print(xtable(res$Descriptif[-c(8,9,19,20,21,24,25,26,27),],align="|r|rrr|"),table.placement="H",size="small")
    cat(paste("La p.valeur associee aux croisement de ces variables est de: "));cat(pvalTEX);cat("\n")
  }
  if(latex==0){return(res)}
}
