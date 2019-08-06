# compare.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


compare<-function(x,y,paired,modmax){	
  
  if(missing(modmax))  {mod.max<-7}    else{mod.max<-modmax}
  if(missing(paired))   {appa<-"FALSE"} else{appa<-paired}
  cas=2								                                                            # Reconnaitre la nature des variables 
  if(is.factor(x)  & is.factor (y)){cas=1}
  if(is.factor(x)  & is.numeric(y)){if(nlevels(as.factor(y))<=mod.max)    {cas=1}else{cas=2}}
  if(is.numeric(x) & is.factor (y)) {if(nlevels(as.factor(x))<=mod.max)    {cas=1}else{cas=2}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))<=mod.max & nlevels(as.factor(y))<=mod.max) {cas=1}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))<=mod.max & nlevels(as.factor(y))> mod.max) {cas=2}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))> mod.max & nlevels(as.factor(y))<=mod.max) {cas=2}}
  if(is.numeric(x) & is.numeric(y)){if(nlevels(as.factor(x))> mod.max & nlevels(as.factor(y))> mod.max) {cas=3}}
  if(is.character(x)&is.character(y)){cas=1}
  if(is.character(x)&is.factor(y)){cas=1}
  if(is.factor(x)&is.character(y)){cas=1}
  
  
  if(cas==2){result<-"Erreur: Les variables cont de nature differentes, impossibilite de comparer"}
  
  
  ########################################################## PREMIER CAS : COMPARAISON DE DEUX VARIABLES CONTINUES ####################################
  if(cas==3){
    S	<-Q<-matrix(99,ncol=9,nrow=3)
    N   <-c(99,99,99)
    C	<-matrix(99,1,2)
    T   <-matrix(99,ncol=2,nrow=2)
    S1<-round(c(length(x),sum(is.na(x)),mean(x,na.rm=TRUE),var(x,na.rm=TRUE),sd(x,na.rm=TRUE),median(x,na.rm=TRUE),min(x,na.rm=TRUE),max(x,na.rm=TRUE),max(x,na.rm=TRUE)-min(x,na.rm=TRUE)),3)
    S2<-round(c(length(y),sum(is.na(y)),mean(y,na.rm=TRUE),var(y,na.rm=TRUE),sd(y,na.rm=TRUE),median(y,na.rm=TRUE),min(y,na.rm=TRUE),max(y,na.rm=TRUE),max(y,na.rm=TRUE)-min(y,na.rm=TRUE)),3)
    S[1,]<-S1;S[2,]<-S2;S[3,]<-S[1,]-S[2,]
    colnames(S)=c("N","NA","Moyenne","Var","Sd","Med","Min","Max","Etendue")
    ifelse(is.null(names(x)),temp<-c("Variable_1","Variable_2","Delta"),temp<-c(names(x),names(y),"Delta"))
    rownames(S)=temp
    
    Q[1,]<-quantile(x,probs=c(0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99),na.rm=TRUE)
    Q[2,]<-quantile(y,probs=c(0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99),na.rm=TRUE)
    Q[3,]<-Q[1,]-Q[2,]
    colnames(Q)<-c("1%","2.5%","5%","10%","50%","90%","95%","97.5%","99%")
    rownames(Q)=temp
    
    C[1,1]<-ifelse(round(cor.test(x,y,type="p")$p.value,2)>0.01,round(cor.test(x,y,type="p")$p.value,2),"<0.01")
    C[1,2]<-ifelse(round(cor.test(x,y,type="s")$p.value,2)>0.01,round(cor.test(x,y,type="s")$p.value,2),"<0.01")
    colnames(C)<-c("Pearson","Spearman")
    rownames(C)<-c("")
    C<-noquote(C)
    
    N[1]<-round(shapiro.test(x)$p.value,2)
    N[2]<-round(shapiro.test(y)$p.value,2)
    N[3]<-paste(ifelse(N[1]>0.05,1,0)+ifelse(N[2]>0.05,1,0),"/ 2")
    names(N)<-c(paste("Shapiro",temp[1]),paste("Shapiro",temp[2]),"Total")
    N<-noquote(N)
    
    if(appa=="FALSE"){
      T[1,1]<-round(try(t.test(x,y)$p.value),2);T[1,2]<-round(try(wilcox.test(x,y)$p.value),2)
      ifelse(try(t.test(x,y)$p.value)>0.05,res.t.test<-"Ho",res.t.test<-"H1")
      ifelse(try(wilcox.test(x,y)$p.value)>0.05,res.wilcox<-"Ho",res.wilcox<-"H1")
      T[2,1]<-res.t.test;T[2,2]<-res.wilcox
      colnames(T)<-c("t.test","wilcoxon")
      rownames(T)<-c("p.valeur","Hyp")
    }
    T<-noquote(T)
    
    if(appa=="TRUE"){
      T[1,1]<-round(try(t.test(x,y,paired=TRUE)$p.value),2);T[1,2]<-round(try(wilcox.test(x,y,paired=TRUE)$p.value),2)
      ifelse(try(t.test(x,y,paired=TRUE)$p.value)>0.05,res.t.test<-"Ho",res.t.test<-"H1")
      ifelse(try(wilcox.test(x,y,paired=TRUE)$p.value)>0.05,res.wilcox<-"Ho",res.wilcox<-"H1")
      T[2,1]<-res.t.test;T[2,2]<-res.wilcox
      colnames(T)<-c("t.test","wilcoxon")
      rownames(T)<-c("p.valeur","Hyp")
    }
    result<-list(Stats=S,Quantile=Q,Correlation=C,Normalite=N,Comparaison=T)
  }
  
  
  ########################################################## DEUXIEME CAS : COMPARAISON DE DEUX FACTEURS       ####################################
  if(cas==1)	  		
  {
    x<-as.factor(x)
    y<-as.factor(y)
    if(length(intersect(levels(x),levels(y)))==0){result="Erreur : Les variables demandees n'ont aucune modalite en commun."}
    else{
      cas=3
      Nombre.mod.x<-nlevels(x)
      Nombre.mod.y<-nlevels(y)
      
      D<-matrix(99,nrow=2,ncol=2)
      D[1,1]<-length(x);D[1,2]<-length(y)
      D[2,1]<-sum(is.na(x));D[2,2]<-sum(is.na(y))
      colnames(D)<-c("N1","N2")
      rownames(D)<-c("N","NA")
      
      max.lev<-levels(as.factor(c(as.vector(x),as.vector(y))))
      Tx<-Ty<-matrix(0,ncol=2,nrow=length(max.lev))
      tablx<-table(x);pourx<-round(table(x)/length(x[is.na(x)==F]),3)
      for(i in 1:length(max.lev)) {try(Tx[i,1]<-tablx[names(tablx)==max.lev[i]],silent=TRUE)}
      for(i in 1:length(max.lev)) {try(Tx[i,2]<-pourx[names(tablx)==max.lev[i]],silent=TRUE)}
      tably<-table(y);poury<-round(table(y)/length(y[is.na(y)==F]),3)
      for(i in 1:length(max.lev)) {try(Ty[i,1]<-tably[names(tably)==max.lev[i]],silent=TRUE)}
      for(i in 1:length(max.lev)) {try(Ty[i,2]<-poury[names(tably)==max.lev[i]],silent=TRUE)}
      Tdelta<-round(Tx-Ty,3);
      Tx.Ty<-
        noquote(
          cbind(
            Tx,
            rep("  |  ",length(max.lev)),
            Ty,
            rep("  |  ",length(max.lev)),
            Tdelta
          )
        )
      
      colnames(Tx.Ty)<-c("N_1"," %","","N_2"," %","","DELTA","%")
      rownames(Tx.Ty)<-max.lev
      
      Txy<-table(x,y)
      p.val.khi2<-try(chisq.test(Txy)$p.value,silent=TRUE)
      p.val.Mcnem<-try(mcnemar.test(as.matrix(table(x,y)))$p.value,silent=TRUE)
      p.val.fish<-try(fisher.test(Txy)$p.value,silent=TRUE)
      if(appa=="FALSE"){
        T<-matrix(99,ncol=2,nrow=2)
        T[1,]<-round(cbind(p.val.khi2,p.val.fish),2)
        colnames(T)<-c("Chi2","F.exact")
        rownames(T)<-c("p.val","Hyp")
        ifelse(p.val.khi2>0.05,T[2,1]<-"Ho",T[2,1]<-"H1")
        ifelse(p.val.fish>0.05,T[2,2]<-"Ho",T[2,1]<-"H1")
        T<-noquote(T)
      }
      else{T<-matrix(99,ncol=1,nrow=2)
      T[1,]<-round(p.val.Mcnem,2)
      colnames(T)<-c("McNemar")
      rownames(T)<-c("p.val","Hyp")
      ifelse(p.val.Mcnem>0.05,T[2,1]<-"Ho",T[2,1]<-"H1")
      T<-noquote(T)
      }
      result<-list(Effectifs=D,Descriptif=Tx.Ty,Tableau_croise=Txy,Tests=T)
    }
  }# fin si 
  if((cas!=1) & (cas!=2) & (cas!=3) ){result<-"Erreur: Le type de variable n'est pas reconnu, merci de votre comprehension"}
  print(result)
  
}# fin fonction
