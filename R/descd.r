# descd.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description TODO voir ce que définit Var textuelles
#' @param 
#' @details 
#' @examples 
#' dtf <- data.frame(a =rnorm(20), 
#'                   b= sample(x = LETTERS[1:5], size  = 20,replace=T), 
#'                   c = sample(x = LETTERS[1:5], size  = 20,replace=T),
#'                   d = rep(date(),20))
#'dtf$c <- as.character(dtf$c)
#'descd(dtf)
#'
#' @return 
#' @export


descd<-function(D){
  
  
  if(is.data.frame(D)){situation=2}else{
    if(is.matrix(D))    {situation=1}else{situation=0}}
  if(situation==2 | situation==1) {
    n.lignes		<-dim(D)[1]
    n.col			<-dim(D)[2]
    N			    <-n.lignes*n.col
    #---------------------------------------------
    # Calcul du nombre de variables quanti et quali
    n.quanti<-n.quali<-n.txt<-n.autres<-0
    for(i in 1:n.col){
      x                   <-D[,i]
      modmax              <-7
      if(length(table(as.factor(x)))==1){nature.variable=-1}else{
        if(is.character(x)){nature.variable<-0;res<-"Variable textuelle"}
        if(is.factor(x)){
          if(nlevels(x)<modmax){nature.variable<-2}else{nature.variable<-3;res<-"Facteur nombreuses modalites, a verifier manuellement"}}else{
            if(is.numeric(x))
            { if (nlevels(as.factor(x))<modmax) {nature.variable<-2} else{nature.variable<-1}}
            else{nature.variable<-0     } }
      }
      if(nature.variable==1)                          {n.quanti   =n.quanti+1}
      if(nature.variable==2)                          {n.quali    =n.quali+1}}
    if(nature.variable==0)                          {n.txt      =n.txt+1}
    if (nature.variable==-1 | nature.variable==3)   {n.autres   =n.autres+1}}
  
  #----------------------------------------------------
  C.vide <- apply(D,2,function(x)all(is.na(x)))                                                                                     #colonnes vides
  lig.vides <- sum(apply(D,1,function(x)all(is.na(x))))                                                                     #nombre de lignes vides
  col.vides <- sum(C.vide)                                                                                                                           #nombre de colonnes vides
  dat.vides <- paste(sum(is.na(D)),"(",round(sum(is.na(D))/N*100,0),"%",")")
  sub.comp.lig <- dim(D[complete.cases(D[,-which(C.vide)]),])[1]                                            #nombre de lignes completes (sans les colonnes vides)
  sub.comp <- paste(sub.comp.lig," lignes, dans ", sum(!C.vide)," colonnes", sep="")
  
  #----------------------------------------------------
  
  RES1                    <-matrix(NA,nrow=3,ncol=1)
  RES1[1,1]               <-n.col
  RES1[2,1]               <-n.lignes
  RES1[3,1]               <-N
  colnames(RES1)          <-""
  rownames(RES1)          <-c("Nombre de colonnes","Nombre de lignes","Nombre de donnees")
  
  RES2                    <-matrix(NA,nrow=4,ncol=1)
  RES2[1,1]               <-col.vides
  RES2[2,1]               <-lig.vides
  RES2[3,1]               <-dat.vides
  RES2[4,1]               <-sub.comp
  RES2                    <-noquote(RES2)
  colnames(RES2)          <-""
  rownames(RES2)          <-c("Nombre de colonnes vides","Nombre de lignes vides","Nombre de donnees manquantes","Dimensions ss-groupe complet")
  
  RES3                    <-matrix(NA,nrow=4,ncol=1)
  RES3[1,1]               <-n.quanti
  RES3[2,1]               <-n.quali
  RES3[3,1]               <-n.txt
  RES3[4,1]               <-n.autres
  colnames(RES3)          <-""
  rownames(RES3)          <-c("Nombre de Var. Quantitatives","Nombre de Var. Qualitatives","Nombre de Var. Textuelles","Nombre de Var. Nature Autre")
  
  RES<-list(Dim=RES1,N.A.=RES2,Nature=RES3)
  
  if (situation==0) {RES<-"1 seule variable a decrire, utilisez desc()"}
  return(RES)
}
