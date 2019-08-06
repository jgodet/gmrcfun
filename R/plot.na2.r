# plot.na2.r
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




plot.na2<-function(D,cumul=0,latex=0){
  if(cumul==2){D<-t(D)}
  setInternet2(TRUE)
  if(!require(TraMineR))install.packages('TraMineR'); library(TraMineR)
  D2<-ifelse(is.na(D),"NA","Present")
  D2.alphab<-c("NA","Present")
  D2.seq <- seqdef(D2,  xtstep = 2, alphabet = D2.alphab)
  
  par(mfrow=c(2,1))
  seqplot(D2.seq, border = NA,type="I", withlegend = "right",space=0,cpal=c("red","blue"),title="Valeurs manquantes par variable",ylab="Sujets") 
  # TAPIS
  if(cumul==1){
    seqdplot(D2.seq, border = NA, withlegend = "right",cpal=c("red","blue"),title="Valeurs manquantes par variable (cumulees)",ylab=" % Sujets" ) 
  }
  if(cumul==2){
    seqdplot(D2.seq, border = NA, withlegend = "right",cpal=c("red","blue"),title="Valeurs manquantes par sujet",ylab=" % Variable" )                                                    
  }
  cat("\n ")
  cat("\n ")
  cat("\n ")
  NbVariables<-dim(D)[2]
  matriceNA<-matrix(NA,nrow=NbVariables,ncol=3)
  for(i in 1:NbVariables){
    matriceNA[i,1]<-round(sum               (is.na(D[,i])))
    matriceNA[i,2]<-round(length            (is.na(D[,i])))
    matriceNA[i,3]<-round(sum               (100*is.na(D[,i]))/length       (is.na(D[,i])),2)
  }
  colnames(matriceNA)<-c("Nb.manquants","Nb.donnees","%")
  rownames(matriceNA)<-colnames(D)
  if(latex==1){
    return(xtable(matriceNA,digits=0))
  }else{
      return(matriceNA)
    }
}


