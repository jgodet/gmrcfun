# desql.r
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


desql<-function(X){
  
  nomX<-deparse(substitute(X))
  cat("\nDescriptif de la variable : ",nomX, "  \n\n") 
  if(!is.factor(X)){X<-as.factor(X)}
  nbl<-nlevels(X)
  aze<-matrix(NA,ncol=2,nrow=nbl+3)
  rownames(aze)<-c(levels(X),"Total","Non Manquants","MANQUANTS")
  colnames(aze)<-c("Effectifs","Proportions")
  tcr<-table(X)
  aze[1:(nbl),1]<-table(X,exclude=NULL)[1:(nbl)]
  aze[1:nbl,2]<-round(tcr*100/sum(tcr),digits=3)
  aze[nbl+1,1]<-sum(tcr)
  aze[nbl+1,2]<-100
  nbmq<-table(X,exclude=NULL)[nbl+1]
  nbmq<- ifelse(is.na(nbmq), 0, nbmq)
  aze[nbl+3,1]<-nbmq
  aze[nbl+3,2]<-nbmq*100/length(X)
  aze[nbl+2,1]<-length(X)-aze[nbl+3,1]
  aze[nbl+2,2]<-100-aze[nbl+3,2]
  
  return(aze)
}