# burt.r
# written by JuG
# August 06 2019


#' Make a disjonctif table
#' @author JuG
#' @description 
#' Does the same as tab.disjonctif function from the  FactoMineR package (but slower :-) and only on vectors)
#' @param x a factor vector
#' @details 
#' @examples 
#' fact <-sample(x = LETTERS[1:5], size=10, replace=T)
#' burt(fact)
#' require(FactoMineR)
#' tab.disjonctif(dtf)
#' @return 
#' @export


burt<-function(x){
  nbre.colonnes	<-nlevels(as.factor(x))
  nbre.lignes		<-length(x)
  MatRES<-matrix(NA,nrow=nbre.lignes,ncol=nbre.colonnes)
  for(j in 1:nbre.colonnes){MatRES[,j]<-ifelse(as.factor(x)==levels(as.factor(x))[j],1,0  ) } 
  colnames(MatRES)<-levels(as.factor(x))
  return(MatRES)
}