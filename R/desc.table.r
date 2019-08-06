# desc.table.r
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


desc.table<-function(D,modmax=7){
  Nbre.variables<-length(D)
  for (i in 1:(Nbre.variables))
  {
    nom<-colnames(D)[i]
    print("_____________________________________")
    print(noquote(c("","","","","","","","","","")))
    print(noquote(paste("","","","","","","","","","","","","","","","","","",nom,"","","","","","","","","")))
    print("_____________________________________")
    d<-desc1(D[,i],modmax=modmax)
    print(d)
  }
}