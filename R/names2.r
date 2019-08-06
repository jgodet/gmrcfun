# names2.r
# written by JuG
# August 06 2019


#' lister les variables d'un jeu de donnees avec les numeros a cote		
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


names2<-function(D){
  n	<-length(D[1,])
  vec	<-1:n
  r	<-rep(NA,n)
  for(i in 1:n){
    r[i]	<-paste(names(D)[i],"(",vec[i],")")}
  return(noquote(r))
}