# desc.r
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

desc<-function(D,modmax=7){
  
  if(is.data.frame(D))	{cas=1;desc.table(D,modmax=modmax)}	                             	# DATA.FRAME
  if(is.numeric(D))	  	{cas=2;r<-desc1(D,modmax=modmax);print(r)}	                        # VARIABLE NUMERIQUE ( Quali ou Quanti )
  if(is.factor(D))		  {cas=3;r<-desc1(D,modmax=modmax);print(r)}                        	# VARIABLE FACTEUR
  if(all(cas!=c(1,2,3))){print("Variable inconnue_ Verifiez la source")}
}