# dputB.r
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



dputB<-function(x){
  
  
  nomX<-deparse(substitute(x))
  
  x<-as.numeric(x)
  
  if(length(x)==1){
    cat("\n\n Cette fonction ne traite pas les elements uniques \n\n")
  }
  
  if(is.vector(x) & length(x)>1){
    cat( "\nlist(\n\n",nomX,"=c(", paste(x,collapse=",")     ,")\n\n)\n\n" )
  }
  
  if(is.data.frame(x)|is.matrix(x)){
    
    
    sortie=NULL
    for(i in 1:dim(x)[2]){
      sortie[i]<-paste(colnames(x)[i],"=c(",paste(x[,i],collapse=","),")",sep="")
    }
    ttes_sorties<-paste(sortie,collapse=",")
    
    
    
    cat(	"\n\n\nlist(\n\n",nomX,"= structure(\n\t.Data=c(",		
         paste(	as.vector(t(x)) , collapse=","),
         "),\n\t.Dim=c(",
         paste(dim(x),collapse=","),
         ")\n\t)     \n)\n\n\n"
    )
  }
}