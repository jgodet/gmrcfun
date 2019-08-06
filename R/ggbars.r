# ggbars.r
# written by JuG
# August 06 2019


#' Do something but what???
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' Y <- rnorm(10)
#' ggbars(Y)
#'
#' @return 
#' @export


ggbars<-function(Y,X="Variable"){
  if(!require(ggplot2)){install.packages('ggplot2')}
  library(ggplot2)
  nomX<-deparse(substitute(X))
  nomY<-deparse(substitute(Y))
  
  if(length(X)==1 & X[1]=="Variable"){X<-rep("",length(Y))}
  
  
  pc<-as.numeric(as.character(as.vector(prop.table(table(X,Y),1))))
  Groupe<-as.factor(rep(levels(as.factor(Y)),each=dim(prop.table(table(X,Y),1))[1]))
  G<-as.factor(rep(levels(as.factor(X)),dim(prop.table(table(X,Y),1))[2]))
  D<-data.frame(pc,Groupe,G)
  
  graph <- ggplot(data=D, aes(x=G, y=pc,fill=Groupe)) +                  
    geom_bar(stat="identity",position = "dodge",ymax=100) + xlab(nomX)+ ylab("%")+
    geom_text(aes(label =paste(round(pc*100,0),"%",sep=""),ymax=0),position=position_dodge(width=0.9), vjust=-0.25)
  
  return(graph) 
  
}
