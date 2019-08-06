# pie.r
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


pie<-function(x,nomx,na.rm=FALSE){
  
  if(missing(nomx)) {nomx<-deparse(substitute(x))}
  if(na.rm==TRUE){x<-x[!is.na(x)]}
  
  
  if(sum(is.na(x))!=0){	levels(x)<-c(levels(x),"NA")
  x[is.na(x)]<-"NA"}
  
  effec           <-as.numeric(table(factor(as.character(x))))
  labls           <-names(table(factor(as.character(x))))
  zz                      <-data.frame(labls,effec)
  
  myTitle         <-paste("Proportions : Variable",nomx)
  percent <-paste(as.character(round(effec/sum(effec)*100,1)),"%")
  coord   <-effec/2 + c(0, cumsum(effec)[-length(effec)])
  
  g               <-ggplot(data.frame(labls,effec,coord,percent), aes(x="", y=effec, fill=labls)) + 
    geom_bar(width=1) + 
    coord_polar("y")+ 
    labs(x="")+labs(y="")+
    geom_text(aes(y =coord , label = percent), size=7) +
    guides( fill = guide_legend(label.position = "bottom",keywidth = 3, keyheight = 3,  title="",title.theme = element_text(size=rel(2),angle=0)))+
    theme(  legend.position="bottom",
            legend.text = element_text(colour = 'black', angle = 0, size = 13, hjust = 2, vjust = 2, face = 'bold'),
            plot.title = element_text(size = 20))+
    labs(title = myTitle,size=15)
  
  
  return(g)
}