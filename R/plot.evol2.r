# plot.evol2.r
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


plot.evol2<-function(DT,groups,main,etyp=0) { 
  
  par.groupes<-0
  if(missing(main)){main=""}
  if(!missing(groups)){par.groupes=1}
  N  	<-dim(DT)[1]
  Nt	<-dim(DT)[2]
  noms<-colnames(DT)
  xmin<-0.5
  ymin<-0.9*min(DT,na.rm=TRUE)
  ymax<-1.1*max(DT,na.rm=TRUE)
  xmax<-Nt+0.5
  if(par.groupes!=1){
    plot(c(0,0),col="white",ylab="",xlab="",xaxt="n", xlim=c(xmin,xmax),ylim=c(ymin,ymax),main=main)
    
    axis(1,at=1:Nt,labels=noms)
    moyenne<-rep(NA,Nt)
    for(m in 1:Nt){moyenne[m]<-mean(DT[,m],na.rm=TRUE)}
    for(i in 1:N){
      v<-DT[i,]
      points(v,col=i);lines(v,col=i)
      points(moyenne,col="red",lwd=3)
      lines(moyenne,col="red",lwd=4)
    }
    legend("topleft", c("mean"), col = 2,
           text.col = "green4", lty = c(1),lwd=4,
           merge = TRUE, bg = "gray90")
  } 
  if(par.groupes==1){
    Ngroupes<-nlevels(as.factor(groups))
    plot(c(0,0),col="white",ylab="",xlab="",xaxt="n", xlim=c(xmin,xmax),ylim=c(ymin,ymax),main=main)
    axis(1,at=1:Nt,labels=noms)   
    M<-matrix(NA,nrow=nlevels(as.factor(groups)),ncol=Nt)
    for(c in 1:Nt){  M[,c]<-tapply(DT[,c],groups,mean,na.rm=TRUE) }   
    for(i in 1:Ngroupes){ v<-M[i,]; points(v,col=i+1);lines(v,col=i+1,lwd=4)}
    legend("topleft", as.character(levels(as.factor(groups))), col = 1:Ngroupes+1,
           text.col = "green4",lwd=4,
           merge = TRUE, bg = "gray90")
  }
  
  if(par.groupes==1 & etyp==1){ 
    
    Niveaux<-levels(as.factor(groups))
    if(length(Niveaux)==2){
      for(i in 1:Nt){
        s1<-c(mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        s2<-c(mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        segments(i-0.05,s1[1],i-0.05,s1[2],col="red",lwd=4)
        segments(i+0.05,s2[1],i+0.05,s2[2],col="green",lwd=4)
      }}
    if(length(Niveaux)==3){
      for(i in 1:Nt){
        s1<-c(mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[1]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[1]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        s2<-c(mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[2]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[2]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        s3<-c(mean(DT[,i][groups==Niveaux[3]],na.rm=TRUE)-1.96*sd(DT[,i][groups==Niveaux[3]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])),mean(DT[,i][groups==Niveaux[3]],na.rm=TRUE)+1.96*sd(DT[,i][groups==Niveaux[3]],na.rm=TRUE)/sqrt(length(DT[,i][!is.na(DT[,i])])))
        
        segments(i-0.05,s1[1],i-0.05,s1[2],col="red",lwd=4)
        segments(i,s2[1],i,s2[2],col="green",lwd=4)
        segments(i+0.05,s3[1],i+0.05,s3[2],col="blue",lwd=4)
        
      }}
    
    if(length(Niveaux)>3){print("L'option etyp n'est pas utilisable pour plus de 3 groupes, au risque d'avoir un graphique trop charge")}
    
  }
  
  
  
}
