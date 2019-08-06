# desc1.r
# written by JuG
# August 05 2019


#' Descriptive statistics of a given variable
#' @author JuG
#' @description 
#' @param x 
#' @details 
#' @examples 
#' x <- runif(100)
#' desc1(x)
#' y <- gl(n=2, k=30,labels = letters[1:2])
#' desc1(y)
#' @return 
#' @export


desc1<-function(x,modmax=7){
  #modmax<-7  # passe en argument
  if(length(table(as.factor(x)))==1){res<-paste("Variable n'ayant qu'une seule modalite",levels(as.factor(x)),sep="  :  ")}else{
    if(is.character(x)){nature.variable<-0;res<-"Variable textuelle"}
    if(is.factor(x)){if(nlevels(x)<modmax){nature.variable<-2}else{nature.variable<-3;res<-"Facteur nombreuses modalites, a verifier manuellement"}}else{
      if(is.numeric(x))
      { if (nlevels(as.factor(x))<modmax) {nature.variable<-2} else{nature.variable<-1}}
      else{nature.variable<-0;print("Variable non numerique")     } }
    
    nature<-ifelse(nature.variable==1,"quantitative","qualitative")
    
    
    if(nature.variable==1)                                                              # Dans le cas d'une variable quantitative
    {
      effectif              <-noquote(c(length(x),length(x[!is.na(x)]),100*round(length(x[!is.na(x)])/length(x),2),round(sum(is.na(x)),2),100*round(sum(is.na(x))/length(x),2)))
      names(effectif)       <-c("N","N.presents","%presents","NA","%NA")
      x1                    <-x[!is.na(x)]
      stats                 <-round(c(mean(x1,na.rm=TRUE),var(x1,na.rm=TRUE),sd(x1,na.rm=TRUE),median(x1,na.rm=TRUE)),3)
      names(stats)          <-c("Moyenne","Variance","Ecart-type","Mediane")
      quantiles             <-round(quantile(x,probs=c(0.01,0.025,0.05,0.1,0.25,0.5),na.rm=TRUE),3)
      quantiles2            <-round(quantile(x,probs=c(0.75,0.9,0.95,0.975,0.99),na.rm=TRUE),3)
      min.max               <-round(c(min(x1,na.rm=TRUE),max(x1,na.rm=TRUE),max(x1,na.rm=TRUE)-min(x1,na.rm=TRUE)),3)
      names(min.max)        <-c("min","max","etendue")
      p.val.shapiro         <-round(shapiro.test(x)$p.value,4);norm<-ifelse((p.val.shapiro>0.05),"Non Rejet Normalite","Rejet Normalite")
      Shap                  <-c(p.val.shapiro,norm)
      names(Shap)           <-c("p.valeur","Hypothese")
      res                   <-list(Effectifs=effectif,Type=nature,Stats=stats,Quantiles=quantiles,Quantiles=quantiles2,Extremes=min.max,Test_de_Shapiro_Wilk=Shap)
      #cat("\n")                                                                                                       # Affichage
      #cat("Variable",nature,"\n")
      #cat("\n")
      #cat("Effectifs","\n")
      #cat("\n")
      #cat("\t","N","\t","N.presents","\t","%presents","\t","NA","\t","%NA");cat("\n")
      #cat("\t",effectif[1],"\t",effectif[2],"\t","\t",effectif[3],"\t","\t",effectif[4],"\t",effectif[5],"\n")
      #cat("\n")
      #cat("Statistiques","\n")
      #cat("\n")
      #cat("\t","Moyenne","\t","Ecart-type","\t","Variance","\t","Mediane","\t");cat("\n")
      #cat("\t",stats[1],"\t",stats[2],"\t",stats[3],"\t","\t",stats[4],"\t","\n")
      #cat("\n")
      #cat("\t","2.5%","5.0%","10 %","25 %","50 %","75 %","90 %","95 %","97.5%");cat("\n")
      #cat("\t",quantiles[1],quantiles[2],quantiles[3],quantiles[4],quantiles[5],quantiles[6],quantiles[7],quantiles[8],quantiles[9])
      #cat("\n")
    }
    
    if(nature.variable==2)                                                                # Dans le cas d'une variable qualitative
    {
      effectif              <-noquote(c(length(x),length(x[!is.na(x)]),100*round(length(x[!is.na(x)])/length(x),2),round(sum(is.na(x)),2),100*round(sum(is.na(x))/length(x),2)))
      names(effectif)       <-c("N","Npres","%pres","NA","%NA")
      modalites             <-nlevels(as.factor(x))
      type                  <-paste(nature,"a",modalites,"modalites")
      tabl                  <-table(x)
      pour                  <-round(table(x)/length(x[!is.na(x)]),3)
      pourcumul             <-round(table(x)/length(x[!is.na(x)]),3);for(i in 2:length(pourcumul)){pourcumul[i]<-pourcumul[i-1]+pour[i]}
      IC.2.5<-IC.97.5       <-rep(0,modalites)
      for(i in 1:modalites){
        IC.2.5[i]             <- round(binom.test(tabl[i],sum(tabl))$conf.int[1],3)
        IC.97.5[i]            <- round(binom.test(tabl[i],sum(tabl))$conf.int[2],3)}
      T                     <-cbind(tabl,pour,pourcumul,IC.2.5,IC.97.5)
      
      colnames(T)           <-c("N","%","%cumul","IC_2.5","IC_97.5")
      res                   <-list(Effectifs=effectif,Type=type,Tableaux=T)
      #cat("\n")                                                                                                       # Affichage
      #cat("Variable",type,"\n")
      #cat("\n")
      #cat("Effectifs","\n")
      #cat("\n")
      #cat("\t","N","\t","N.presents","\t","%presents","\t","NA","\t","%NA");cat("\n")
      #cat("\t",effectif[1],"\t",effectif[2],"\t","\t",effectif[3],"\t","\t",effectif[4],"\t",effectif[5],"\n")
      #cat("\n")
      #cat("Tableaux","\n")
      #cat("\n")
      #cat("\t","N","\t","%","\t","%cumules","\t","ICinf","\t","ICsup","\n")
      #for(i in 1:length(T[,1])){
      #AF<-T[i,]
      #cat("\t",AF[1]);cat("\t");cat(AF[2]);cat("\t");cat(AF[3]);cat("\t","\t");cat(AF[4]);cat("\t");cat(AF[5]);cat("\n")
      #}
    }}
  return(res)
  #return(noquote(""))
  
} 

