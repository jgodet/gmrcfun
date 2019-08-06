# tableaux.r
# written by JuG
# August 05 2019


#' Do something
#' @author JuG
#' @description 
#' @param X
#' @param Y
#' 
#' @details 
#' @examples 
#' X <- sample(x = letters[1:2],size = 20, replace = T)
#' Y <- sample(x = letters[3:4],size = 20, replace = T)
#' tableaux(X,Y, latex=T)
#' @return 
#' @export



tableaux<-function(X,Y,latex=0,p.val=T,entete=0,caption=0){
  if(!require(TraMineR)){install.packages('TraMineR')}
  library(TraMineR)
  options(warn=-pi)
  if(!is.data.frame(X)){
    nomX<-deparse(substitute(X))
    nomY<-deparse(substitute(Y))
    print(paste("Tableau Croise entre",deparse(substitute(X)),"(X) et",deparse(substitute(Y)),"(Y)"))
    print(addmargins(table(X,Y)))
    paste("\n","##---------------------##","\n")
    print("Proportions en ligne (en %)")
    x2                               <-round(addmargins(100*prop.table(addmargins(table(X,Y),1),1),2),2)
    rownames(x2)<-c(rownames(x2)[-length(rownames(x2))],"")
    print(x2)
    print("##---------------------##")
    print("Proportions en colonne (en %)")
    x3              <-round(addmargins(100*prop.table(addmargins(table(X,Y),2),2),1),2)
    colnames(x3)<-c(colnames(x3)[-length(colnames(x3))],"")
    print(x3)
    print("##---------------------##")
    print("Proportions du total (en %)")
    print(round(addmargins(prop.table(table(X,Y)))*100,2))
    print("##---------------------##")
    print("Valeurs theoriques")
    print(round(chisq.test(X,Y,correct = FALSE)$expected,2))
    print("##---------------------##")
    print(chisq.test(X,Y,correct = FALSE))
    print("##---------------------##")
    print(fisher.test(X,Y))
  }
  if(is.data.frame(X))
  {
    N.variables             <-dim(X)[2]
    for(i in 1:N.variables){if(nlevels(as.factor(X[,i]))>1){
      entete                  <-paste("Croisement de la variable",colnames(X)[i],"(ligne) et ",deparse(substitute(Y)),"(colonne)")
      tableaux(X[[i]],Y,latex=1,entete=0,caption=entete)}else{
        titre.unemod    <-paste(colnames(X)[i], " : Variable n'ayant qu'une seule modalite")
        cat("\n");cat(titre.unemod);cat("\n")}
    }
  }
  return()
}
