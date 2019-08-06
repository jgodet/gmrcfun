# plot.evol.r
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


plot.evol<-function(Trajectoires, Groupe=NULL,IC = FALSE, Moyenne = TRUE, Temps = NULL, label_x = "Temps",  label_y= "Valeur",  labels_ticks_x=NULL){
  
  # Verifications des arguments.
  # Trajectoires doit est une matrice de type int ou numeric
  if( !(
    is.matrix(Trajectoires) & 
    ( is.numeric(Trajectoires) | is.integer(Trajectoires)
    ))) stop("Trajectoire doit etre une matrice numerique")
  
  nbvisites <- ncol(Trajectoires)
  nbpatients <- nrow(Trajectoires)
  
  # Temps doit avoir la meme taille que le nombre de visites
  if(is.null(Temps)){
    Temps <- 1:nbvisites
  }else{
    if( !(length(Temps) == nbvisites & ( is.numeric(Temps) | is.integer(Temps)))) stop("Temps doit etre un vecteur numerique de la meme taille que le nombre de colonnes de la matrice")
  }
  
  
  
  # Transformer la matrice de large a long. TODO a ameliorer car cas particulier ici
  # En particulier, permettre que le jour de visite ne soit pas equidistant
  
  
  # Si pas de groupe specifie, attribuer le meme groupe a toutes les lignes
  if(is.null(Groupe)){
    Groupe <- rep(nbpatients,1)
  }
  
  
  # Creer la dataframe en version "long" adaptee pour ggplot (une colonne par dimension)
  dfTraj<-data.frame(
    valeurs = as.numeric(as.matrix(Trajectoires)), 
    patient = rep(1:nbpatients, times=nbvisites), 
    visites = rep(Temps , each=nbpatients),
    groupe = as.factor(rep(Groupe, times=nbvisites))
  )
  
  # Verifier que ggplot present
  if(!require(ggplot2)){install.packages('ggplot2')}
  require(ggplot2)
  
  ggtrajectoire<-ggplot() +
    geom_line(data=dfTraj, aes(x=visites, y=valeurs, group=patient)) 
  
  if(Moyenne){
    # Si la moyenne, creer la df moyenne
    moyennes<-aggregate( formula= valeurs~visites+groupe, data=dfTraj, FUN=mean)
    
    # TODO : nettoyer le alpha
    
    # Si un seul groupe, laisser les trajectoires en noir
    if(length(unique(Groupe))>1){
      ggtrajectoire <- ggtrajectoire + aes(color=groupe) 
    }else{
      ggtrajectoire <- ggtrajectoire  + scale_color_discrete(guide="none")
    }
    # Plotter la moyenne en plus
    ggtrajectoire <- ggtrajectoire + 
      aes(alpha= 0.8) +
      geom_line(data=moyennes, aes(color=groupe, size=3, x=visites,y=valeurs,group=groupe, alpha=1 )) +
      scale_alpha(range=c(0.5,1),guide="none")
  }
  
  if(IC & Moyenne){
    bornes <- function(x){
      t.test(x)$conf.int
    }
    bornesIC<-aggregate( formula= valeurs~visites+groupe, data=dfTraj, FUN=bornes)
    
    # Attention! Matrice dans la dataframe 
    if(!require(dfexplore)){install.packages('dfexplore')}
    library(dfexplore)
    bornesIC<-expand_dfmatrix(bornesIC)
    names(bornesIC) <- c("visites", "groupe", "ICmin", "ICmax")
    ggtrajectoire <- ggtrajectoire + geom_errorbar(data=bornesIC, size=2, width=0.3,position="dodge" ,aes(x=visites, ymin=ICmin, ymax=ICmax, group=groupe,color=groupe, alpha=1))
  }
  
  if(is.null(labels_ticks_x)){labels_ticks_x<-as.character(Temps)}
  
  # Ajouter des elements pour faire joli
  ggtrajectoire <- ggtrajectoire +
    scale_x_continuous(breaks = Temps, labels=labels_ticks_x) +
    scale_size(guide="none") +
    xlab(label_x) +
    ylab(label_y)
  
  return(ggtrajectoire)
}
