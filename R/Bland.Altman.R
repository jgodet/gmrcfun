# Analyse de Concordance de Bland et Altman
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Analyse de Concordance de Bland et Altman
#'
#' @description Analyse de Concordance de Bland et Altman
#' @details  manque une ref pour répétition des sujets
#' 
#' @param x,y Deux vecteurs de valeurs quantitatives
#' @param alpha le seuil de significativite a definir 
#' @param rep.mes si TRUE, les donnees sont des donnees repetees
#' @param sujet Le vecteur sujet a passer en cas de donnees repetees.
#' @examples 
#' d <- data.frame( SubjectID = LETTERS[1:20], xval = rnorm(20,10,1), yval = rnorm(20,10,1) + rnorm(20,.2,1))
#' Bland.Altman(d$xval, d$yval)
#' 
#' de <- data.frame( SubjectID = as.factor(rep(LETTERS[1:5],each=4)), xval = rnorm(20,10,1), yval = rnorm(20,10,1) + rnorm(20,.2,1))
#' Bland.Altman(x=de$xval, y = de$yval,alpha = .1,rep.mes = TRUE, sujet = de$SubjectID)
#' 
#' @export

Bland.Altman<-function(x,y,alpha=.05,rep.mes=FALSE,sujet,...){

#**********************************************************************
#* Construire un graphique de Bland et Altman
#* 1. Calculer les constantes
#* 2. Calculer la difference moyenne
#* 3. Calculer l'ecart-type de la diff?rence
#* 4. Calculer les bornes inferieures et superiueres de l'intervalle de confiance
#* 5. Dessiner le graphique
#**********************************************************************
 
#*** 1. Calculer les constantes
z<-qnorm(1-alpha/2)  	## valeur de z correspondant au risque alpha
d<-x-y               		## difference paire par paire
m<-(x+y)/2           		## moyenne paire par paire
 
#*** 2. Calculer la diff?rence moyenne
diff.moy<-mean(d,na.rm=TRUE)
 
#*** 3. Calculer l'ecart-type de la diff?rence
if(rep.mes==FALSE){et.d=sqrt(var(d,na.rm=TRUE))}
else{
#*** 3a. S'assurer que le sujet est une variable categorielle
if(!is.factor(sujet)) sujet<-as.factor(sujet)
#*** 3b. Extraire les informations du mod?le
n<-length(levels(sujet))      		# Nombre de sujets
modele<-aov(d~sujet)          		# Analyse de variance a un facteur
MSB<-anova(modele)[[3]][1]    	# Degres de liberte
MSW<-anova(modele)[[3]][2]    	# Sommes des Carres
 
#*** 3c. Calculer le nombre de paires completes pour chaque sujet
paires<-NULL
for(i in 1:length(levels(as.factor(sujet)))){
paires[i]<-sum(is.na(d[sujet==levels(sujet)[i]])==FALSE)
}
Sig.dl<-(MSB-MSW)/((sum(paires)^2-sum(paires^2))/((n-1)*sum(paires)))
et.d<-sqrt(Sig.dl+MSW)
}
#*** 4. Calculer les bornes inferieures et superieures de l'intervalle de confiance
bsup<-diff.moy+z*et.d 
binf<-diff.moy-z*et.d


 
#*** 5. Dessiner le graphique
plot(m,d,abline(h=c(diff.moy,bsup,binf),col=c("blue","red","red")),las=1,lty=c(1,2,2),pch=16,main="Analyse de Bland et Altman", ylim = c(binf - et.d , bsup+et.d))
valeurs<-round(cbind(binf,diff.moy,bsup),4)
colnames(valeurs)<-c("Borne inf","Moyenne","Borne sup")
if(rep.mes==FALSE) sortie<-list(limites=valeurs,variance=et.d^2)
else sortie<-list(limites=valeurs,variance=Sig.dl)
return(sortie)
}

