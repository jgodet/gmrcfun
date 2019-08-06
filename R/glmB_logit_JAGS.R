# Utile pour glmB
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#'  Utile pour glmB
#' @description  Utile pour glmB
#' @export

glmB_logit_JAGS<-function(formul,data=NULL,Niter=105000,Nburnin=5000,Nthin=5,Prior=NULL,Seuilmonit=NULL,Diagno=TRUE,Graph=TRUE){


if(!require(pROC))install.packages('pROC'); library(pROC)

variables_Matrice       <-data.frame(get_all_vars(formul,data=data))
variables_Matrice 	  	<-variables_Matrice[complete.cases(variables_Matrice),]
variable_reponse        <-variables_Matrice[,1]
variable_explica        <-data.frame(variables_Matrice[,2:length(variables_Matrice)])
nom_covariables 	    	<-colnames(variables_Matrice)[-1]

type=NULL
for(i in 1:length(variable_explica)) {type[i]<-ifelse(is.factor(variable_explica[,i]),1,0)}

nombre_variables        <-length(variable_explica)
design_mat              <-cbind(1,variable_explica)
nombre_coef_fixes       <-length(design_mat)
design_mat_burtee       <-burt_design(design_mat)$RES[,burt_design(design_mat)$num_ref==0]


design_mat_burtee	    	<-model.matrix(formul,data=data)
nombre_coef_burtee	    <-ncol(design_mat_burtee)


#####################################################
###	verifier les seuils de monitorage	 	#####
#####################################################



message_Seuilmonitor<-paste("Erreur:\n
La longueur du vecteur de seuil a monitorer n'est pas egale au nombre de parametres du modele. 
La vecteur de seuils doit etre de type numeric et de longueur", nombre_coef_burtee,
"\n (L'intercept est un parametre a prendre en compte)\n
Par exemple, \n Les distributions sont par defaut comparees a 0, et le vecteur est:" )

Seuilmonitor_defaut<-rep(0,nombre_coef_burtee)



if(is.null(Seuilmonit)){Seuilmonitor<-Seuilmonitor_defaut}else{
if(!is.vector(Seuilmonit)|length(Seuilmonit)!=nombre_coef_burtee){
cat(message_Seuilmonitor)
cat("\n")
cat(Seuilmonitor_defaut)
cat("\n")
stop("",call. = FALSE)
}else{
Seuilmonitor<-Seuilmonit
}
}

#####################################################
###	verifier si la matrice de priors est OK 	#####
#####################################################

if(is.null(Prior)){Priors<-rbind(rep(0,nombre_coef_burtee),rep(0.1,nombre_coef_burtee))}else{Priors<-Prior}
message_dimensions_matrice<-paste("Erreur:\n
La matrice des aprioris n'est pas au bon format. 
La matrice des priors doit etre de dimension: 2 lignes et ",nombre_coef_burtee," colonnes \ncorrespondants:
\ten ligne a la moyenne et la precision 
\ten colonne au nombre de coefficients du modele.(Avec Intercept)\n
Par exemple, \n Le prior peu informatif pour chaque coef est une N(0,0.1), \n La matrice par defaut est:\n")

Priors_non_informatifs<-	rbind(	rep(0,nombre_coef_burtee),
									rep(0.1,nombre_coef_burtee))

colnames(Priors_non_informatifs)<-colnames(design_mat_burtee)
rownames(Priors_non_informatifs)<-c("moy","prec")
if(!is.matrix(Priors)){
				cat(message_dimensions_matrice);
				print(Priors_non_informatifs);
				cat("\n")
				stop("",call. = FALSE)
				}else{
if((dim(Priors)[1]!=2)|(dim(Priors)[2]!=nombre_coef_burtee)){
				cat(message_dimensions_matrice);
				print(Priors_non_informatifs);
				cat("\n");
				stop("",call. = FALSE)}
}

########### FIN PRIORS ##############################
#---------------------------------------------------------
library(R2jags)

modelH<-function(){

for( i in 1 : N ) {Y[i]~dbern(p[i]) 

for( j in 1 : nbcoefx ){monome[i,j]     <- designmat[i,j]*beta[j]}                                                                                                 
                        logit(p[i])     <- sum(monome[i,])   } 

for( j in 1 : nbcoefx ){beta[j]         ~  dnorm(Matrice_prior[1,j],Matrice_prior[2,j])
                        Pr_beta_pos[j]  <- step(beta[j]-Seuilmonitor[j])
                        RC[j]           <- exp (beta[j])}

}

donnees<-list(      Y             	= variable_reponse,
                    N             	= length(variable_reponse) ,
                    designmat       = as.matrix(design_mat_burtee),
                    nbcoefx         = nombre_coef_burtee,
					            Matrice_prior	= Priors,
					            Seuilmonitor	= Seuilmonitor
                         )

parametre       <-c("beta","RC","Pr_beta_pos")
initi<-list(list(beta=rep(0.2,nombre_coef_burtee)))
modele  <-jags(         data                    = donnees,
                        inits                   = initi,
                        parameters.to.save      = parametre,     
                        model.file              = modelH,
                        n.chains                = 1,
                        n.iter                  = Niter,
                        n.burnin                = Nburnin,
                        n.thin                  = Nthin,
					            	progress.bar			="text")

####    Question d'affichage MATRICE RESULTATS          ###############################



indexPrbeta	<-grep  ("Pr_beta_pos"      ,rownames(modele$BUGSoutput$summary))
indexRRbeta	<-grep  ("RR"               ,rownames(modele$BUGSoutput$summary))
indexRCbeta	<-grep  ("RC"               ,rownames(modele$BUGSoutput$summary))
indexbeta	<-grep  ("beta"             ,rownames(modele$BUGSoutput$summary))[-indexPrbeta]
indexalpha	<-grep  ("alpha"            ,rownames(modele$BUGSoutput$summary))
indexdevia	<-grep  ("deviance"         ,rownames(modele$BUGSoutput$summary))

RET         <-modele$BUGSoutput$summary[c(indexbeta,indexRCbeta,indexRRbeta,indexPrbeta,indexalpha,indexdevia),]
DIC	        <-c(DIC=modele$BUGSoutput$DIC,pD=modele$BUGSoutput$pD)


nom_complet <-colnames(design_mat_burtee	)[-1]
rownames(RET)<-c(
                        c("int",nom_complet),
        paste("OR",     c("int",nom_complet)),
        paste("Prob",   c("int",nom_complet),">",Seuilmonitor),
        "deviance")


#############################################################
###         Check si univariee qualitatif        #############
#############################################################

UniQuali    <-(nombre_variables==1 & is.factor(model.frame(formul)[,2]))

#####################################################################################
#### Diagnostiques si continue ou multivariee quali / quanti             #############
#####################################################################################
if(Diagno & !UniQuali){
  install.packages('pROC')
  require('pROC')
coefficients_moy        <-RET[1:nombre_coef_burtee,1]
predictions             <-exp(design_mat_burtee%*%coefficients_moy)/(1+exp(design_mat_burtee%*%coefficients_moy))

y                       <-variable_reponse
if(nombre_variables==1){x<-variable_explica[,1]}else{x<-predictions}

y           <-as.numeric(y)
x           <-as.numeric(x)
predictions <-as.numeric(predictions)

rocobj                  <- plot.roc(y,predictions, percent=TRUE,ci=TRUE,print.auc=TRUE)                                         

AUC                     <-c(    round(rocobj$ci[1],2),
                                round(rocobj$ci[2],2),
                                round(rocobj$ci[3],2))
#ci.thresholds(rocobj)
optimums                <-ci(rocobj, of="thresholds", thresholds="best")
plot(optimums) 
best.cut                <-as.numeric(rownames(round(optimums$sensitivity,2))) # meilleur cut sur Y
best.cut.X              <-(log((best.cut )/(1-best.cut ))-RET[1,1])/RET[2,1]

# Cas particulier ou il n'y a pas de meilleur seuil, tout interrompre
if(all(is.infinite(	best.cut	))){
print(modele) ; 
cat("\n\n\nERREUR : il semble que les meilleurs seuils de dichotomie de la variable soient -Inf et + Inf.\n" )
cat("Le calcul des performances diagnostiques est compromis. Vous pouvez relancer la commande\n")
cat("en utilisant l'option Diagno=FALSE\n") ;
stop(" ")}


if(nombre_variables>1){best.cut.X<-best.cut}    # si plusieurs variables travailler sur predictions
Nombre_Seuils           <-length(best.cut)
if(Nombre_Seuils==1){   best.cut.X<-t(best.cut.X)   }
best.sen                <-round(optimums$sensitivity,2)
best.spe                <-round(optimums$specificity,2)
rownames(best.sen)      <-paste("( Seuil:",round(best.cut.X,2),") Sens")
rownames(best.spe)      <-paste("( Seuil:",round(best.cut.X,2),") Spec")
V1                      <-matrix(nrow=Nombre_Seuils,ncol=4);colnames(V1)=c("VP","VN","FP","FN")
V2                      <-matrix(nrow=Nombre_Seuils,ncol=4);colnames(V2)=c("VPP","VPN","Exactitude","Taux d erreur")
rownames(V1)<-rownames(V2)<-paste("( Seuil:",round(best.cut.X,2),") ")

TABLEAUX                <-list()

for(i in 1:Nombre_Seuils){
y2                      <-y
SENSdecroissant         <-(median(x[y==0]))>(median(x[y==1]))
                    x2  <-ifelse(x>best.cut.X[i],1,0)
if(SENSdecroissant){x2  <-ifelse(x<best.cut.X[i],1,0)}
T                       <-table(x2,y2)  
                    rownames(T)<-paste(c("(x<","(x>"),round(best.cut.X[i],2),")",sep="")
if(SENSdecroissant){rownames(T)<-paste(c("(x>","(x<"),round(best.cut.X[i],2),")",sep="")}
VP                      <-T[2,2]
VN                      <-T[1,1]
FP                      <-T[2,1]
FN                      <-T[1,2]
V1[i,]                  <-cbind(VP,VN,FP,FN)
VPP                     <-round(VP/(VP+FP),4)
VPN                     <-round(VN/(VN+FN),4)
Exact                   <-round((VP+VN)/(VP+VN+FP+FN),4)
Erreur                  <-round((FP+FN)/(VP+VN+FP+FN),4)
V2[i,]                  <-100*cbind(VPP,VPN,Exact,Erreur)
TABLEAUX[[i]]<-T
}
        
####    RESULTATS       ####
SeSp                <-rbind(best.sen,best.spe)
SeSp                <-SeSp[order(match(rownames(SeSp),sort(rownames(SeSp)))),]  # trier les Se et Sp pour l'affichage
seuils              <-cbind(round(best.cut.X,3),round(best.cut,3))
rownames(seuils)    <-rep("Seuil",Nombre_Seuils)
if(Nombre_Seuils>1){
    V1              <-V1[order(match(rownames(V1),sort(rownames(V1)))),]        # trier les r?sultats pour l'affichage
    V2              <-V2[order(match(rownames(V2),sort(rownames(V2)))),]        # trier les r?sultats pour l'affichage
    seuils          <-seuils[order(seuils[,1]),]
    rownames(seuils)<-paste(rep("Seuil n?",Nombre_Seuils),1:Nombre_Seuils)          
                    } # fin if Nombre_seuils>1
colnames(seuils)    <-c("Variable","Pr?dictions")

} # fin if diagno & !Uniquali


#####################################################################################
#### Diagnostiques si une seule qualitative                            #############
#####################################################################################

if(Diagno & UniQuali){
LogistUniQuali<-logist(variable_reponse,variable_explica[,1])
TABLEAUX    <-LogistUniQuali$Table
V1          <-LogistUniQuali$Valeurs
SeSp        <-LogistUniQuali$Performance
V2          <-LogistUniQuali$OR  
                     }

#####################################################################################
#### Affichage des RESULTATS                                            #############
#####################################################################################

ParamWB             <-matrix(NA,ncol=1,nrow=4)
ParamWB[1,1]        <-1
ParamWB[2,1]        <-Niter
ParamWB[3,1]        <-Nburnin
ParamWB[4,1]        <-Nthin
rownames(ParamWB)   <-c("Nbr Chaines","Nbr Iterations","Nbr Brulees","Nbr Thin")
colnames(ParamWB)   <-""
colnames(Priors)    <-colnames(design_mat_burtee)
rownames(Priors)    <-c("moy","prec")

if(Diagno & !UniQuali){

if(nombre_variables>1){seuils<-seuils[2]}

RESULTATS<-list(
                    MCMC=noquote(ParamWB),
                    Priors=Priors,
                    DIC=DIC,
                    Resultats=RET,
                    seuils=seuils,
                    TableauCroise=TABLEAUX,
                    Tableau=V1,
                    Performance=SeSp,
                    AUC=paste("AUC:", AUC[2],"[",AUC[1],";",AUC[3],"]"  ),
                    Performance2=V2)
}

if(Diagno & UniQuali){
RESULTATS<-list(
                    MCMC=noquote(ParamWB),
                    Priors=Priors,
                    DIC=DIC,
                    Resultats=RET,
                    TableauCroise=TABLEAUX,
                    Tableau=V1,
                    Performance=SeSp)
                    #OR=V2)
}

if(!Diagno){
RESULTATS<-list(    MCMC=noquote(ParamWB),
                    Priors=Priors,
                    DIC=DIC,
                    Resultats=RET)
}

######
###
#       debug eventuel
###
######

# empty

##################################################################################
cat("---------------------------");cat("\n")
cat("REGRESSION LOGISTIQUE BAYESIENNE");cat("\n")
cat("---------------------------");cat("\n")
cat("---Graphique: Diagnostics de convergence MCMC---");cat("\n")
cat("---------------------------");cat("\n")

#####################################################################################
###################        Diagnostics de CV             ##############################
#####################################################################################

if(Graph){plot_Conv_Variables_MAT_JAGS(modele,nom_complet);    par(mfrow=c(1,1))}


#####################################################################################
###################        WARNINGS                    ##############################
#####################################################################################
if(nombre_variables>1){
    warning("Le mod?le comporte plusieurs variables:  
le seuil ainsi que les performances du mod?les sont 
calcul?es pour les pr?dictions et non pour les valeurs d une variable",call. = FALSE)}

if(Diagno & !UniQuali & nombre_variables==1 & nlevels(as.factor(model.frame(formul)[,2]))<5){
    warning("La variable explicative pr?sente peu de modalit?s.  
Sous cette syntaxe, elle est consid?r?e comme quantitative.
Utilisez la commande as.factor() si qualitative.",call. = FALSE)}

return(RESULTATS)
}