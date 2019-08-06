# Utile pour glmB
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer
#

#' Utile pour glmB
#'  @description  Utile pour glmB
#' @export


glmB_gamma_JAGS<-function(formul,data=NULL,Niter=105000,Nburnin=5000,Nthin=5,Prior=NULL,Seuilmonit=NULL,Graph=TRUE){
variables_Matrice       <-data.frame(get_all_vars(formul,data=data))
variables_Matrice <-variables_Matrice[complete.cases(variables_Matrice),]
variable_reponse        <-variables_Matrice[,1]
variable_explica        <-data.frame(variables_Matrice[,2:length(variables_Matrice)])
nom_covariables <-colnames(variables_Matrice)[-1]

#####################################################
###	verifier si reponse 0	 	#####
#####################################################

message_Erreur0<-paste("Erreur:\n
La variable contient soit des 0, soit des valeurs negatives.  
La distribution GAMMA n'est definie que sur ]0;+Inf[ \n\n" )

if(sum(variable_reponse==0|variable_reponse<0 )==0){Variable_sans0<-TRUE}else{ 
cat(message_Erreur0)
cat("\n")
stop("",call. = FALSE)
}

###########################


type=NULL
for(i in 1:length(variable_explica)) {type[i]<-ifelse(is.factor(variable_explica[,i]),1,0)}

nombre_variables        <-length(variable_explica)
design_mat              <-cbind(1,variable_explica)
nombre_coef_fixes       <-length(design_mat)
design_mat_burtee       <-burt_design(design_mat)$RES[,burt_design(design_mat)$num_ref==0]


design_mat_burtee		<-model.matrix(formul,data=data)
nombre_coef_burtee	<-ncol(design_mat_burtee)


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



for( i in 1 : N ) {Y[i]~dgamma(a[i],b) 

a[i]<-mu[i]*b
for( j in 1 : nbcoefx ){monome[i,j]		<- designmat[i,j]*beta[j]}                                                                                                 
                  	log(mu[i]) 	<- sum(monome[i,])   } 

for( j in 1 : nbcoefx ){beta[j]		~  dnorm(Matrice_prior[1,j],Matrice_prior[2,j])
				Pr_beta_pos[j]	<- step(beta[j]-Seuilmonitor[j])
				RR[j] 		<- exp (beta[j])}
b~dgamma(0.01,0.01)

}


donnees<-list(          	Y             	= variable_reponse,
                        	N             	= length(variable_reponse) ,
                              designmat         = as.matrix(design_mat_burtee),
                              nbcoefx           = nombre_coef_burtee ,
					Matrice_prior	= Priors,
					Seuilmonitor	= Seuilmonitor  							  
                         )
parametre       <-c("beta","RR","Pr_beta_pos")
initi<-list(list(beta=rep(0.2,nombre_coef_burtee),b=0.2))
modele  <-jags(         data                    = donnees,
                        inits                   = initi,
                        parameters.to.save      = parametre,     
                        model.file              = modelH,
                        n.chains                = 1,
                        n.iter                  = Niter,
                        n.burnin                = Nburnin,
                        n.thin                  = Nthin,
						progress.bar			="text")



####	Question d'affichage MATRICE RESULTATS		###############################


indexPrbeta	<-grep  ("Pr_beta_pos",rownames(modele$BUGSoutput$summary))
indexRRbeta	<-grep  ("RR",rownames(modele$BUGSoutput$summary))
indexRCbeta	<-grep  ("RC",rownames(modele$BUGSoutput$summary))
indexbeta	<-grep  ("beta",rownames(modele$BUGSoutput$summary))[-indexPrbeta]
indexalpha	<-grep  ("alpha",rownames(modele$BUGSoutput$summary))
indexdevia	<-grep  ("deviance",rownames(modele$BUGSoutput$summary))

RET<-modele$BUGSoutput$summary[c(indexbeta,indexRCbeta,indexRRbeta,indexPrbeta,indexalpha,indexdevia),]

DIC		<-c(DIC=modele$BUGSoutput$DIC,pD=modele$BUGSoutput$pD)
######		Les noms de ligenes da la matrice resultats	###################
#nom_complet<-NULL
#for (i in 1:length(type)){
#if(type[i]==0)
#{nom_complet<-c(nom_complet,nom_covariables[i])}else
#{nom_complet<-c(nom_complet,paste(nom_covariables[i],levels(variable_explica[,i])[-1])  )}}

nom_complet<-colnames(design_mat_burtee	)[-1]
rownames(RET)<-c(
        c("int",nom_complet),
        paste("RR",c("int",nom_complet)),
	paste("Prob",c("int",nom_complet),">",Seuilmonitor),
        "deviance")

#####################################################################################
###################        Diagnostics pr?dictifs	 ##############################
#####################################################################################


ParamWB<-matrix(NA,ncol=1,nrow=4)

ParamWB[1,1]<-1
ParamWB[2,1]<-Niter
ParamWB[3,1]<-Nburnin
ParamWB[4,1]<-Nthin
rownames(ParamWB)<-c("Nbr Chaines","Nbr Iterations","Nbr Brulees","Nbr Thin")
colnames(ParamWB)=""
colnames(Priors)<-colnames(design_mat_burtee)
rownames(Priors)<-c("moy","prec")
if(Graph){gamm(variable_reponse)}
RESULTATS<-list(MCMC=noquote(ParamWB),Priors=Priors,DIC=DIC,Resultats=RET)

##################################################################################
cat("---------------------------");cat("\n")
cat("REGRESSION GAMMA BAYESIENNE");cat("\n")
cat("---------------------------");cat("\n")
titre<-paste("---	Graphique: Adequation de la distrib Gamma--------")
cat(titre);cat("\n")
cat("---------------------------");cat("\n")
cat("---Graphique: Diagnostics de convergence MCMC---");cat("\n")
cat("---------------------------");cat("\n")
cat("\n");cat("\n")


#####################################################################################
###################        Diagnostics de CV		 ##############################
#####################################################################################

if(Graph){plot_Conv_Variables_MAT_JAGS(modele,nom_complet);	par(mfrow=c(1,1))}

return(RESULTATS)}



##################################################################################################
########		Fonction pour identifier et recuperer les effets aleatoires		##############
##################################################################################################
reconnaitre_alea<-function(formul){
require("lme4")
presence_effet_alea	<-sum(grepl("\\|",as.character(formul)))
nbre_effet_alea		<-ifelse(presence_effet_alea==1,sum(gregexpr("\\|",as.character(formul)[grepl("\\|",as.character(formul))])[[1]]>0),0)

nbSUJETS<-NULL
if(presence_effet_alea==1){
Objet_issu_LFormula	<-lFormula(formul)
Valea<-Objet_issu_LFormula$ reTrms$flist
for(j in 1:nbre_effet_alea){
nbSUJETS[j]<-max(as.numeric(as.character(Valea[,j])))}
}else{
Valea<-NULL
}

RES<-list(	Presence_alea	=presence_effet_alea,
		Nombre_alea		=nbre_effet_alea,
		Vecteurs_alea	=matrix(as.numeric(as.character(as.matrix(Valea))),nrow=dim(Valea)[1]),
		Nombre_Sujets	=nbSUJETS)
return(RES)
}


