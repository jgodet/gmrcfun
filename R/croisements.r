# croisements.r
# written by JuG
# August 05 2019


#' Do something
#' @author JuG
#' @description 
#' @param numero.variable.dinteret
#' @param D
#' @param qualif
#' @param quantiF
#' @param affichage
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


croisements<-function(numero.variable.dinteret,D,qualiF,quantiF,affichage=40){

  tableaux.croises<-1
  if(missing(quantiF)){nul<-0}else{if(length(quantiF)==1) {quantiF=c(quantiF,quantiF)}}
  if(missing(qualiF)){nul<-0}else{if(length(qualiF)==1) {qualiF=c(qualiF,qualiF)}}

  choix.de.sortie.R.1.LaTeX.2		            <-1		# Choisir le format d'affichage des resultats
  choix.entre.manuelle1.et.auto.2	            <-2		# Choisir la reconnaissance automatique ou specification manuelle de la nature des variables
  modalites.maximum				            <-7		# Modalites maximum des variables qualitatives (en automatique)

  ##     Variable d'INTERET                ###

  ny<-numero.variable.dinteret
  Y<-D[,ny]


  if(!require('xtable')){install.packages('xtable')}
  library(xtable)
  if(!require('epitools')){install.packages('epitools')}
  library(epitools)

  choix<-choix.entre.manuelle1.et.auto.2

  #*******\________Reconnaissance automatique_______/*******#

  mod.max<-modalites.maximum
  Factor<-VarNumeric<-0
  nbvar<-length(D)

  for(i in 1:nbvar)# Reconnaissance des facteurs et des numeriques
  { if(is.factor (D[,i])==TRUE){Factor    <-c(Factor,i        )}
    if(is.numeric(D[,i])==TRUE){VarNumeric<-c(VarNumeric,i    )}}
  Factor        <-Factor[2:(length(Factor))]
  VarNumeric    <-VarNumeric[2:(length(VarNumeric))]
  Factinfseuil  <-Factsupseuil<-0

  if(sum(Factor,na.rm=TRUE)!=0){
    for(i in 1:(length(Factor))){
      if(nlevels(D[,Factor][,i])<=mod.max){Factinfseuil<-c(Factinfseuil ,Factor[i])}
      if(nlevels(D[,Factor][,i])> mod.max){Factsupseuil<-c(Factsupseuil ,Factor[i])}}
    Factinfseuil<-Factinfseuil[2:length(Factinfseuil)]
    Factsupseuil<-Factsupseuil[2:length(Factsupseuil)]
  }

  NbNumeric<-length(D[,VarNumeric])
  quanti<-quali<-0
  for(i in 1:NbNumeric){
    if(nlevels(as.factor(D[,VarNumeric][,i]))>mod.max){quanti<-c(quanti,VarNumeric[i])}
    if(nlevels(as.factor(D[,VarNumeric][,i]))<=mod.max){quali<-c(quali ,VarNumeric[i])}}
  quanti<-quanti[2:(length(quanti))]
  quali<-quali[2:(length(quali))]

  situation<-0
  if(missing(quantiF))			# Distinguons volontairement le cas ou il n'y qu'une seule variable
  {nul=0}else{
    if(length(quantiF)==1){choix<-1;situation=5;quantitatives<-quantiF}
  }

  if(missing(qualiF))			# Distinguons volontairement le cas ou il n'y qu'une seule variable
  {nul=0}else{
    if(length(qualiF)==1){choix<-1;situation=6;qualitatives<-c(qualiF,qualiF)}
  }

  # Definition de 4 situations possibles:
  # 1 : Rien n'est specifie, tout est auto
  # 2 : juste quali  est specifie
  # 3 : juste quanti est specifie
  # 4 : quali et quanti sont specifies
  # 5 : une seule variable quantitative
  # 6 : une seule variable qualitative

  if(situation<5){
    if(missing(quantiF)& missing(qualiF )  ){choix<-2; situation<-1}
    else{
      if(missing(quantiF)){choix<-1; qualitativesM <-qualiF ; quantitativesM <--1;situation<-2 }
      else{
        if(missing(qualiF )){choix<-1; quantitativesM<-quantiF; qualitativesM  <--1; situation<-3 }
        else{choix<-1;quantitativesM<-quantiF;qualitativesM <-qualiF;situation<-4}
      }
    }

    if(choix==1){qualitatives<-qualitativesM;quantitatives<-quantitativesM}else{
      if(choix==2){qualitatives<-quali;quantitatives<-quanti}else{stop("Erreur de choix")}  }

    autorisation<-rep(0,(length(Factinfseuil)))
    for (i in 1:(length(Factinfseuil)))
      if((is.na(Factinfseuil[i])==0) &(Factinfseuil[i]!=0)){autorisation[i]<-1}
    if(situation==1){
      if((sum(autorisation))==(length(autorisation))){qualitatives<-c(qualitatives,Factinfseuil)}
    }
  }

  Affichage.Nombre.variables.quanti<-Affichage.Nombre.variables.quali<-0
  if(situation==1){Affichage.Nombre.variables.quali<-(length(qualitatives)-1)	   ;Affichage.Nombre.variables.quanti<-length(quantitatives)}
  if(situation==2){Affichage.Nombre.variables.quali<-(length(qualiF))		       ;Affichage.Nombre.variables.quanti<-0}
  if(situation==3){Affichage.Nombre.variables.quali<-0					       ;Affichage.Nombre.variables.quanti<-length(quantiF)}
  if(situation==4){Affichage.Nombre.variables.quali<-(length(qualiF))		       ;Affichage.Nombre.variables.quanti<-length(quantiF)}
  if(situation==5){Affichage.Nombre.variables.quali<-0					       ;Affichage.Nombre.variables.quanti<-1}
  if(situation==6){Affichage.Nombre.variables.quali<-1					       ;Affichage.Nombre.variables.quanti<-0}

  ##   CROISEMENTS ET TESTS                 ######

  options(width=400)
  if(tableaux.croises==1){
    if((situation==1)|(situation==2)|(situation==4)|(situation==6)){


      ################################        VARIABLES QUALITATIVES             ########################################


      cat(noquote(" _____________________________________________________________\n"))
      cat(noquote("/____________________________________________________________/|\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      if(Affichage.Nombre.variables.quali>9){Affichage.espace.quali<-"               ||"}else{Affichage.espace.quali<-"              ||"}
      titre<-paste("|","              VARIABLES QUALITATIVES","(",Affichage.Nombre.variables.quali,")",Affichage.espace.quali)
      cat(noquote(titre));cat("\n")
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|____________________________________________________________|/\n"))
      cat(noquote("\n"));


      for(i in 1:(length(qualitatives))) {if(colnames(D[,qualitatives])[i]==colnames(D)[ny]) {numero<-i} else{numero<-0}}

      for(i in 1:(length(qualitatives))) {
        if(i!=numero){
          LTABLE      <-length(table(is.na(D[,qualitatives][,i])))
          if((LTABLE==1) & (is.na(D[,qualitatives][,i][1])==1)) {VIDE<-1}else{VIDE<-0}
          if(VIDE==0){
            A                    <-table(Y,D[,qualitatives][,i])                                                                                 ### Matrice des effectifs croises
            colnames(A)          <-c(paste(names(D[,qualitatives])[i],attr(table(D[,qualitatives][i]),"dimnames")[[1]]))
            rownames(A)          <-c(paste(names(D)[ny],attr(table(Y),"dimnames")$Y))
            longueur.A           <-sum(nchar(colnames(A)))
            mat.separation.A     <-cbind(matrix("",ncol=(affichage-longueur.A),nrow=2),c("|","|"),c("",""))                                             ### De quoi faire de l'affichage propre
            Nbcolonnes	         <-length(colnames(A))
            Nblignes	         <-length(rownames(A))
            Alig                 <-A;for(k in 1:Nblignes){Alig[k,]<-round(A[k,]/sum(A[k,]),3)}                                                  ### Matrice des effectifs en ligne
            colnames(Alig)       <-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%lign"))
            Acol                 <-A;for(m in 1:Nbcolonnes){Acol[,m]<-round(A[,m]/sum(A[,m]),3)}                                                ### Matrice des pourcentages en colonne
            colnames(Acol)       <-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%col"))
            Atot                 <-round(A/sum(A),3);colnames(Atot)<-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%tot"))          ### Matrice des pourcentages totaux
            colnames(Atot)       <-c(paste(attr(table(D[,qualitatives][i]),"dimnames")[[1]],"%tot"))
            Vecteur.separation   <-noquote(matrix(c("|","|"),nrow=2))
            Acomp                <-noquote(cbind(A,mat.separation.A,Alig,c(" "),Vecteur.separation,c(" "),Acol,c(" "),Vecteur.separation,c(" "),Atot))

            if(length(A)>4){Acomp<-list(Tableau=A,Pourcentages.lignes=Alig,Pourcentages.colonnes=Acol,Pourcentages.total=Atot)}                 ### Condition pour voir si c'est pas trop long
            ### Sinon reprendre dans une autre ligne
            p                    <-chisq.test(A)$p.value;p2<-try(fisher.test(A)$p.value,silent = TRUE)
            OR                   <-try(fisher.test(A)$estimate,silent=TRUE)
            if(length(A)==4){
              if((OR>=try(fisher.test(A)$conf.int[1],silent=TRUE))&(OR<=try(fisher.test(A)$conf.int[2],silent=TRUE)))
              {ORCI<-c(try(fisher.test(A)$conf.int[1],silent=TRUE),try(fisher.test(A)$conf.int[2],silent=TRUE))}
              if((OR<try(fisher.test(A)$conf.int[1],silent=TRUE))|(OR>try(fisher.test(A)$conf.int[2],silent=TRUE)))
              {ORCI<-unname(oddsratio(A)$measure[2,][2:3])}
              OR                   <-round(OR,digits<-3);ORCI<-round(ORCI,digits=3)
              conjonction<-"dans ["
            }
            if(length(A)!=4) {
              OR <- "Non_calculable"
              ORCI <- vector("character",2) #c("","")
              conjonction <- c(" Probleme_taille")
              }
            ### Condition, si la matrice n'est pas carree
            ### Alors l'OR n'est pas defini.
            if(p<0.05 ){ p      <-paste(round(p ,2),"           ***")} else{p <-round(p,2 )}                                                    ### Affichage des etoiles si le test est signif
            if(is.numeric(p2)){if(p2<0.05 ){p2     <-paste(round(p2,2),"           ***")} else{p2<-round(p2,2)}}else{p2<-"NA"}

            if(choix.de.sortie.R.1.LaTeX.2==1){print(Acomp);                                                                                    ### Affichage des resultats des tests
              R                   <-noquote(paste("Test d'homogeneite du Chi-2       ",p))
              R3                  <-noquote(paste("Rapport de Cotes: ",OR,conjonction,ORCI[1],",",ORCI[2],"]"))
              R2                  <-noquote(paste("Test d'homogeneite Fisher exact   ",p2))
              R                   <-rbind(R,R2,R3)
              colnames(R)         <-c("")
              rownames(R)         <-c("","","")
              print(noquote(R))
              cat(noquote(" \n"));cat(noquote("____________________________________________________________\n"));cat(noquote(" \n"))}

            if(choix.de.sortie.R.1.LaTeX.2==2){print(xtable(A,caption=paste("Chi2",round(p,3),"/","Fisher",try(round(p2,3),silent=TRUE))))}
          }else{
            Affich<-c(names(D[,qualitatives])[i],"--> Vecteur de MANQUANTS")
            print("");print("");print("");print("");print("")
            print(Affich)
            print("__________________________________________________")
            print("");print("");print("");print("");print("")
          }}}}

    ################################        VARIABLES QUANTITATIVES            ########################################


    if(situation==5){quantitatives<-c(quantitatives,quantitatives)}
    nam<-names(D[,quantitatives])
    Nquant<-length(quantitatives)
    nbr.mod.rep<-length(attr(table(Y),"dimnames")$Y)
    modalites.rep<-as.numeric(attr(table(Y),"dimnames")$Y)
    moyenne <-variance<-mini<-NN<-NAA<-maxi<-mediane<-ecarttype<-quant025<-quant975<-rep(NA,nbr.mod.rep)
    if((situation==1)|(situation==3)|(situation==4)|(situation==5)){
      # Question d'affichage
      cat(noquote(" _____________________________________________________________\n"))
      cat(noquote("/____________________________________________________________/|\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      if(Affichage.Nombre.variables.quanti>9){Affichage.espace<-"              ||"}else{Affichage.espace<-"             ||"}
      titre<-paste("|","              VARIABLES QUANTITATIVES","(",Affichage.Nombre.variables.quanti,")",Affichage.espace)
      cat(noquote(titre));cat(noquote("\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|                                                            ||\n"))
      cat(noquote("|____________________________________________________________|/\n"))
      cat(noquote("\n"));
      borne<-Nquant
      if(situation==5){borne=Nquant-1}
      for(i in 1:borne)
      {
        for(j in 1:2)
        {
          NAA[j]      <-sum(is.na(        D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]]))
          NN[j]       <-length(           D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]])-NAA[j]
          moyenne[j]  <-round(mean(       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)                    # Calcul des statistiques desriptives appropriees
          mediane[j]  <-round(median(     D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          variance[j] <-round(var (       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          ecarttype[j]<-round(sd (        D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          mini[j]     <-round(min (       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          maxi[j]     <-round(max (       D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE),3)
          quant025[j] <-round(quantile (  D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE,prob=0.025),3)
          quant975[j] <-round(quantile (  D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[j]],na.rm=TRUE,prob=0.975),3)}
        A<-cbind(NN,round((NN/(sum(NN))),2),NAA,moyenne,variance,ecarttype,mini,maxi,mediane,quant025,quant975)
        colnames(A) <-c("N","% ","NA","moyenne","variance","sd","min","max","med","q_2.5%","q_97.5%")
        rownames(A) <-c(paste(names(D)[ny],attr(table(Y),"dimnames")$Y))
        LT1         <-length(table(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]])))                                # Verifier la condition, l'un des deux vecteurs est nul
        LT2         <-length(table(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])))
        if(((LT2==1)&(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]][1]==TRUE)))|((LT1==1)&(is.na(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]][1]==TRUE))))
        {p<-"MANQUANTS";p2<-"MANQUANTS"}
        else{
          p           <-      try(t.test      (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE)
          p2          <-      try(wilcox.test (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE)
          p.norm.1    <-      try(shapiro.test(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]])$p.value,silent=TRUE)
          p.norm.2    <-      try(shapiro.test(D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE)
          p3          <-round(try(var.test    (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE),2)
          p4          <-round(try(ansari.test (D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[1]],D[,quantitatives][,i][Y==attr(table(Y),"dimnames")$Y[2]])$p.value,silent=TRUE),2)



          if((p.norm.1>0.05) & (p.norm.2>0.05)){normalite<-"Acceptee 2/2"}
          if((p.norm.1>0.05) & (p.norm.2<0.05)){normalite<-"Rejetee 1/2"}
          if((p.norm.1<0.05) & (p.norm.2>0.05)){normalite<-"Rejetee 1/2"}
          if((p.norm.1<0.05) & (p.norm.2<0.05)){normalite<-"Rejetee 0/2"}}
        if(p<0.05 ){ p <-paste(round(p,2),"      ***")} else{p<-round(p,2)}
        if(p2<0.05 ){p2<-paste(round(p2,2),"      ***")}else{p2<-round(p2,2)}
        if(p3<0.05 ){p3<-paste(round(p3,2),"      '''")}else{p3<-round(p3,2)}
        if(p4<0.05 ){p4<-paste(round(p4,2),"      '''")}else{p4<-round(p4,2)}
        if((p!="MANQUANTS")&(p2!="MANQUANTS"))			{V0 =noquote(cbind(nam[i],"         ",normalite))}     # Affichage des resultats
        if((p!="MANQUANTS")&(p2!="MANQUANTS"))			{V  =noquote(rbind(paste("Egalite des moyennes : Test de Student    ",p),paste("Comparaison distributions: Test de MW.Wilcoxon",p2)))}else{V=cbind("Prob de manquants",p,"Prob de manquants",p2)}     # Affichage des resultats
        if((p!="MANQUANTS")&(p2!="MANQUANTS"))     		{V2 =noquote(rbind(paste("Egalite des variances: Test de Fisher     ",p3),        paste("Egalite des variances: Test de Ansari     ",p4)))}
        colnames(V0)=c("Variable","","Normalite echantillons")
        rownames(V0)=c("")
        V<-noquote(rbind(V,V2))
        colnames(V)<-c("")
        rownames(V)<-c("","","","")
        if(choix.de.sortie.R.1.LaTeX.2==1){print(A);print(V0); print(V);cat(noquote(" \n"));cat(noquote("____________________________________________________________\n"));cat(noquote(" \n"));cat(noquote(" \n"))}
        if(choix.de.sortie.R.1.LaTeX.2==2){print(xtable(A,caption=paste(nam[i],"/","t.test",round(p,2),"/","Wilcoxon",round(p2,2))))}
      }}
  }
  options(width=80)
  cat("**************************** FINI ************************************************\n")
}

