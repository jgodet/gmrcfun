# Intervalle de confiance d'une difference de deux proportions
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer - JuG
#

#' Intervalle de confiance d'une difference de deux proportions
#'
#' @description  Intervalle de confiance d'une difference de deux proportions
#' 
#' @param x1 Succes groupe 1
#' @param n1 Patients groupe 1
#' @param x2 Succes groupe 2
#' @param n2 Patients groupe 2
#' @param alpha01 parametre a priori sur la distribution beta de la premiere proportion
#' @param alpha02 parametre a priori sur la distribution beta de la deuxieme proportion
#' @param beta01 parametre a priori sur la distribution beta de la premiere proportion
#' @param beta02 parametre a priori sur la distribution beta de la deuxieme proportion
#' @param val largeur de l'intervalle de credibilite, par defaut 0.95
#' @return Bayesian, Wald, continuity correction Wald and approx Bayesian Confidence Intervals
#' @examples 
#' IC.diff.prop(x1=2,n1=10,x2=3,n2=11)
#' @export

IC.diff.prop<-function(x1,n1,x2,n2,alpha01=0.5,alpha02=0.5,beta01=0.5,beta02=0.5,val=0.95){

if( n1>1 & n2>1 & x1<=n1 & x2<=n2 ) {situation=1}else{situation=0}

if(situation==1){delta<-x2/n2-x1/n1
a1<-x1+alpha01
a2<-x2+alpha02
b1<-n1-x1+beta01
b2<-n2-x2+beta02
f<-function(t){
integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value
})
},-t,1)$value
}
g<-function(t){
1-integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*(1-integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value)
})
},0,1-t)$value
}
ifelse(f(0)<(1-val)/2,Binf<-optimize(function(x) abs((1-val)/2-g(x)),lower=0,upper=abs(delta)+0.001,maximum=FALSE,tol=1e-16)$minimum
,Binf<-optimize(function(x) abs((1-val)/2-f(x)),lower=-1,upper=min(delta+0.001,0),maximum=FALSE,tol=1e-16)$minimum)

f<-function(t){
integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value
})
},-t,1)$value
}
g<-function(t){
1-integrate(function(p1) { 
sapply(p1, function(p1) {p1^(a1-1)*(1-p1)^(b1-1)/beta(a1,b1)*(1-integrate(function(p2) p2^(a2-1)*(1-p2)^(b2-1)/beta(a2,b2),0,t+p1)$value)
})
},0,1-t)$value
}
ifelse(f(0)<(1+val)/2,Bsup<-optimize(function(x) abs((1+val)/2-g(x)),lower=0,upper=1,maximum=FALSE,tol=1e-16)$minimum,Bsup<-optimize(function(x) abs((1+val)/2-f(x)),lower=-1,upper=0,maximum=FALSE,tol=1e-16)$minimum)

BinfW<-x2/n2-x1/n1-qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)
BsupW<-x2/n2-x1/n1+qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)

BinfWCC<-x2/n2-x1/n1-qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)-(1/n1+1/n2)/2
BsupWCC<-x2/n2-x1/n1+qnorm((val+1)/2,0,1)*sqrt((x1/n1*(n1-x1)/n1)/n1+(x2/n2*(n2-x2)/n2)/n2)+(1/n1+1/n2)/2

BinfBapp<-a2/(a2+b2)-a1/(a1+b1)-qnorm((val+1)/2,0,1)*sqrt((a2/(a2+b2)*b2/(a2+b2))/(a2+b2)+(a1/(a1+b1)*b1/(a1+b1))/(a1+b1))
BsupBapp<-a2/(a2+b2)-a1/(a1+b1)+qnorm((val+1)/2,0,1)*sqrt((a2/(a2+b2)*b2/(a2+b2))/(a2+b2)+(a1/(a1+b1)*b1/(a1+b1))/(a1+b1))

M<-matrix(NA,nrow=4,ncol=2)
M[1,]<-c(Binf,Bsup)
M[2,]<-c(BinfW,BsupW)
M[3,]<-c(BinfWCC,BsupWCC)
M[4,]<-c(BinfBapp,BsupBapp)
colnames(M)<-c("B.inf","B.sup")
rownames(M)<-c("Bayesien","Wald","Wald.corr.conti","Bay.approche")

R<-list(Estimation=delta,IC=M)

} # fin if situation


if(situation!=1)
{
print("Conditions non verifiees	 n1>1 	 n2>1 	 x1<=n1  x2<=n2")
R<-""
}
return(R)
}

