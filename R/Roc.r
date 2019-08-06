# Roc.r
# written by JuG
# August 05 2019


#' ROC curve for logistic regression
#' @author JuG
#' @description ROC curve for logistic regression
#' @param mod logistic regression model
#' @details 
#' @examples 
#' dtf <- data.frame(Y = rbinom(n = 60 ,size=1,prob = .3), X = rnorm(60, 10,2) )
#' mod <- glm(Y ~ X, data= dtf, family = "binomial")
#' Roc(mod)
#' @return 
#' @export


Roc<-function(mod){
  if(!require(Epi)){install.packages('Epi')}
  library(Epi)
  ROC(fitted(mod,type="response"),mod$y,AUC=TRUE,main="Courbe ROC", MI=F)
}
