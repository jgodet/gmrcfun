# Burter une design matrix
# Date : 2015-06-15
# Original function : Mickael Schaeffer
# Refactored by : Mickael Schaeffer
#

#' Utile pour GlmB





burt_design<-function(M){
type<-RES<-num_ref<-NULL
for(i in 1:length(M)) {type[i]<-ifelse(is.factor(M[,i]),1,0)
if(type[i]==1){
RES<-cbind(RES,burt(M[,i]));
num_ref<-c(num_ref,c(1,rep(0,nlevels(M[,i])-1)))
		}
else{
RES<-cbind(RES,M[,i]);
num_ref<-c(num_ref,0)
	}
		}
return(list(RES=RES,num_ref=num_ref))
}
