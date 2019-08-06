# cs01.r
# written by JuG
# August 05 2019


#'  Normalize data to 0-1 range
#' @author JuG
#' @description Normalize data to 0-1 range
#' @param X numerical vector
#' @param min either min of X or user define value
#' @param max either min of X or user define value
#' @details 
#' @examples 
#' X <- runif(n = 20, min = 1,max = 100)
#' cs01(X)
#' cs01(X,0,150)
#' @return numeric()
#' @export


cs01<-function(X,mini = min(X),maxi = max(X)){
  if(missing(mini)|missing(maxi)){
    print("Warning of cs01")
  }
  if(missing(mini)){
    print("mini parameter was defined as  min(X)")
    mini <- min(X, na.rm=T)
  }
  if(missing(maxi)){
    print("maxi parameter was defined as  max(X)")
    maxi <- max(X,na.rm=T)
  }  

  if(!missing(mini) & !missing(maxi)){
    X_star	<-(X-mini)/(maxi-mini)
    X_starstar	<-(X_star*(length(X_star)-1)+0.5)/(length(X_star))
  }
  return(X_starstar)
}
