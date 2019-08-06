# ggpie.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' x <- 1:10
#' ggpie(x)
#'
#' @return 
#' @export


ggpie<-function(Valeur,Groupe="Graphique"){
  if(!require(reshape)){install.packages("reshape")}
  library(reshape)
  if(!require(plyr)){install.packages("plyr")}
  library(plyr)
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(ggplot2)
  
  y  = data.frame(category=Groupe,
                  value=Valeur)
  # get counts and melt it
  data.m = melt(table(y))
  names(data.m)[3] = "count"
  # calculate percentage:
  m1 = ddply(data.m, .(category), summarize, ratio=count/sum(count))
  #order data frame (needed to comply with percentage column):
  m2 = data.m[order(data.m$category),]
  # combine them:
  mydf = data.frame(m2,ratio=m1$ratio)
  # get positions of percentage labels:
  mydf = ddply(mydf, .(category), transform, position = cumsum(ratio) - 0.5*ratio)
  # create bar plot
  colnames(mydf)[2]<-"Legende"
  mydf$Legende<-as.factor(mydf$Legende)
  pie = ggplot(mydf, aes(x = factor(1), y = ratio, fill = Legende)) +
    theme_void()+
    xlab("")+ylab("")+
    geom_bar(stat = "identity",width = 1) +
    
    facet_wrap(~category)
  # make a pie
  pie = pie + coord_polar(theta = "y")
  # add labels
  return(
    pie +
      geom_text(aes(label = sprintf("%1.2f%%", ratio*100), y =1- position))
  )
}
