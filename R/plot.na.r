# plot.na.r
# written by JuG
# August 06 2019


#' Graphical representation of  missing values present in a data.frame 
#' @author JuG
#' @description  Graphical representation of  missing values present in a data.frame 
#' @param data_in data frame
#' @details 
#' @examples 
#' dtf <- data.frame(varA = rnorm(10), varB = sample(x = LETTERS[1:2], size = 10, replace=T), varC = sample(c(1:9,NA), size=10) )
#' plot.na(dtf)
#' plot.na(dtf, title = "Titre")
#' @return 
#' @export


plot.na <- function(data_in, title = NULL){
  if(!require(ggplot2)){install.packages('ggplot2')}
  require(ggplot2)
  temp_df <- as.data.frame(ifelse(is.na(data_in), 1, 0))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(       x = unlist(data_temp$x), 
                                 y = unlist(data_temp$y), 
                                 m = unlist(data_temp$m))
  
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + 
    scale_fill_manual(values=c("lightblue", "red"), 
                      name="Missing\n(1=Yes, 0=No)") + theme_light() + ylab("Variable") + xlab("Numero du sujet") + ggtitle(title)
}
