# plot.na.r
# written by JuG
# August 06 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


plot.na <- function(data_in, title = NULL){
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