#' Create Colour and Shapes for Censored Data Plots
#'
#' @description Add guideline values to table.
#' @param data Data frame with censored data. need to have logical column named "cen" with TRUE for censored values
#' @param statinfo data frame with a station column and the corresponding colour (named colour) and shape (named shape). Shape options include: square, triUp, triDown,circle, and diamond.
#'
#' @return A data frame with colours for colour, fill and shape
#' @export
#'
#' @examples

cenPltcode<-function(data=NA,statinfo=NA){
  #data=dftemp

  # cen col must be named cen
  # stat info column designates the colours and shapes for each station
  shpdf<-data.frame(shape=c("square","triUp","triDown","circle","diamond"),
                    cennum=c(0,2,6,1,5),
                    ncennum=c(22,24,25,21,23))

  # dataframe for coding
  temp1<-statinfo %>%
    left_join(shpdf)

  if(all(is.na(temp1$cennum)==F)==FALSE){stop("shape not included")}
  statord<-data$station
  # build data frame with coding
  temp2<-data %>%
    left_join(temp1) %>%
    mutate(shape=ifelse(cen==TRUE,cennum,ncennum)) %>%
    mutate(fill=colour) %>%
    mutate(colour=ifelse(cen==TRUE,colour,minnow.grey)) %>%
    mutate(legend.shp =  case_when(
      shape %in% c(0,22) ~ 22,
      shape %in% c(2,24) ~ 24,
      shape %in% c(6,25) ~ 25,
      shape %in% c(1,21) ~ 21,
      shape %in% c(5,23) ~ 23,
    ))%>%
    dplyr::select(-ncennum,-cennum)

  return(temp2)

}
