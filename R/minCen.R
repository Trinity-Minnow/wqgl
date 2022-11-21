#' Calculate Min Value for Censored Data.
#'
#' @description Calculate and format min value for censored data.  Retains "<" if smallest value is censored.
#' @param vals Character vector with potential censored values
#' @param format Whether to format the output to the desired number of digits
#' @param digits Number of significant digits
#'
#' @return Minimum value
#' @export
#'
#' @examples

minCen<-function(vals,format=F,digits=3) {
  df1<-data.frame(vals) %>%
    mutate(val2=subdl(vals))

  minval<-df1[which(df1$val2==min(df1$val2,na.rm=T)),"vals"][1]

  if(format==T){minval=formatCenval(minval,digits)} else{minval=as.character(minval)}
  return(minval)
}
