#' Calculate Max Value for Censored Data.
#'
#' @description Calculate and format max value for censored data.  Takes highest detected value. If all values are censored it takes the smalled LRL.
#' @param vals Character vector with potential censored values.
#' @param format Whether to format the output to the desired number of digits.
#' @param digits Number of significant digits.
#'
#' @return Maximum value
#' @export
#'
#' @examples

maxCen<-function(vals,format=F,digits=3) {
  if(all(grepl("<",na.omit(vals))==T)){
    maxval<-paste0("<",subdl(min(vals,na.rm=T)))
  } else{
    #maxval=max(subdl(vals),na.rm=T)
    maxval=max(vals[!grepl("<",vals)])

  }
  if(format==T){maxval=formatCenval(maxval,digits)} else{maxval=as.character(maxval)}
  return(maxval)
}
