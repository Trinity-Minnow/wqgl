#' Format Numbers with Censored values
#' @description Formats numbers to a desired number of digits and keeps "<"'s
#'
#' @param x character vector with censored data
#' @param digit number of significant digits
#' @param comma Logical specifiying if a comma should be inserted into numbers greater than 1000.
#'
#'
#' @return
#' @export
#'
#' @examples

formatCenval<-function(x,digit=2,comma=T){
  options(scipen=999)
  cen<-grepl("<",x)
  val<-subdl(x)
  cen[is.na(val)==T]<-NA
  forvals<-val
  # if(forcenval2){
  forvals[which(val>40)]<-formatC(forvals[which(val>40)],format="fg",flag="#",big.mark=",",digit=digit)

  forvals[which(val<=40)]<-gsub("\\.$","",formatC(signif(as.numeric(forvals[which(val<=40)]),digit),
                                                  digits=digit,format="fg",flag="#",big.mark=","))


  forvals[which(cen==T & val >40)]<-paste0("<",forvals[which(cen==T & val >40)])
  forvals[which(cen==T & val <=40)]<-paste0("<",as.numeric(forvals[which(cen==T & val <=40)]))

  if (comma==T){
    return(gsub(" ","",forvals))
  } else{
    forvals<-gsub(" ","",forvals)
    return(gsub(",","",forvals))
  }

}
