#' Converts Units for Censored data
#'
#' @description Add guideline values to table.
#' @param x character vector with censored data
#' @param conval unit multiplier
#'
#' @return character vector with converted units and <'s retained
#' @export
#'
#' @examples

unitconvert<-function(x,conval){
  vals<-gsub(" ","",x)
  # get censoring
  cenvals<-grepl("<",vals)
  # replace "<" signs
  vals<-gsub("<","",vals)
  #  convert
  vals<-as.numeric(vals)*conval
  # paste "<" back in
  options(scipen=999) # turn off scientific notation
  vals[cenvals==TRUE]<-paste0("<",vals[cenvals==TRUE])
  vals

  return(vals)

}
