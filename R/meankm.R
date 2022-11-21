#' Calculate Mean for Censored Data.
#'
#' @description Calculate and format mean for censored data.
#' @param vals Character vector with potential censored values.
#' @param format Whether to format the output to the desired number of digits.
#' @param digits Number of significant digits.
#'
#' @return Mean value.
#' @export
#'
#' @examples

meankm<-function(vals,digit=3,format=F){
  options(scipen=999)
  x2<-vals[is.na(vals)==F]
  v1<-subdl(x2)
  c1<-grepl("<",x2)
  if(length(v1)==0){m1<-NA} else{
    if(all(c1==T)){
      m1<-paste0("<",min(v1))} else if(all(is.na(v1))){m1<-NA}
    else{
      maxy=abs(max(v1)) + abs(min(v1))  #Flip using abs (min) + abs(max)
      # fit survival model with 0 being ND and 1 being detect
      survmodel=survival::survfit(survival::Surv((maxy-v1), abs(c1-1))~1)
      sfit = summary(survmodel, rmean = "individual")
      m1<-maxy-as.numeric(sfit$table[5])
      if(all(c1==T)){m1<-paste0("<",m1)}

    }

  }

  #return(as.character(m1))
  if(format==T){
    return(gsub(" ","",formatCenval(m1,digit)))
  }else{
    return(as.character(m1))
  }
}
