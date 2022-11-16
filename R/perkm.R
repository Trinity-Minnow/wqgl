#' Calculate Percentiles for Censored Data.
#'
#' @description Calculate and format percentiles for censored data.
#' @param vals Character vector with potential censored values
#' @param probs Probabilites to evaluate.
#' @param format Whether to format the output to the desired number of digits
#' @param digits Number of significant digits
#'
#' @return Percentile values.
#' @export
#'
#' @examples

perkm<-function(vals,digit=3,probs,format=T){
  x2<-x[is.na(x)==F]
  v1<-subdl(x2)
  c1<-grepl("<",x2)
  if(all(c1==T)){
    percentiles<-paste0("<",min(v1))
    #percentiles<-NA
  } else{
    maxy=abs(max(v1)) + abs(min(v1))  #Flip using abs (min) + abs(max)
    # fit survival model with 0 being ND and 1 being detect
    survmodel=survfit(Surv((maxy-v1), abs(c1-1))~1)
    sfit = summary(survmodel, rmean = "individual")
    m1<-maxy-as.numeric(sfit$table[5])

    sfit = summary(survmodel, rmean = "individual")


    percentiles<-maxy-quantile(survmodel,probs=1-probs,
                               conf.int=FALSE)

    if(is.na(percentiles)==T){

      q1<-quantile(v1,probs,na.rm=T)
      q1val<-min(v1[v1>=q1],na.rm=TRUE)
      percentiles<-q1val
    }

  }

  if(format==T){
    percentiles = forcenval2(percentiles ,digit, comma=T)
  }

  percentiles
}
