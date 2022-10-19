#' Remove "<"s from Values at LRL
#' @description replaces <'s with nothing and convert to numeric
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

subdl<-function(x){as.numeric(gsub("<*","",x))}
