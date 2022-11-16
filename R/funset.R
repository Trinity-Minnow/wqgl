#' Convert text to functions
#'
#' @description Add guideline values to table.
#' @param text to convert to function
#'
#' @return function
#' @export
#'
#' @examples

funset<-function(glfun){
  fun1<-function(x,manUnit=2){}
  body(fun1)<-parse(text=glfun)
  return(list(fun1))
}
