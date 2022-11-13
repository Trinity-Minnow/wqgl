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
<<<<<<< HEAD
  fun1<-function(x,manUnit=2){}
=======
  fun1<-function(x){}
>>>>>>> e905f422e5d30e210e5616c1da959063189999bf
  body(fun1)<-parse(text=glfun)
  return(list(fun1))
}
