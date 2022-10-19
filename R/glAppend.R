#' Add Guideline Values to Table
#'
#' @description Add guideline values to table.
#' @param dat1
#' @param Location
#' @param Type
#'
#' @return A dataframe with screening values
#' @export
#'
#' @examples

glAppend<-function(dat1=NA,Location=NA,Type=NA,name=NA){

  # set up data table
  datGL<-allgls %>%
    dplyr::filter(Location==Location,Type==Type) %>%
    rowwise()%>%
    mutate(GL_upper=ifelse(apply=="function",funset(GL_upper),list(GL_upper))) %>%
    dplyr::select(param,Type,apply,Depends,GL_lower,GL_upper)


  full<-dat1 %>%
    left_join(datGL) %>%
    rowwise() %>%
    mutate(!!paste(name,"upper",sep="_"):=ifelse(apply=="function",
                                    as.numeric(GL_upper(eval(sym(Depends)))),
                                    as.numeric(GL_upper))) %>%
    mutate(!!paste(name,"lower",sep="_"):=as.numeric(GL_lower)) %>%
    dplyr::select(-Type,-apply,-Depends,-GL_lower,-GL_upper)
  return(full)


}
