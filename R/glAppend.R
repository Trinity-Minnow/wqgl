#' Add Guideline Values to Table
#'
#' @description Add guideline values to table.
#' @param dat1 Data frame in long format. Should have column with management unit (MU) when using benchmarks.
#' @param Location The location of the guidelines. Only BC at the moment.
#' @param Type Type of guidelines. Current options include: Long-term chronic, Short-term acute, Teck-BM1,Teck-BM2, and Teck-BM3. The data frame allgls contains all guidelines currently available.
#' @param Name The name to use for resulting guideline column. Will result in two columns with "_upper" and "_lower" appended to the name.
#'
#' @return A dataframe with screening values
#' @export
#'
#' @examples

glAppend<-function(dat1=NA,Location=NA,glType=NA,name=NA){

  # set up data table
  datGL<-allgls %>%
    dplyr::filter(Location==Location,Type==glType) %>%
    rowwise()%>%
    mutate(GL_upper=ifelse(apply=="function",funset(GL_upper),list(GL_upper))) %>%
    dplyr::select(param,Type,apply,Depends,GL_lower,GL_upper)


  full<-dat1 %>%
    left_join(datGL) %>%
    rowwise() %>%
    #mutate(MUval="MU") %>%
    mutate(!!paste(name,"upper",sep="_"):=ifelse(apply=="function",
                                    #as.numeric(GL_upper(x=eval(sym(Depends)),mu=eval(sym(MUval)))),
                                    as.numeric(GL_upper(x=eval(sym(Depends)),manUnit=eval(sym("MU")))),
                                    as.numeric(GL_upper))) %>%
    mutate(!!paste(name,"lower",sep="_"):=as.numeric(GL_lower)) %>%
    dplyr::select(-Type,-apply,-Depends,-GL_lower,-GL_upper)
  return(full)


}
