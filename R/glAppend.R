#' Add Guideline Values to Table
#'
#' @description Add guideline values to table.
<<<<<<< HEAD
#' @param dat1 Data frame in long format. Should have column with management unit (MU) when using benchmarks.
#' @param Location The location of the guidelines. Only BC at the moment.
#' @param Type Type of guidelines. Current options include: Long-term chronic, Short-term acute, Teck-BM1,Teck-BM2, and Teck-BM3. The data frame allgls contains all guidelines currently available.
#' @param Name The name to use for resulting guideline column. Will result in two columns with "_upper" and "_lower" appended to the name.
=======
#' @param dat1
#' @param Location
#' @param Type
>>>>>>> e905f422e5d30e210e5616c1da959063189999bf
#'
#' @return A dataframe with screening values
#' @export
#'
#' @examples

<<<<<<< HEAD
glAppend<-function(dat1=NA,Location=NA,glType=NA,name=NA){

  # set up data table
  datGL<-allgls %>%
    dplyr::filter(Location==Location,Type==glType) %>%
=======
glAppend<-function(dat1=NA,Location=NA,Type=NA,name=NA){

  # set up data table
  datGL<-allgls %>%
    dplyr::filter(Location==Location,Type==Type) %>%
>>>>>>> e905f422e5d30e210e5616c1da959063189999bf
    rowwise()%>%
    mutate(GL_upper=ifelse(apply=="function",funset(GL_upper),list(GL_upper))) %>%
    dplyr::select(param,Type,apply,Depends,GL_lower,GL_upper)


  full<-dat1 %>%
    left_join(datGL) %>%
    rowwise() %>%
<<<<<<< HEAD
    #mutate(MUval="MU") %>%
    mutate(!!paste(name,"upper",sep="_"):=ifelse(apply=="function",
                                    #as.numeric(GL_upper(x=eval(sym(Depends)),mu=eval(sym(MUval)))),
                                    as.numeric(GL_upper(x=eval(sym(Depends)),manUnit=eval(sym("MU")))),
=======
    mutate(!!paste(name,"upper",sep="_"):=ifelse(apply=="function",
                                    as.numeric(GL_upper(eval(sym(Depends)))),
>>>>>>> e905f422e5d30e210e5616c1da959063189999bf
                                    as.numeric(GL_upper))) %>%
    mutate(!!paste(name,"lower",sep="_"):=as.numeric(GL_lower)) %>%
    dplyr::select(-Type,-apply,-Depends,-GL_lower,-GL_upper)
  return(full)


}
