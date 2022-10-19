#' Create BLM Input File
#'
#' @description Create input file to run BLM to derive copper guideline.
#' @param dat1 Data frame in long format that has been run through sreenCols script.
#' @param lunames A character vector of column names
#' @return A dataframe that can be used as input into the BLM software.
#' @export
#'
#' @examples

blminput<-function(dat1=NA,lunames=NA){
lunames<-c("station","year","month","day")
  # set up data table
  df1<-dat1%>%
    filter(param=="Copper (Cu)-Dissolved") %>%
    pivot_wider(names_from=param,values_from=value) %>%
    dplyr::select(all_of(lunames),
                  `Temperature, Field_screen`,
                  `pH, Field_screenMin`,
                  `Copper (Cu)-Dissolved`,
                  `Dissolved Organic Carbon_screen`,
                  `Hardness (as CaCO3)_screen`) %>%
    unite("lu",all_of(lunames),sep="_")


  return(df1)


}
