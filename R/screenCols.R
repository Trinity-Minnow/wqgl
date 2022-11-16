#' Adding Screening Columns to Water Quality Data
#'
#' @param data Data frame of water quality data
#' @param params Vector of water quality parameters
#' @param value Name of values column
#' @param gls Which guidelines (BC, CCME)
#'
#' @return A data frame with the appended screening columns.
#' @export
#'
#' @examples

screenCols<-function(data=NA,params=NA,value=NA,gls="BC"){

  dat2<-data%>%
    tidyr::pivot_wider(names_from=param,values_from=value) %>%
    dplyr::mutate(`Hardness (as CaCO3)_screen`=subdl(`Hardness (as CaCO3)`),
           `Chloride (Cl)_screen`=subdl(`Chloride (Cl)`),
           `pH, Field_screen`=`pH, Field`,
           `Dissolved Organic Carbon_screen`=`Dissolved Organic Carbon`,
           `Temperature, Field_screen`=`Temperature, Field`
    ) %>%
    tidyr::pivot_longer(names_to="param",values_to="value",params) %>%
    tidyr::drop_na(value) %>%
    dplyr::group_by(station) %>%
    # take conservative hardness
    dplyr::mutate(`Hardness (as CaCO3)_screen`=
             ifelse(is.na(`Hardness (as CaCO3)_screen`),
                    min(`Hardness (as CaCO3)_screen`,na.rm=T),`Hardness (as CaCO3)_screen`),
           # fill ph values
           `pH, Field_screenMin`=
             ifelse(is.na(`pH, Field_screen`),
                    min(`pH, Field_screen`,na.rm=T),`pH, Field_screen`),
           `pH, Field_screenMax`=
             ifelse(is.na(`pH, Field_screen`),
                    max(`pH, Field_screen`,na.rm=T),`pH, Field_screen`),
           # fill CL values
           `Chloride (Cl)_screen`=
             ifelse(is.na(`Chloride (Cl)_screen`),
                    min(`Chloride (Cl)_screen`,na.rm=T),
                    `Chloride (Cl)_screen`),
           # fill DOC values
           `Dissolved Organic Carbon_screen`=
             ifelse(is.na(`Dissolved Organic Carbon_screen`),
                    min(`Dissolved Organic Carbon_screen`,na.rm=T),
                    `Dissolved Organic Carbon_screen`)) %>%
    # fill in temperature variables
    dplyr::group_by(station, month) %>%
    dplyr::mutate(`Temperature, Field_screen`=ifelse(is.na(`Temperature, Field_screen`),
                                              max(`Temperature, Field_screen`,na.rm=T),
                                              `Temperature, Field_screen`)) %>% # will get inf is you have no temp for that station
    dplyr::ungroup() %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(`Temperature, Field_screen`=ifelse(is.infinite(`Temperature, Field_screen`),
                                              max(`Temperature, Field_screen`,na.rm=T),`Temperature, Field_screen`)) %>%  # replace with max of whole dataset
    dplyr::ungroup() %>%
    dplyr::mutate(
      `Hardness (as CaCO3)_screen`=ifelse(is.infinite(`Hardness (as CaCO3)_screen`),
                                          min(`Hardness (as CaCO3)_screen`,na.rm=T),`Hardness (as CaCO3)_screen`),
      `Chloride (Cl)_screen`=ifelse(is.infinite(`Chloride (Cl)_screen`),
                                    min(`Chloride (Cl)_screen`,na.rm=T),`Chloride (Cl)_screen`),
      `Dissolved Organic Carbon_screen`=ifelse(is.infinite(`Dissolved Organic Carbon_screen`),
                                               min(`Dissolved Organic Carbon_screen`,na.rm=T),`Dissolved Organic Carbon_screen`),
      `pH, Field_screenMin`=ifelse(is.infinite(`pH, Field_screenMin`),
                                   min(`pH, Field_screenMin`,na.rm=T),`pH, Field_screenMin`),
      `pH, Field_screenMax`=ifelse(is.infinite(`pH, Field_screenMax`),
                                   min(`pH, Field_screenMax`,na.rm=T),`pH, Field_screenMax`)) %>%
    dplyr::mutate(temp_ph=glTempPh(gl=gls,pH=as.numeric(`pH, Field_screenMax`),
                                   temp=as.numeric(`Temperature, Field_screen`))) %>%
    dplyr::mutate(`pH, Field_screenMin`=as.numeric(`pH, Field_screenMin`))
  # testing


  return(dat2)
}
