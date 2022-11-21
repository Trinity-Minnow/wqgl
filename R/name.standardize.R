#' standardize parameter names using param table from Teck database
#'
#' @description standardize parameter names using param table from Teck database.
#' @param paramdf data.frame or list of parameters need to be standardized with  Parameter_Name column
#' @param namesdf data.frame with Parameter_Name and Param_Pref columns with Param_Pref is the column for final name and that table is pulled from lu_Param table in Teck database.
#'
#' @return data frame with parameters that do not have the standardized names and data frame with all params name matched up.
#' @export
#'
#' @examples  source(paste0(gsub("Documents","Trinity Consultants, Inc/Minnow - General/",path.expand('~')),"Client Data/Teck/Data_Managment/Database/Gather_Data_functions.R"))

# my_db  <- dbPool(
#   drv = RMariaDB::MariaDB(),
#   db = "Teck_DB",
#   host = "10.0.0.159",
#   username = "ydinh", # you should change these to your own logins which all follow this same pattern
#   password = "ydinh1!"
# )
#
#
# tbl_str<-DB_GetStr(my_db,join="left",tbl_use="tbl_con",database="Teck_DB")
#
# namesdf = DBtbl_pull(my_db,
#                      "lu_Param",
#                      tbl_str=tbl_str,
#                      tbl_src="tbl_out") %>%
#   dplyr::select(Parameter_Name ,Param_Pref,Param_Type ) %>%
#   collect()


name.standardize<- function(paramdf,namesdf, type=NA) {



  fndf = left_join(paramdf, namesdf) %>%
    dplyr::select(Parameter_Name, Param_Pref) %>%
    filter(is.na(Param_Pref))

  fndf2 = left_join(paramdf, namesdf) %>%
    dplyr::select(Parameter_Name, Param_Pref)

  fn = list("missing standardized names" = fndf,
            "standardized names"=fndf2 )

  return( fn)


}



