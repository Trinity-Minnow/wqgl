## code to prepare `allgls` dataset goes here

allgls <- readr::read_csv("data-raw/Guideline_functions.csv")
usethis::use_data(allgls, overwrite = TRUE)

BCammLu <- readr::read_csv("data-raw/BCammoniaGLs.csv")
usethis::use_data(BCammLu, overwrite = TRUE)
