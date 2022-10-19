#' BLM extract for Dissolved Copper BC Guidelines
#'
#' @description Add Text Boxes to Plot when guidelines exceed maximum range of plot.
#' @param x data frame - BLM model ouput in the txt format

#' @return data frame in the long format with 4 columns: station and sample (similar to BLM input and output files) and guideline type (either chronic or acute), and guideline values.
#' @export
#'
#' @examples

blm.extract =function(x) {

  chr<-read.table(x,fill = TRUE , header = FALSE)

  type <- chr[1,1]

  chr <- chr %>% # file name of the BLM model output
    mutate(V1=as.numeric(V1)) %>% # to select only BLM GLs section
    filter(!is.na(V1)) %>%
    dplyr::select(V2, V3, V5) # normally V2, V3, V5 will have information that we need
  # will got warming
  # }

  names(chr)=c(# rename columns to merge with other GLs
    "station",
    "sample",
    type)

  chr <- chr %>% tidyr::gather(type, key = "type", value = "value")
  return(chr)
}

