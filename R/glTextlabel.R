#' Add Guideline Text Boxes to Plot
#'
#' @description Add Text Boxes to Plot when guidelines exceed maximum range of plot.
#' @param gldf data frame in long format with name of gls (pltName),the maximum y values (maxy),the colour of the label and the unit(unit)
#' @param unit the unit of the parameter
#'
#' @return Table with labels and colours.
#' @export
#'
#' @examples

glTextlabel<- function(gldf,unit) {

  #x<-gldf
  #adj=0.6
  # maxy = x %>%
  #   #gather(type, gl, gllist) %>%
  #   #drop_na(gl) %>%
  #   dplyr::summarise(min.gl=min(gl.value, na.rm = T),
  #                    max.gl=max(gl.value, na.rm = T),
  #                    max.val=max(value, na.rm = T)) %>%
  #   mutate(maxy=case_when(
  #     max.val < min.gl*adj ~  max.val,
  #     max.val > min.gl*adj & max.val <= min.gl ~ min.gl,
  #     max.val > min.gl & max.val< max.gl*adj ~ max.val,
  #     max.val > max.gl*adj & max.val <= max.gl ~ max.gl,
  #     max.val>max.gl ~ max.val)) %>%
  #   pull(maxy)

  #maxy<-10

  #gldata<-x
  glmiss<-gldf %>%
    filter(gl.value>maxy)
    #gldf[which(gldf$gl.value>maxy),]

  glsumm<-glmiss %>%
    group_by(pltName,pltcol) %>%
    dplyr::summarize(n=length(unique(gl.value)),
                     min=min(gl.value),
                     max=max(gl.value)) %>%
    ungroup()%>%
    mutate(min=formatCenval(min,2)) %>%
    mutate(label=ifelse(n==1,paste(pltName,"not shown =",min,unit),
                        paste("Min ",pltName,"not shown =",min,unit)))

  return(glsumm)


}


