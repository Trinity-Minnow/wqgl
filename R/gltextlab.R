#' Add Guideline Text Boxes to Plot
#'
#' @description Add Text Boxes to Plot when guidelines exceed maximum range of plot.
#' @param x data frame in long format
#' @param vallist name of columns with plot values and other factors that affect the y maximum (for example normal ranges)
#' @param gllist list of column names with guidelines
#' @param gllabel list of names of guidelines in same order of gllist
#' @param glcol list of colours for text in text box. Same order as gllist
#' @param Unit unit of the parameter.
#'
#' @return List of labels to inserted to plot.
#' @export
#'
#' @examples

gltextlab<- function(x, vallist, gllist, gllabel, glcol,Unit, adj=0.6) {

  # get the gls dataframe
  glfn = data.frame(gl=gllist,
                    label=gllabel,
                    color=glcol)
  gldata = x  %>%
    gather(gl,gl.value , gllist) %>%
    drop_na(gl.value)  %>%
    left_join(glfn)

  # get the maxy of the plot
  df = x %>%
    gather(type, result, vallist)

  maxdf=max(df$result,na.rm = T)

  maxy = x %>%
    gather(type, gl, gllist) %>%
    #drop_na(gl) %>%
    dplyr::summarise(min.gl=min(gl, na.rm = T),
                     max.gl=max(gl, na.rm = T)) %>%
    mutate(max.val= maxdf) %>%
    mutate(maxy=case_when(
      max.val < min.gl*adj ~  max.val,
      max.val > min.gl*adj & max.val <= min.gl ~ min.gl,
      max.val > min.gl & max.val< max.gl*adj ~ max.val,
      max.val > max.gl*adj & max.val <= max.gl ~ max.gl,
      max.val>max.gl ~ max.val)) %>%
    pull(maxy)

  # loop through gldata row to identify which will need to show on the plot
  glmiss<-gldata[which(gldata$gl.value>maxy),]
  if (nrow(glmiss)>0){

    minvals<-aggregate(glmiss$gl.value,list(label=glmiss$label,color=glmiss$color),min)
    minvals$label<-factor(minvals$label,levels=unique(glmiss$label))
    minvals<-minvals[order(minvals$label),]
    maxvals<-aggregate(glmiss$gl.value,list(label=glmiss$label,color=glmiss$color),max)
    maxvals$label<-factor(maxvals$label,levels=unique(glmiss$label))
    maxvals<-maxvals[order(maxvals$label),]
    missmin <- aggregate(glmiss$gl.value, list(label = glmiss$label,
                                               color = glmiss$color), min)
    missmin$label <- factor(missmin$label, levels = unique(glmiss$label))
    missmin <- missmin[order(missmin$label), ]

    # derive GL
    labellist<-list()
    for (m in 1:nrow(missmin)){
      if(minvals[m,3]==maxvals[m,3]){minname=""}else{minname="Min "}

      val<-missmin[m,"x"]
      val<-forvals(val)

      # get the unit match with parameter unit
      if (grepl("u",Unit)==FALSE){
        labellist[[m]]<-data.frame(label=paste0(minname,
                                                missmin[m,"label"],
                                                " not shown = ",val," mg/L"),color=missmin[m,"color"])
      } else {
        labellist[[m]]<-data.frame(label=paste0(minname,
                                                missmin[m,"label"],
                                                " not shown = ",val," ug/L"),color=missmin[m,"color"])
      }

    }
  } else {labellist<-list()}

  return(labellist)

}


