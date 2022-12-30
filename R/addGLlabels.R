#' Add Guideline Values to Table
#'
#' @description Add guideline values to table.
#' @param labels vector of labels to add.
#' @param pl plot that labels will be added to.
#' @param pltcol Vector of colors for the labels.
#' @param labsep Multiplier to separate text labels
#'
#' @return Plot with labels added.
#' @export
#'
#' @examples

addGLlables<-function(labels=NA,
                      pl=NA,
                      pltcol=NA,
                      labsep=0.1,
                      xdate=T){

  p1=pl

  # determine y mins and maxes
  ymax<-ggplot_build(p1)$layout$panel_params[[1]]$y.range[2]
  ymin<-ggplot_build(p1)$layout$panel_params[[1]]$y.range[1]
  inv<-summarise_layout(ggplot_build(p1))$yscale[[1]]$trans$inverse
  rnge<-ymax-ymin

  # determine x mins and maxes
  xmax<-ggplot_build(p1)$layout$panel_params[[1]]$x.range[2]
  xmin<-ggplot_build(p1)$layout$panel_params[[1]]$x.range[1]

  if(xdate==T){
    xmax<-as.Date(xmax,origin='1970-01-01')
    xmin<-as.Date(xmin,origin='1970-01-01')
  }

  for (l in 1:length(labels)){
    # l=1
    xnum<-xmin+as.numeric(xmax-xmin)*0.02

    ynum<-(ymax-((rnge)*labsep*l))
    ynum=inv(ynum)

    # ynum<-maxy-(maxy*(0.1*l))
    gllab<-as.character(labels[l])

    p1<-p1+annotate("label",label=paste(gllab),
                    x=xnum,y=ynum,hjust=0,
                    color=as.character(pltcol[l]),
                    label.size=NA,
                    #fill="blue",
                    fill=rgb(1,1,1,alpha=0.7),
                    size=2.5)+geom_segment(x=xnum,xend=xnum,
                                           y=ynum-(ynum*0.04),yend=ynum+(ynum*0.04),
                                           arrow = arrow(length = unit(0.09, "cm"),type="open"),size=0.3,
                                           color=as.character(pltcol[l]))


  }
  p1

}
