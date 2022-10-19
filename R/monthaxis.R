#' Add Years and Months to x-axis labels.
#'
#' @description Add Text Boxes to Plot when guidelines exceed maximum range of plot.
#' @param x Plots to change x-axis. The x-axis needs to be dates
#' @param yrstart
#' @param yrend
#' @param months
#' @param minusM
#' @param addM
#' @param monthsize
#' @param yrtick
#' @param mntick
#' @param yearsize
#' @param rotyear
#'
#'
#' @return Table with labels and colours.
#' @export
#'
#' @examples

monthaxis<-function(x,
                    yrstart,
                    yrend,
                    months=F,
                    minusM=4,
                    addM=0,
                    monthsize=10,
                    yrtick=0.14,
                    mntick=0.06,
                    yearsize=10,
                    rotyear=0){
  minusM=minusM-1
  p1<-x

  if(is.na(minusM)){
    # determine x mins and maxes
    xmax<-ggplot_build(p1)$layout$panel_params[[1]]$x.range[2]
    maxdate<-as.Date(xmax,origin='1970-01-01')

    xmin<-ggplot_build(p1)$layout$panel_params[[1]]$x.range[1]
    mindate<-as.Date(xmin,origin='1970-01-01')

    # determine the breaks for the date labels
    yrticks<-as.Date(paste0("1/1/",seq(yrstart,yrend,by=1)),"%d/%m/%Y")

    # month tick set up
    monlet<-c("J","F","M","A","M","J","J","A","S","O","N","D")
    monthlabs<-rep(monlet,yrend-yrstart)
    monthbreaks<-seq(mindate+14,maxdate,by="month")
    monthticks<-seq(mindate,maxdate,by="month")
  } else{


    mindate<-as.Date(paste0("1/",12-minusM,"/",yrstart),"%d/%m/%Y")
    #maxdate<-as.Date(paste0("1/1/",yrend),"%d/%m/%Y")
    if(addM>0){
      maxdate<-as.Date(paste0("1/",1+(addM-1),"/",yrend),"%d/%m/%Y")}else{
        maxdate<-as.Date(paste0("1/1/",yrend),"%d/%m/%Y")
      }

    # determine the breaks for the date labels
    yrticks<-as.Date(paste0("1/1/",seq(yrstart,yrend,by=1)),"%d/%m/%Y")
    yrticks[1]<-mindate

    if(addM>0){yrticks[length(yrticks)+1]<-maxdate}
    #yrticks<-as.Date(paste0("1/1/",seq(yrstart+1,yrend,by=1)),"%d/%m/%Y")

    # month tick set up
    monlet<-c("J","F","M","A","M","J","J","A","S","O","N","D")
    monthlabs<-rep(monlet,yrend-yrstart)
    monthlabs<-monthlabs[c(12-minusM):length(monthlabs)]
    if(addM>0){monthlabs<-c(monthlabs,monlet[1:(addM-1)])}

    monthbreaks<-seq(mindate+14,maxdate,by="month")
    monthticks<-seq(mindate,maxdate,by="month")
  }

  ymax<-ggplot_build(p1)$layout$panel_params[[1]]$y.range[2]
  maxy<-ggplot_build(p1)$layout$panel_params[[1]]$y.range[2]
  axismins<-ggplot_build(p1)$layout$panel_params[[1]]$y.range[1]
  #if(logscale==T){ymax<-10^(ymax);maxy<-10^(maxy);axismins<-10^(axismins)}


  if (months==T){

    p1<-p1 + scale_x_date(limits=c(mindate,maxdate),
                          breaks = monthbreaks, labels = as.character(monthlabs),
                          expand=c(0,0))

    p1<-p1+theme(axis.ticks.x=element_blank(),
                 axis.title.x=element_blank(),
                 axis.text.x=element_text(size=monthsize,margin = margin(t = 0)))
    # add year ticks
    for (j in 1:length(yrticks)){
      p1<-p1+annotation_custom(grid::segmentsGrob(gp = grid::gpar(col = "black", lwd = 0.5)),
                               xmin=yrticks[j], xmax=yrticks[j],
                               ymin=axismins+(axismins-ymax)*yrtick, ymax=axismins)}


    p1<-p1 + scale_x_date(limits=c(mindate,maxdate),
                          breaks = monthbreaks, labels = as.character(monthlabs),
                          expand=c(0,0))
    p1<-p1+theme(axis.ticks.x=element_blank(),axis.title.x=element_blank())


    # add month ticks
    for (j in 1:length(monthticks)){
      p1<-p1+annotation_custom(grid::segmentsGrob(gp = grid::gpar(col = "black", lwd = 0.5)),
                               xmin=monthticks[j], xmax=monthticks[j],
                               ymin=axismins+(axismins-ymax)*mntick, ymax=axismins)}

    if (is.na(minusM)) {
      # add year names
      yearlabs<-seq(yrstart,yrend-1,by=1)
      yearlocs<-as.Date(paste0("1/7/",yearlabs),"%d/%m/%Y")
      for (j in 1:length(yearlabs)){
        p1<-p1+annotation_custom(grid::textGrob(yearlabs[j],gp=grid::gpar(fontsize=yearsize,
                                                                          fontfamily = "ArialMT"),rotyear),
                                 xmin=yearlocs[j], xmax=yearlocs[j],
                                 ymin=axismins+(axismins-ymax)*(yrtick-0.02), ymax=axismins+(axismins-ymax)*(yrtick-0.02))
      }
    } else{

      yearlabs<-seq(yrstart,yrend-1,by=1)
      if(addM>0){yearlabs[length(yearlabs)+1]<-yrend}
      yearlocs<-as.Date(paste0("1/7/",yearlabs),"%d/%m/%Y")
      locs1<-as.Date(paste0("1/",12-median(1:minusM)+1,"/",yrstart),"%d/%m/%Y")
      if(addM>0){yearlocs[length(yearlocs)]<-as.Date(paste0("1/",round(median(1:addM))+1,"/",yrend),"%d/%m/%Y")}
      yearlocs[1]<-locs1

      for (j in 1:length(yearlabs)){
        p1<-p1+annotation_custom(textGrob(yearlabs[j],gp=gpar(fontsize=yearsize,
                                                              fontfamily="ArialMT",rotyear)),
                                 xmin=yearlocs[j], xmax=yearlocs[j],
                                 ymin=axismins+(axismins-ymax)*(yrtick-0.02), ymax=axismins+(axismins-ymax)*(yrtick-0.02))
      }
    }

  }
  if (months==F){
    yearlabs<-seq(yrstart,yrend-1,by=1)
    yearlocs<-as.Date(paste0("1/7/",yearlabs),"%d/%m/%Y")
    p1<-p1 + scale_x_date(limits=c(mindate,maxdate),
                          breaks = yearlocs, labels = yearlabs,
                          expand=c(0,0))

    p1<-p1+theme(axis.ticks.x=element_blank(),axis.title.x=element_blank())
    # rotate axis
    if (rotyear!=0){p1<-p1+theme(axis.text.x = element_text(angle = 90, hjust = 0,vjust=0.5))}

    # add in larger tickmarks for breaks in year
    for (j in 1:length(yrticks)){
      p1<-p1+annotation_custom(segmentsGrob(gp = gpar(col = "black", lwd = 0.5)),
                               xmin=yrticks[j], xmax=yrticks[j],
                               ymin=axismins-(abs(axismins-ymax))*yrtick, ymax=axismins)}
    p1<-p1+coord_cartesian(clip="off")
  }


  p1<-p1+coord_cartesian(clip="off")


}


