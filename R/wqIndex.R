#' Calculate CCME Water Quality Index
#'
#' @description Calculate CCME Water Quality Index
#' @param dat Data frame with water quality or sediment quality data.
#' @param group character vector with column(s) for grouping.
#' @param gl name of guideline column
#' @param param name of params column
#'
#' @return Original dataframe with sediment quality columns add to the end
#' @export
#'
#' @examples


wqIndex<-function(dat=NA,group=NA,
                  gl="gl",
                  param="param"){

  df2<-dat %>%
    ungroup() %>%
    mutate(fail=ifelse(value>get(gl),1,0)) %>% # determine if sample exceeds GL
    mutate(excursion=(value/get(gl))-1)

  # calculate the number
  f1Failvar<-df2 %>%
    filter(fail==1) %>% # filter for exceedances
    dplyr::select(group,param) %>%
    distinct() %>%  # pull out unique values - only what total number of parameters which had at least 1 exceedance
    group_by_at(group) %>%
    summarize(nFailVar=length(unique(param)))

  # amplitude summary
  f3Amp<-df2 %>%
    group_by_at(group) %>%
    mutate(nall=n()) %>% # number of tests
    filter(fail==1) %>% # only tests that fail
    summarize(sumExcursion=sum(excursion),nTests=unique(nall)) %>%
    mutate(nse=sumExcursion/nTests) %>%
    mutate(f3=nse/(0.01*nse+0.01))

  fullss<-df2%>%
    group_by_at(group) %>%
    summarize(nall=n(),
              nGL=length(unique(param)),
              nFailTest=sum(fail)) %>%
    left_join(f1Failvar) %>%
    mutate(f1=nFailVar/nGL*100,
           f2=nFailTest/nall*100) %>%
    left_join(f3Amp %>% dplyr::select(-nTests)) %>%
    mutate(wqIndex=100-(sqrt(f1^2+f2^2+f3^2)/1.732))

}
