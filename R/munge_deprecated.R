munge_depracted<-function(fl){
  #G2BC
  require(dplyr)
  require(readxl)
  require(tidyr)
  # determine assay used
  assay<- gregexpr("inj|titr", tolower(basename(fl))) %>%  regmatches(basename(fl), .) %>% unlist() %>% tolower()
  # read in excel sheet
  fl%>% readxl::read_excel(.,sheet="Raw")%>%
    select(.,Measurement,Tick,Well,pH) %>%
    # grab last 3 ticks of measure and take the mean
    group_by(.,Measurement) %>%
    filter(.,Tick %in% c(max(Tick)-c(2,1,0))) %>%
    ungroup(.) %>%
    group_by(.,Measurement,Well) %>%
    summarize(.,meanpH=mean(pH)) %>%
    ungroup(.) %>%
    ## Bridge,deteremines munge difference for either injection assay or titration assay
    ({
      if (assay=='inj'){
        . %>%
          mutate(pHM=c("m0uL","m20uL","m42uL","m67uL")[Measurement]) %>%
          select(.,-Measurement) %>%
          tidyr::spread(.,key=pHM,value=meanpH) %>%
          rename(.,startpH=m0uL) %>%
          tidyr::gather(.,volHCl,finalpH,-Well,-startpH) %>%
          mutate(volHCl=gsub("[A-Z,a-z]","",volHCl)  %>% as.numeric())
      }else{
        . %>%
          mutate(.,pHM=c("startpH","finalpH")[Measurement]) %>%
          select(.,-Measurement) %>%
          tidyr::spread(.,key=pHM,value=meanpH) %>%
          mutate(Col=as.numeric(gsub("[A-Z]","",Well))) %>%
          mutate(.,volHCl=rep(c(0,20,42,67),3)[Col]) %>%
          select(.,-Col)
      }
    }) %>%
    mutate(.,deltapH=startpH-finalpH) %>%
    mutate(.,HClMolarity=5/1000) %>%
    mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>%
    mutate(.,volMedia=180/(1000^2)) %>%
    mutate(.,bufferCapacity=molesH/(deltapH*volMedia)) %>%
    mutate(file=fl)
}
