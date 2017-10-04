munge<-function(fl){
  #g2b2
  require(dplyr)
  require(readxl)
  require(tidyr)
  AC<-function(fl){
    h<-readxl::read_excel(fl,sheet="Assay Configuration")[,c(1,2)]
    AClist<-unlist(h[,2]) %>% set_names(unlist(h[,1]))
    data.frame(Lot=paste0(AClist["Cartridge Type"] %>% unname(),
                          AClist["Cartridge Lot"] %>% unname()),
               sn=AClist["Cartridge Serial"]%>% unname(),
               fl=AClist["Assay Name"]%>% unname())
  }
  G2F1<-{
    . %>%
      readxl::read_excel(.,sheet="Raw")%>%
      select(.,Measurement,Tick,Well,pH) %>%
      # grab last 3 ticks of measure and take the mean
      group_by(.,Measurement) %>%
      filter(.,Tick %in% c(max(Tick)-c(2,1,0))) %>%
      ungroup(.) %>%
      group_by(.,Measurement,Well) %>%
      summarize(.,meanpH=mean(pH)) %>%
      ungroup(.) %>%
      mutate(.,pHM=c("startpH","finalpH")[Measurement]) %>%
      select(.,-Measurement) %>%
      tidyr::spread(.,key=pHM,value=meanpH)
  }
  FUNs<-list(
    '96'=function(u){
      u %>%
        mutate(.,Col=gsub("[A-Z]","",Well) %>% as.numeric()) %>%
        mutate(.,volHCl=rep(c(0,20,42,67),3)[Col]) %>%
        select(.,-Col) %>%
        mutate(.,deltapH=startpH-finalpH) %>%
        mutate(.,HClMolarity=5/1000) %>%
        mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>%
        mutate(.,volMedia=180/(1000^2)) %>%
        mutate(.,bufferCapacity=molesH/(deltapH*volMedia))
    },
    '24'=function(u){
      u %>%
        mutate(.,volHCl=c(rep(62,6),rep(2*62,6),rep(3*62,6),rep(0,6))) %>%
        mutate(.,deltapH=startpH-finalpH) %>%
        mutate(.,HClMolarity=5/1000) %>%
        mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>%
        mutate(.,volMedia=500/(1000^2)) %>%
        mutate(.,bufferCapacity=molesH/(deltapH*volMedia))
    },
    '8'=function(u){
      u %>%  mutate(.,volHCl=c(0,20,42,67) %>% rep(.,2)) %>%
        mutate(.,deltapH=startpH-finalpH) %>%
        mutate(.,HClMolarity=5/1000) %>%
        mutate(.,molesH=(HClMolarity*(volHCl/1000^2))) %>%
        mutate(.,volMedia=180/(1000^2)) %>%
        mutate(.,bufferCapacity=molesH/(deltapH*volMedia))
    }
  )

  G2F1(fl) %>%
  {FUNs[[.$Well %>% unique() %>% length() %>% as.character()]](.)} %>%
    merge(AC(fl))

}
