# Alix Reverdy
# Explore 2
# Compare chains out for predictor temperature at 1.5°C for 1, 2 and 3 RCPs

rm(list=ls())
gc()

#########
#LIBRARY#
#########


########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse-indic/compare_predT/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"

ref_year=1990# central year of 1976-2005 reference period

load(paste0(path_data,"simu_lst.Rdata"))

###########
#FUNCTIONS#
###########



######
#MAIN#
######


for(v in unique(simu_lst$var)[c(1,2)]){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(17)]){
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_1rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_1rcp[[1]]$Xfut==1.5)
    chains_1rcp=reconstruct_chains(lst.QUALYPSOOUT_temp_1rcp,idx_space = idx_Xfut)
    rm(lst.QUALYPSOOUT_temp_1rcp)
    gc()
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_2rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_2rcp[[1]]$Xfut==1.5)
    chains_2rcp=reconstruct_chains(lst.QUALYPSOOUT_temp_2rcp,idx_space = idx_Xfut)
    rm(lst.QUALYPSOOUT_temp_2rcp)
    gc()
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_3rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_3rcp[[1]]$Xfut==1.5)
    chains_3rcp=reconstruct_chains(lst.QUALYPSOOUT_temp_3rcp,idx_space = idx_Xfut)
    rm(lst.QUALYPSOOUT_temp_3rcp)
    gc()
    
    ## Compare the 3 runs for RCP8.5
    
    chains1=data.frame(t(chains_1rcp))
    chains2=data.frame(t(chains_2rcp[seq(2,nrow(chains_2rcp),2),]))
    chains3=data.frame(t(chains_3rcp[seq(3,nrow(chains_3rcp),3),]))
    chains1=pivot_longer(chains1,cols=everything(),names_to = "chain",values_to = "rcp1")
    chains2=pivot_longer(chains2,cols=everything(),names_to = "chain",values_to = "rcp2")
    chains3=pivot_longer(chains3,cols=everything(),names_to = "chain",values_to = "rcp3")
    
    data=data.frame(cbind(chains1$rcp1,chains2$rcp2,chains3$rcp3))
    colnames(data)=c("rcp1","rcp2","rcp3")
    plt1=ggplot(data)+
      geom_bin2d(aes(x=rcp3,y=rcp2),binwidth=c(0.01,0.01))+
      scale_fill_viridis_c()+
      coord_equal()+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("3 RCPs")+
      ylab("2 RCPs")
    plt1
    plt2=ggplot(data)+
      geom_bin2d(aes(x=rcp3,y=rcp1),binwidth=c(0.01,0.01))+
      scale_fill_viridis_c()+
      coord_equal()+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("3 RCPs")+
      ylab("RCP 8.5")
    plt2
    
    plt=ggarrange(plt1,plt2,common.legend = T)
    plt
    
    ## Compare intra run 3RCP between RCP
    
    ## Compare ensemble means
    
    
  }
}
  
      