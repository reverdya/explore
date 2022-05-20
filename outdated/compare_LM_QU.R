# Alix Reverdy
# Explore 2
# Compare results from method linear and method QUALYPSO

rm(list=ls())
gc()

#########
#LIBRARY#
#########

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R')

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/Qualypso/3GCM_all_basins/compare_QU_LM/"

var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

load(file=paste0(path_data,"processed/lst_indic.Rdata"))
predict=c("time","temp")

######
#MAIN#
######

for (i in 1:length(lst_indic)){
  for(p in 1:length(predict)){
    
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],".RData"))
    if(predict[p]=="time"){
      lst.QUALYPSOOUT_QU=lst.QUALYPSOOUT_time
      pred="temps"
    }else{
      lst.QUALYPSOOUT_QU=lst.QUALYPSOOUT_temp
      pred="temperature"
    }
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],"_lm.RData"))
    if(predict[p]=="time"){
      lst.QUALYPSOOUT_LM=lst.QUALYPSOOUT_time
    }else{
      lst.QUALYPSOOUT_LM=lst.QUALYPSOOUT_temp
    }
    
    plot_comp_anova(LM=lst.QUALYPSOOUT_LM,QU=lst.QUALYPSOOUT_QU,type = "grandmean",nameEff = NULL,pred_name = pred,ind_name = lst_indic[i],var_name = "Moyenne d'ensemble",folder_out = path_fig,interactive_plt = F)
    plot_comp_anova(LM=lst.QUALYPSOOUT_LM,QU=lst.QUALYPSOOUT_QU,type = "resvar",nameEff = NULL,pred_name = pred,ind_name = lst_indic[i],var_name = "Variabilité résiduelle",folder_out = path_fig,interactive_plt = F)
    plot_comp_anova(LM=lst.QUALYPSOOUT_LM,QU=lst.QUALYPSOOUT_QU,type = "eff",nameEff = "rcp",pred_name = pred,ind_name = lst_indic[i],var_name = "Effet RCP",folder_out = path_fig,interactive_plt = F)
    plot_comp_anova(LM=lst.QUALYPSOOUT_LM,QU=lst.QUALYPSOOUT_QU,type = "eff",nameEff = "gcm",pred_name = pred,ind_name = lst_indic[i],var_name = "Effet GCM",folder_out = path_fig,interactive_plt = F)
    plot_comp_anova(LM=lst.QUALYPSOOUT_LM,QU=lst.QUALYPSOOUT_QU,type = "eff",nameEff = "rcm",pred_name = pred,ind_name = lst_indic[i],var_name = "Effet RCM",folder_out = path_fig,interactive_plt = F)
  }
}