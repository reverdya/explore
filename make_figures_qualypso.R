# Alix Reverdy
# Explore 2
# Make figures from Qualypso runs

rm(list=ls())
gc()
dev.off()

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
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/Qualypso/"

var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

lst_indic=c("Q_mean_year","Q_q95_year","VCN10","VCN10_day")
predict=c("time","temp")

select_gcm=c("CNRM-CM5-LR","EC-EARTH","IPSL-CM5A-MR")

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions
Unit="%" #so far all changes are relative

######
#MAIN#
######

sim_stations=read.csv(file = paste0(path_data,"raw/SIM2/Infos_stations_modcou_OK.csv"),sep=";")
sim_stations=sim_stations[!sim_stations$Num_ordre_Modcou %in% c(626,669,763,764,861),]#take only stations with no NA

select_stations=read.xlsx(paste0(path_data,"raw/SIM2/selection_bassins_SIM2.xlsx"))# Sample of watersheds with a diversity of characteristic for plotting
idx=which(sim_stations$Num_ordre_Modcou%in%select_stations$Numero_Modcou)
select_stations=select_stations[order(select_stations$Numero_Modcou),]#because idx is reordered
select_stations$idx=idx

#############################################################
## Times series Qualypso for selected basins QUALYPSO method


for (i in 1:length(lst_indic)){
  folder_out=paste0(path_fig,"3GCM_all_basins/QU/",lst_indic[i],"/")
  dir.create(folder_out)
  for(p in 1:length(predict)){
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],".RData"))
    if(predict[p]=="time"){
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
      pred_name="temps"
      pred_unit=""
    }else{
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
      pred_name="temperature"
      pred_unit="deg C"
    }
    for(b in 1:nrow(select_stations)){
      idx=select_stations$idx[b]
      
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = T)

      plotQUALYPSOMeanChangeAndUncertainties_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOTotalVarianceDecomposition_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOTotalVarianceByScenario_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff = "rcp",nameScenario = "rcp8.5",plain_name_Scen = "RCP 8.5",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
    }
  }
}



##############################################
## Change maps QUALYPSO method


for (i in 1:length(lst_indic)){
  folder_out=paste0(path_fig,"3GCM_all_basins/QU/",lst_indic[i],"/maps/")
  dir.create(folder_out)
  for(p in 1:length(predict)){
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],".RData"))
    if(predict[p]=="time"){
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
      pred_name="temps"
      pred_unit=""
      horiz=2085
      horiz3=c(2030,2050,2085)
    }else{
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
      pred_name="temperature"
      pred_unit="deg C"
      horiz=2
      horiz3=c(1.5,2,3)
    }
    map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out)
    map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],rcp_name = "rcp8.5",rcp_plainname="RCP 8.5",folder_out = folder_out)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "RCM",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out)
  }
  load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_time.RData"))
  map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT_time,horiz=2085,ind_name = lst_indic[i],folder_out = folder_out)
}

## Try adjusting color scale
i=1
p=1
folder_out=paste0(path_fig,"3GCM_all_basins/QU/",lst_indic[i],"/maps/")
load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],".RData"))
if(predict[p]=="time"){
  lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
  pred_name="temps"
  pred_unit=""
  horiz=2085
  horiz3=c(2030,2050,2085)
}else{
  lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
  pred_name="temperature"
  pred_unit="deg C"
  horiz=2
  horiz3=c(1.5,2,3)
}
map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out,scale_col = 0.5,bin_col = 25)
map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],rcp_name = "rcp8.5",rcp_plainname="RCP 8.5",folder_out = folder_out,scale_col = 0.5,bin_col = 25)


#############################################################
## Times series Qualypso for selected basins Linear method


for (i in 1:length(lst_indic)){
  folder_out=paste0(path_fig,"3GCM_all_basins/LM/",lst_indic[i],"/")
  dir.create(folder_out)
  for(p in 1:length(predict)){
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],"_lm.RData"))
    if(predict[p]=="time"){
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
      pred_name="temps"
      pred_unit=""
    }else{
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
      pred_name="temperature"
      pred_unit="deg C"
    }
    for(b in 1:nrow(select_stations)){
      idx=select_stations$idx[b]
      
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
      
      plotQUALYPSOMeanChangeAndUncertainties_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOTotalVarianceDecomposition_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOTotalVarianceByScenario_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff = "rcp",nameScenario = "rcp8.5",plain_name_Scen = "RCP 8.5",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=folder_out)
    }
  }
}



##############################################
## Change maps Linear method


for (i in 1:length(lst_indic)){
  folder_out=paste0(path_fig,"3GCM_all_basins/LM/",lst_indic[i],"/maps/")
  dir.create(folder_out)
  for(p in 1:length(predict)){
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],"_lm.RData"))
    if(predict[p]=="time"){
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
      pred_name="temps"
      pred_unit=""
      horiz=2085
      horiz3=c(2030,2050,2085)
    }else{
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
      pred_name="temperature"
      pred_unit="deg C"
      horiz=2
      horiz3=c(1.5,2,3)
    }
    map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out)
    map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],rcp_name = "rcp8.5",rcp_plainname="RCP 8.5",folder_out = folder_out)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "RCM",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out)
  }
  load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_time_lm.RData"))
  map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT_time,horiz=2085,ind_name = lst_indic[i],folder_out = folder_out)
}
