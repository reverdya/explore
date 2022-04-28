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
## Times series Qualypso for selected basins Linear method


for (i in 1:length(lst_indic)){
  folder_out=paste0(path_fig,"3GCM_all_basins/",lst_indic[i],"/")
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
  folder_out=paste0(path_fig,"3GCM_all_basins/",lst_indic[i],"/maps/")
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
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],folder_out = folder_out)
  }
  load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_time_lm.RData"))
  map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT_time,horiz=2085,ind_name = lst_indic[i],folder_out = folder_out)
}



############################################
## Plot GCM temperature

load(file = paste0(path_data,"processed/simu_lst.Rdata"))
first_ref_year=1975
last_ref_year=2005
first_full_year=1972# from raw data filenames
last_full_year=2098# from raw data filenames
vecYears=seq(first_full_year,last_full_year,1)

tmp=format_global_tas(path_data,first_full_year,last_full_year,simu_lst,first_ref_year,last_ref_year)
mat_Globaltas_gcm=tmp[[3]]
mat_Globaltas_gcm=data.frame(apply(mat_Globaltas_gcm,MARGIN = 2,function(x) smooth.spline(x=vecYears,y=x,spar = 1)$y))
mat_Globaltas_gcm$year=vecYears

data=gather(mat_Globaltas_gcm,key = "chain",value = "val",-year)
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))
data$gcm=unlist(lapply(strsplit(data$chain,"_"),function(x) x[2]))

plt=ggplot(data)+
  geom_line(aes(x=year,y=val,col=rcp,lty=gcm),size=1.2)+
  xlab("")+
  ylab("Changement de température planétaire (deg C)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = col_3rcp,labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  scale_linetype_discrete("GCM")+
  ggtitle("Changement de temperature planetaire pour les differents RCP/GCM\npar rapport a la reference 1850-1900 (1860-1900 pour HadGEM2)")
save.plot(plt,Filename = "global_tas",Folder = path_fig,Format = "jpeg")

###########################################################################################################################
## Scatter plot global temperature (smoothed or not) VS yearly mean discharge for reference watersheds (smoothed or not)

mat_Globaltas_gcm=tmp[[3]]
mat_Globaltas_spline=tmp[[1]]
mat_Globaltas=vector(length=nrow(simu_lst),mode="list")
vec_global_tas_gcm=unlist(lapply(colnames(mat_Globaltas_gcm),function(x) strsplit(x,"_")[[1]][2]))
vec_global_tas_rcp=unlist(lapply(colnames(mat_Globaltas_gcm),function(x) strsplit(x,"_")[[1]][1]))
for (i in 1:nrow(simu_lst)){
  mat_Globaltas[[i]]=mat_Globaltas_gcm[,which(vec_global_tas_gcm==simu_lst[i,]$gcm & vec_global_tas_rcp==sub(".","",simu_lst[i,]$rcp,fixed=T))]
}
mat_Globaltas=do.call(cbind,mat_Globaltas)

load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/Q_mean_year_list_QUALYPSOOUT_3GCM_time.RData"))
for (i in 1:nrow(select_stations)){
  all_chains=vector(length=nrow(simu_lst),mode="list")
  for (j in 1:nrow(simu_lst)){
    load(paste0(path_data,"processed/indic_hydro/Q_mean_year_",simu_lst$rcp[j],"_",simu_lst$gcm[j],"_",simu_lst$rcm[j],"_",simu_lst$bc[j],"_",simu_lst$hm[j],".Rdata"))
    all_chains[[j]]=res
  }
  ClimateProjections=lapply(all_chains, function(x) x[,c(1,select_stations$idx[i])])
  ClimateProjections=lapply(ClimateProjections,function(x) x[x$year>=first_full_year & x$year<=last_full_year,][,2])
  ClimateProjections=t(do.call(rbind,ClimateProjections))
  ClimateProjections_spline=apply(ClimateProjections,MARGIN = 2,function(x) smooth.spline(x=vecYears,y=x,spar = 1.1)$y)
  data=data.frame(tas=as.vector(mat_Globaltas),tas_spline=as.vector(mat_Globaltas_spline),q=as.vector(ClimateProjections),q_spline=as.vector(ClimateProjections_spline))
  
  plt=ggplot(data)+
    geom_point(aes(x=tas,y=q,col="raw"),size=0.7,alpha=0.6)+
    geom_point(aes(x=tas_spline,y=q_spline,col="spline"),size=0.7,alpha=0.6)+
    scale_color_manual("",values = c("raw"=ipcc_2col[1],"spline"=ipcc_2col[2]),labels=c("Brut","Spline"))+
    xlab("Changement de température planétaire (deg C)")+
    ylab("Module annuel")+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    guides(colour = guide_legend(override.aes = list(size=3)))+
    ggtitle(paste0("Module annuel du bassin ",select_stations$Nom[i],"\nen fonction du changeemnt de temperature planetaire"))
  save.plot(plt,Filename = paste0("global_tasVSmodule_",select_stations$Nom[i]),Folder = path_fig,Format = "jpeg")
}

##################################################################################
## Plot map of reference (1990) value of indicator for continuous positive indicator (of discharge)

ref_year=1990
scale_col=0.2
bin_col=c(250,500,50)
lst_indic2=lst_indic[lst_indic!="VCN10_day"]
for (i in 1:length(lst_indic2)){
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic2[i],"_list_QUALYPSOOUT_3GCM_time.RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
    exut$val=unlist(lapply(lst.QUALYPSOOUT, function(x) mean(x$CLIMATEESPONSE$phi[,which(x$Xfut==ref_year)])))
    colnames(exut)=c("Num_ordre_Modcou","y","x","val")
    
    q995=quantile(exut$val[exut$val>=0],probs=0.995)
    lim_col=q995
    lim_col=round(lim_col/bin_col[i])*bin_col[i]#arrondi au bin_col[i] le plus proche
    
    plt=base_map_outlets(data = exut,val_name = "val")
    plt=plt+
      scale_fill_gradientn("",colours = rescale_col(brewer.blues(100),exut$val,scale_col),limits=c(0,lim_col),breaks=seq(0,lim_col,bin_col[i]),oob=squish,labels=c(seq(0,lim_col-bin_col[i],bin_col[i]),paste0("> ",lim_col)))+
      ggtitle(paste0("Valeurs de reference (1990) de ",lst_indic2[i],"\n(moyenne des fonctions de reponse disponibles)"))
    save.plot(plt,Filename = paste0("ref1990_response_",lst_indic2[i]),Folder = path_fig,Format = "jpeg")
}