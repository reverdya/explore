# Alix Reverdy
# Explore 2
# Make figures from Qualypso runs

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

path_data="C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/Qualypso/"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/French_cities/"

load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"refs.Rdata"))

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions

######
#MAIN#
######


#######################################################################################
## Extract indexes of reference "cities"

ref_cities=read.xlsx(paste0(path_sig,"French_cities_coord.xlsx"))
ref_cities$col=ref_cities$row=ref_cities$xcoord
for (i in 1:nrow(ref_cities)){
  dist=sqrt((refs$lon-ref_cities$xcoord[i])^2+(refs$lat-ref_cities$ycoord[i])^2)
  min_dist=as.vector(which(dist==min(dist),arr.ind = T))
  ref_cities$row[i]=min_dist[1]
  ref_cities$col[i]=min_dist[2]
}
ref_cities$idx=(ref_cities$col-1)*nrow(refs$mask)+ref_cities$row
tmp=c(1:length(as.vector(refs$mask)))
tmp=tmp[as.logical(refs$mask)]
ref_cities$idx_masked=which(tmp %in% ref_cities$idx)

#############################################################
## Times series Qualypso for selected cities

# for(v in unique(simu_lst$var)){
for(v in unique(simu_lst$var)[2]){
  dir.create(paste0(path_fig,v,"/"))
  #for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(17)]){
    folder_out=paste0(path_fig,v,"/",i,"/")
    dir.create(folder_out)
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_lm.RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    pred_name="temps"
    predict="time"
    pred_unit=""
    xlim=c(1990,2100)
    for(c in 1:nrow(ref_cities)){
      idx=ref_cities$idx_masked[c]
      # Warnings "removed rows" okay, due to xlim
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,incert=F,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = T,xlim=xlim,incert=F,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,incert=F,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,incert=F,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,incert=F,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,incert=F,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,incert=F,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,incert=F,var=v)
      plotQUALYPSO_summary_change(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
    }
  }
}

##############################################
## Maps

# for(v in unique(simu_lst$var)){
for(v in unique(simu_lst$var)[2]){
  #for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(17)]){
    folder_out=paste0(path_fig,v,"/",i,"/maps/")
    dir.create(folder_out)
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_lm.RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    pred_name="temps"
    pred="time"
    pred_unit=""
    horiz=2085
    horiz3=c(2030,2050,2085)
    freq_col=0.99
    
    map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    # map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),rcp_name = "rcp8.5",rcp_plainname="RCP 8.5",folder_out = folder_out,freq_col=freq_col,pix=T)
    # 
    # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    # 
    # map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz=horiz,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T)
    

  }
}



######################################################################################
## Plot map of reference (1990) value of indicator for continuous positive indicator

ref_year=1990
lst_indic2=lst_indic[!lst_indic %in% c("VCN10_day")]
name_indic2=name_indic[!lst_indic %in% c("VCN10_day")]
for (i in 1:length(lst_indic2)){
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic2[i],"_list_QUALYPSOOUT_3GCM_time_lm.RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
    exut$val=unlist(lapply(lst.QUALYPSOOUT, function(x) mean(x$CLIMATEESPONSE$phi[,which(x$Xfut==ref_year)])))
    colnames(exut)=c("Num_ordre_Modcou","y","x","val")
    
    plt=base_map_outlets(data = exut,val_name = "val")
    plt=plt+
      #scale_fill_gradientn("",colours = rescale_col(brewer.blues(100),exut$val,scale_col),limits=c(0,lim_col),breaks=seq(0,lim_col,bin_col[i]),oob=squish,labels=c(seq(0,lim_col-bin_col[i],bin_col[i]),paste0("> ",lim_col)))+
      binned_scale(aesthetics = "fill",scale_name = "toto",name="",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",trans="log10")+#that way because stepsn deforms colors
      ggtitle(paste0("Valeurs de référence (1990) du ",name_indic2[i],"\n(moyenne des fonctions de réponse disponibles)"))
    plt$layers[[3]]$aes_params$size=5
    save.plot(plt,Filename = paste0("ref1990_response_",lst_indic2[i]),Folder = path_fig,Format = "jpeg")
}

