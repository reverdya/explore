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

nbcores=detectCores()-2

ref_year=1990

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

for(v in unique(simu_lst$var)[c(1,2)]){

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
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = T,xlim=xlim,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var=v)
      if(v=="evspsblpotAdjust"){
        plotQUALYPSO_summary_change(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = T,idx_row = ref_cities$row[c],idx_col = ref_cities$col[c])
        plotQUALYPSO_summary_change(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = F,)
      }else{
        plotQUALYPSO_summary_change(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = T,idx_pix = idx)
        plotQUALYPSO_summary_change(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = F,idx_pix = idx)
      }
      plt_bxplt=plotQUALYPSO_boxplot_horiz_rcp(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred = predict,pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,var=v,indic=i,horiz = c(2030,2050,2085))
    }
  }
}

##############################################
## Maps

# for(v in unique(simu_lst$var)){
for(v in unique(simu_lst$var)[c(1,2)]){
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
    
    map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v,nbcores=nbcores)
    map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),rcp_name = "rcp85",rcp_plainname="RCP 8.5",folder_out = folder_out,freq_col=freq_col,pix=T,var=v,nbcores=nbcores)

    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
    map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)

    map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz=horiz,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v,ref0=ref_year)
    
    map_var_part(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = 2085,pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out =folder_out,pix=T,var=v)
    map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = 2085,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
    map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varres",horiz = 2085,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
    map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="vartot",horiz = 2085,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
    map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="incert",horiz = 2085,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
    map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="mean",horiz = 2085,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
    map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="rcp85",horiz = 2085,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
    

  }
}



###################################################################################################
## Plot map of reference (1990) value of indicator mean response


# for(v in unique(simu_lst$var)){
for(v in unique(simu_lst$var)[c(1,2)]){
  #for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(17)]){
    folder_out=paste0(path_fig,v,"/",i,"/maps/")
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_lm.RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
    exut$idx=seq(1:nrow(exut))
    exut$val=unlist(lapply(lst.QUALYPSOOUT, function(x) mean(x$CLIMATEESPONSE$phi[,which(x$Xfut==ref_year)])))
    colnames(exut)=c("x","y","idx","val")
    
    q99=quantile(exut$val,probs=0.99)
    q01=quantile(exut$val,probs=(1-0.99))
    lim_col=as.numeric(c(q01,q99))
    lim_col=round(lim_col)#arrondi au 1 le plus proche
    br=round(seq(lim_col[1],lim_col[2],length.out=6))
    if(v=="tasAdjust"){br=round(seq(lim_col[1],lim_col[2],length.out=6),1)}

    plt=base_map_grid(data = exut,val_name = "val")
    plt=plt+
      guides(fill=guide_colorbar(barwidth = 2, barheight = 15,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))+
      binned_scale(aesthetics = "fill",scale_name = "toto",name = "",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),limits=lim_col,oob=squish,show.limits = T,breaks=br,labels= c(paste0(">",br[1]),br[2:5],paste0("<",br[6])) )+#that way because stepsn deforms colors
      ggtitle(paste0("Valeurs de référence (1990) du ",v,"_",i,"\n(moyenne des fonctions de réponse disponibles)"))
    save.plot(plt,Filename = paste0("ref1990_mean-response_",v,"_",i),Folder = folder_out,Format = "jpeg")
  }
}

