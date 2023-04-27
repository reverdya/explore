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
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"

load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"refs.Rdata"))
mask_fr=as.vector(refs$mask)

path_river=paste0(path_sig,"/processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig,"/raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions
ref_year=1990

nbcores=detectCores()-2


######
#MAIN#
######


#######################################################################################
## Extract indexes of reference "cities"

ref_cities=read.xlsx(paste0(path_sig,"raw/French_cities/French_cities_coord.xlsx"))
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

#########################################################################################
## Reference departments and zones

ref_dep=read.csv(paste0(path_sig,"processed/SAFRAn_ref_deptmt.csv"))
colnames(ref_dep)=c("id","code","name")
idx_ref_dep=c(12,34,64,65,75)
ref_bv=read.csv(paste0(path_sig,"processed/SAFRAn_ref_bv.csv"))
colnames(ref_bv)=c("id","code","name")
idx_ref_bv=c(206,226,242)
ref_reg=read.csv(paste0(path_sig,"processed/SAFRAn_ref_reg_hyd.csv"))
colnames(ref_reg)=c("id","name")
idx_ref_reg=c(1,4,139)

#########################################################################################
## Reference cities above 1000 m

ref_snow=data.frame(name=c("La Grave","Font-Romeu","Mont-Dore"),xcoord=c(6.30620,2.04383,2.80826),ycoord=c(45.04667,42.50592,45.57661))
ref_snow$col=ref_snow$row=ref_snow$xcoord
for (i in 1:nrow(ref_snow)){
  dist=sqrt((lon-ref_snow$xcoord[i])^2+(lat-ref_snow$ycoord[i])^2)
  min_dist=as.vector(which(dist==min(dist),arr.ind = T))
  ref_snow$row[i]=min_dist[1]
  ref_snow$col[i]=min_dist[2]
}

#############################################################
## Times series Qualypso for selected cities
## lst.QUALYPSOUT is a list of QUALYPSOOUT objects through time (or temperature)

for(v in unique(simu_lst$var)){

# for(v in unique(simu_lst$var)[c(1,2)]){

  dir.create(paste0(path_fig,v,"/"))
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
  # for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(5)]){
    for (preds in c("time","temp")){
    # for (preds in c("time")){
      if (preds == "time"){
        folder_out=paste0(path_fig,v,"/",i,"/")
        load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
        pred_name="temps"
        predict="time"
        pred_unit=""
        xlim=c(1990,2100)
        horiz3=c(2030,2050,2085)
      }
      if (preds == "temp"){
        folder_out=paste0(path_fig,v,"/",i,"/temp/")
        load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp.RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
        pred_name="température"
        predict="temp"
        pred_unit="°C"
        xlim=c(min(lst.QUALYPSOOUT[[1]]$Xfut),max(lst.QUALYPSOOUT[[1]]$Xfut))
        horiz3=c(1.5,2,3,4)
      }
      
      dir.create(folder_out)

      for(c in 1:nrow(ref_cities)){
        idx=ref_cities$idx_masked[c]
        # Warnings "removed rows" okay, due to xlim
        if(preds=="time"){
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = T,xlim=xlim,var=v)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var=v)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var=v)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var=v)
        }
        if(preds=="temp"){
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,includeMean = T)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,includeMean = T)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,includeMean = T)
        }
        plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
        plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
        plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v)
        
        if(v=="evspsblpotAdjust"){
          plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = T,idx_row = ref_cities$row[c],idx_col = ref_cities$col[c],path_temp=path_temp)
          plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = F,idx_row = ref_cities$row[c],idx_col = ref_cities$col[c],path_temp=path_temp)
        }else{
          plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = T,idx_pix = idx,path_temp=path_temp)
          plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var=v,indic = i,simpler = F,idx_pix = idx,path_temp=path_temp)
        }
        plt_bxplt=plotQUALYPSO_boxplot_horiz_rcp(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred = predict,pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = ref_cities$name[c],bv_full_name = ref_cities$name[c],pred_unit = pred_unit,folder_out=folder_out,var=v,indic=i,horiz = horiz3,title=T)
      }
    }
    
  }
}

##############################################
## Maps

for(v in unique(simu_lst$var)){
# for(v in unique(simu_lst$var)[c(1,2)]){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
  # for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(5)]){
    for (preds in c("time","temp")){
    # for (preds in c("time")){
      if (preds == "time"){
        folder_out=paste0(path_fig,v,"/",i,"/maps/")
        load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
        pred_name="temps"
        predict="time"
        pred_unit=""
        xlim=c(1990,2100)
        horiz=2085
        horiz3=c(2030,2050,2085)
      }
      if (preds == "temp"){
        folder_out=paste0(path_fig,v,"/",i,"/temp/maps/")
        load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp.RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
        pred_name="température"
        predict="temp"
        pred_unit="°C"
        xlim=c(min(lst.QUALYPSOOUT[[1]]$Xfut),max(lst.QUALYPSOOUT[[1]]$Xfut))
        horiz=3
        horiz3=c(1.5,2,3,4)
      }
      dir.create(folder_out)
      freq_col=0.99
      
      if(preds=="time"){
        map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v,nbcores=nbcores)
        map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz=horiz,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v,ref0=ref_year)
        map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),rcp_name = "rcp85",rcp_plainname="RCP 8.5",folder_out = folder_out,freq_col=freq_col,pix=T,var=v,nbcores=nbcores)
        
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
      
        map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="rcp85",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
      }
      if(preds=="temp"){
        map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),rcp_name = "",rcp_plainname="",folder_out = folder_out,freq_col=freq_col,pix=T,var=v,nbcores=nbcores,path_temp=path_temp)
        
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
      }
           
      map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
      map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
      map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = folder_out,freq_col=freq_col,pix=T,var=v)
        
      map_var_part(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out =folder_out,pix=T,var=v)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varres",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="vartot",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="incert",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="mean",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = folder_out,pix=T,var=v,freq_col=freq_col)
      
    }
  }
}



###################################################################################################
## Plot map of reference (1990) value of indicator mean response


for(v in unique(simu_lst$var)){
# for(v in unique(simu_lst$var)[c(1,2)]){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
  # for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(5)]){
    folder_out=paste0(path_fig,v,"/",i,"/maps/")
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_allyears.RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
    exut$idx=seq(1:nrow(exut))
    exut$val=apply(lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phi[,,1],1,mean)
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

