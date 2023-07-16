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

path_data="C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-hydro/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/hydro/Qualypso/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/processed/"
path_hadcrut="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/"

load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"ref.Rdata"))
load(file=paste0(pth_temp,"T_coef_spline1990toGlob.Rdata"))

path_river=paste0(path_sig,"/processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig,"/raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions
nbcores=detectCores()-2
ref_year=1990

bv_sample=c("K337301001","K259301001","K118001010")
storylines=data.frame(gcm=c("HadGEM2-ES","HadGEM2-ES","CNRM-CM5","EC-EARTH"),rcm=c("ALADIN63","CCLM4-8-17","ALADIN63","HadREM3-GA7"),bc=rep("ADAMONT",4),type=c("Chaud et humide","Sec en été et chaud","Faibles changements","Sec"))


######
#MAIN#
######


#######################################################################################
## Extract indexes of reference watersheds

ref=ref[ref$n==9,]
ref$idx=seq(1,nrow(ref))
coordinates(ref) <- c("x_l93", "y_l93")
proj4string(ref) <- CRS("+init=epsg:2154")
ref=spTransform(ref,CRS=CRS("+init=epsg:4326"))
ref=as.data.frame(ref)
colnames(ref)[colnames(ref)=="x_l93"]="x"
colnames(ref)[colnames(ref)=="y_l93"]="y"

bv_selec=ref[ref$code %in% bv_sample,]

#############################################################
## Times series Qualypso for selected watersheds
## lst.QUALYPSOUT is a list of QUALYPSOOUT objects through time (or temperature)
for (sp in c("FR","LO")){
  for (i in unique(simu_lst$indic)){
    for (preds in c("time","temp")){
      if (preds == "time"){
        folder_out=paste0(path_fig,i,"/",sp,"/")
        load(file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time_",sp,".RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
        pred_name="temps"
        predict="time"
        pred_unit=""
        xlim=c(1990,2105)
        horiz3=c(2030,2050,2085)
      }
      if (preds == "temp"){
        folder_out=paste0(path_fig,i,"/",sp,"/temp/")
        load(file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_temp_",sp,".RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
        pred_name="température"
        predict="temp"
        pred_unit="°C"
        # xlim=c(min(lst.QUALYPSOOUT[[1]]$Xfut),max(lst.QUALYPSOOUT[[1]]$Xfut))
        xlim=c(min(lst.QUALYPSOOUT[[1]]$Xfut),4)
        horiz3=c(1.5,2,3,4)
      }
      
      dir.create(folder_out)
      
      for(c in 1:nrow(bv_selec)){
        idx=bv_selec$idx[c]
        # Warnings "removed rows" okay, due to xlim
        if(preds=="time"){
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q")
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcp",plain_nameEff = "RCP",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = T,xlim=xlim,var="Q")
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var="Q")
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var="Q")
          # plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var="Q")
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="hm",plain_nameEff = "HM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp85",xlim=xlim,var="Q")
        }
        if(preds=="temp"){
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q",includeMean = T)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q",includeMean = T)
          # plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q",includeMean = T)
          plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="hm",plain_nameEff = "HM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q",includeMean = T)
        }
        plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q")
        plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q")
        # plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q")
        plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="hm",plain_nameEff = "HM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q")
        
        for(storyl in c(T,F)){
          plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$code[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,var="Q",indic = i,idx_pix = idx,path_temp=path_hadcrut,storyl=storyl)
          plotQUALYPSO_boxplot_horiz_rcp(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred = predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,var="Q",indic=i,horiz = horiz3,title=T,storyl=storyl)
        }
      }
    }
    print(i)
  }
  
  
  ## Regime
  # months=c("janv","fevr","mars","avr","mai","juin","juill","aout","sept","oct","nov","dec")
  # folder_out=paste0(path_fig,"regime/")
  # for (preds in c("time","temp")){
  #   lst_lst.QUALYPSOOUT=vector(mode="list")
  #   for(mth in months){
  #     if (preds == "time"){
  #       load(file=paste0(path_data,"Qualypso/QA_",mth,"/QA_",mth,"_list_QUALYPSOOUT_time_FR.RData"))
  #       lst_lst.QUALYPSOOUT[[mth]]=lst.QUALYPSOOUT_time
  #     }
  #     if (preds == "temp"){
  #       load(file=paste0(path_data,"Qualypso/QA_",mth,"/QA_",mth,"_list_QUALYPSOOUT_temp_FR.RData"))
  #       lst_lst.QUALYPSOOUT[[mth]]=lst.QUALYPSOOUT_temp
  #     }
  #   }
  #   if (preds == "time"){
  #     pred_name="temps"
  #     predict="time"
  #     pred_unit=""
  #     horiz=c(2085)
  #   }
  #   if (preds == "temp"){
  #     pred_name="température"
  #     predict="temp"
  #     pred_unit="°C"
  #     horiz=c("1.5","2",3,"4")
  #   }
  #   for(c in 1:nrow(bv_selec)){
  #     idx=bv_selec$idx[c]
  #     for(storyl in c(T,F)){
  #       plotQUALYPSO_regime(lst_lst.QUALYPSOOUT=lst_lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,bv_name = bv_selec$name[c],bv_full_name = bv_selec$name[c],pred_unit = pred_unit,folder_out=folder_out,horiz=horiz,var="Q",title=T,storyl=storyl)
  #     }
  #   }
  # }
}


##############################################
## Maps
for (sp in c("FR","LO")){
  for (i in unique(simu_lst$indic)){
    for (preds in c("time","temp")){
      if (preds == "time"){
        folder_out=paste0(path_fig,i,"/",sp,"/maps/")
        load(file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time_",sp,".RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
        pred_name="temps"
        predict="time"
        pred_unit=""
        xlim=c(1990,2105)
        horiz=2085
        horiz3=c(2030,2050,2085)
      }
      if (preds == "temp"){
        folder_out=paste0(path_fig,i,"/",sp,"/temp/maps/")
        load(file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_temp_",sp,".RData"))
        lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
        pred_name="température"
        predict="temp"
        pred_unit="°C"
        # xlim=c(min(lst.QUALYPSOOUT[[1]]$Xfut),max(lst.QUALYPSOOUT[[1]]$Xfut))
        xlim=c(min(lst.QUALYPSOOUT[[1]]$Xfut),4)
        horiz=3
        horiz3=c(1.5,2,3,4)
      }
      dir.create(folder_out)
      freq_col=0.99
      
      if(preds=="time"){
        map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",nbcores=nbcores,zoom=sp)
        map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz=horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",ref0=ref_year,zoom=sp)
        map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,rcp_name = "rcp85",rcp_plainname="RCP 8.5",folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",nbcores=nbcores,zoom=sp)
        
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp85",horiz = horiz,name_eff = "hm",name_eff_plain = "HM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        
        map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="rcp85",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,pix=F,var="Q",freq_col=freq_col,zoom=sp)
        map_storyline(lst.QUALYPSOOUT = lst.QUALYPSOOUT,RCP = "rcp85",RCP_plainname="RCP8.5",horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp,storylines=storylines)
      }
      if(preds=="temp"){
        map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,rcp_name = "rcp85",rcp_plainname="RCP8.5",folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",nbcores=nbcores,path_temp=path_temp,cat="hydro",zoom=sp)
        
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = horiz,name_eff = "hm",name_eff_plain = "HM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
        
        map_storyline(lst.QUALYPSOOUT = lst.QUALYPSOOUT,RCP = NULL,RCP_plainname=NULL,horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp,storylines=storylines)
      }
      
      map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
      map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
      # map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "bc",name_eff_plain = "BC",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
      map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "hm",name_eff_plain = "HM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,freq_col=freq_col,pix=F,var="Q",zoom=sp)
      
      
      map_var_part(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out =folder_out,pix=F,var="Q",zoom=sp)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,pix=F,var="Q",freq_col=freq_col,zoom=sp)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varres",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,pix=F,var="Q",freq_col=freq_col,zoom=sp)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="vartot",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,pix=F,var="Q",freq_col=freq_col,zoom=sp)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="incert",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,pix=F,var="Q",freq_col=freq_col,zoom=sp)
      map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="mean",horiz = horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = folder_out,pix=F,var="Q",freq_col=freq_col,zoom=sp)
      
      map_limitations(lst.QUALYPSOOUT = lst.QUALYPSOOUT,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out =folder_out,pix=F,var="Q",zoom=sp)
    }
    print(i)
    try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)#otherwise "depth" error linked to plots after some time, maybe not solved because could be linked to resizing Rstudio window and/or plot window
    gc()
  }
}





###################################################################################################
## Plot map of reference (1990) value of indicator mean response

for (sp in c("FR","LO")){
  for (i in unique(simu_lst$indic)){
    folder_out=paste0(path_fig,i,"/",sp,"/maps/")
    dir.create(folder_out)
    load(file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time_",sp,".RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    exut=data.frame(x=as.vector(ref$x),y=as.vector(ref$y))
    exut$idx=seq(1:nrow(exut))
    exut$val=apply(lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phi[,,1],1,mean)
    colnames(exut)=c("x","y","idx","val")
    
    q99=quantile(exut$val,probs=0.99)
    q01=quantile(exut$val,probs=(1-0.99))
    lim_col=as.numeric(c(q01,q99))
    lim_col=round(lim_col)#arrondi au 1 le plus proche
    br=round(seq(lim_col[1],lim_col[2],length.out=6))
  
    plt=base_map_outlets(data = exut,val_name = "val",zoom=sp)
    plt=plt+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))+
      binned_scale(aesthetics = "fill",scale_name = "toto",name = "",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),limits=lim_col,oob=squish,show.limits = T,breaks=br,labels= c(paste0(">",br[1]),br[2:5],paste0("<",br[6])) )+#that way because stepsn deforms colors
    ggtitle(paste0("Valeurs de référence (1990) du ",i,"\n(moyenne des fonctions de réponse disponibles)"))
    save.plot(plt,Filename = paste0("ref1990_mean-response_",i),Folder = folder_out,Format = "jpeg")
    print(i)
  }
}

