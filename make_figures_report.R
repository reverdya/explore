# Alix Reverdy
# Explore 2
# Make figures from Qualypso runs for report

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

path_data_hydro="C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-hydro/"
path_data_meteo="C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/5_rendu/rapport/figures/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/processed/"
path_hadcrut="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/"


load(file=paste0(path_temp,"T_coef_spline1990toGlob.Rdata"))

path_river=paste0(path_sig,"/processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig,"/raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions
nbcores=detectCores()-2
ref_year=1990

storylines=data.frame(gcm=c("HadGEM2-ES","HadGEM2-ES","CNRM-CM5","EC-EARTH"),rcm=c("ALADIN63","CCLM4-8-17","ALADIN63","HadREM3-GA7"),bc=rep("ADAMONT",4),type=c("HadGEM2-ES/ALADIN63","HadGEM2-ES/CCLM4-8-17","CNRM-CM5/ALADIN63","EC-EARTH/HadREM3-GA7"))#using shorter model names than in meteo




############
#MAIN HYDRO#
############




load(file=paste0(path_data_hydro,"simu_lst.Rdata"))
load(file=paste0(path_data_hydro,"ref.Rdata"))

#######################################################################################
## Extract indexes of reference watersheds LO

ref_LO=ref[ref$n==length(unique(simu_lst$hm)),]
ref_LO$idx=seq(1,nrow(ref_LO))
coordinates(ref_LO) <- c("x_l93", "y_l93")
proj4string(ref_LO) <- CRS("+init=epsg:2154")
ref_LO=spTransform(ref_LO,CRS=CRS("+init=epsg:4326"))
ref_LO=as.data.frame(ref_LO)
colnames(ref_LO)[colnames(ref_LO)=="x"]="x2"
colnames(ref_LO)[colnames(ref_LO)=="y"]="y2"
colnames(ref_LO)[colnames(ref_LO)=="x_l93"]="x"
colnames(ref_LO)[colnames(ref_LO)=="y_l93"]="y"


#######################################################################################
## Extract indexes of reference watersheds FR

# hm_sampleFR=c("CTRIP","GRSD","MORDOR-SD","ORCHIDEE","SMASH")
hm_sampleFR=c("CTRIP","GRSD","MORDOR-SD","ORCHIDEE","SMASH")#Keeping same number of basins as MORDOR-SD for run so that latter it does not decrease, but removing its chains
codes=vector(length=length(hm_sampleFR),mode="list")
for(c in 1:length(hm_sampleFR)){# for each chain
  if(hm_sampleFR[c]!="SMASH"){#because some station not simulated for SMASH
    dir_tmp <- list.files(paste0(path_data_hydro,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*CNRM-CM5*rcp85*ALADIN*ADAMONT*",hm_sampleFR[c],"*")))
  }else{
    dir_tmp <- list.files(paste0(path_data_hydro,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*CNRM-CM5*rcp85*ALADIN*CDFt*",hm_sampleFR[c],"*")))
  }
  pth_tmp=list.files(dir_tmp,full.names=T,recursive = T,include.dirs = F,pattern=glob2rx(paste0("*QA.f*")))
  res=read_fst(pth_tmp)
  colnames(res)=c("gcm","rcp","rcm","bc","hm","code","year","indic")
  codes[[c]]=unique(res$code)
}
basins_FR=Reduce(intersect, codes)
ref_FR=ref[ref$code %in% basins_FR,]
ref_FR$idx=seq(1,nrow(ref_FR))
coordinates(ref_FR) <- c("x_l93", "y_l93")
proj4string(ref_FR) <- CRS("+init=epsg:2154")
ref_FR=spTransform(ref_FR,CRS=CRS("+init=epsg:4326"))
ref_FR=as.data.frame(ref_FR)
colnames(ref_FR)[colnames(ref_FR)=="x"]="x2"
colnames(ref_FR)[colnames(ref_FR)=="y"]="y2"
colnames(ref_FR)[colnames(ref_FR)=="x_l93"]="x"
colnames(ref_FR)[colnames(ref_FR)=="y_l93"]="y"
ref_FR$y=as.numeric(ref_FR$y)
hm_sampleFR=c("CTRIP","GRSD","ORCHIDEE","SMASH")
load(file=paste0(path_data_hydro,"Qualypso/VCN3/VCN3_list_QUALYPSOOUT_time_FR.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
ref_FR$mask_weird_values=(sqrt(lst.QUALYPSOOUT[[1]]$INTERNALVAR)*100)<200#catchments that does not seem to satisfy normality hypothesis

########################################################
## Hydro maps

space="LO"
ref=ref_LO
for (i in unique(simu_lst$indic)){
  preds="time"
  load(file=paste0(path_data_hydro,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time_",space,".RData"))
  lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
  pred_name="temps"
  predict="time"
  pred_unit=""
  xlim=c(1990,2105)
  horiz=2085
  horiz3=c(2030,2050,2085)
  freq_col=0.99
  plt=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "hm",name_eff_plain = "HM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = NA,freq_col=freq_col,pix=F,var="Q",zoom=space)
  plt=plt+
    labs(title=NULL)
  save.plot(dpi=300,plt,Filename = paste0(i,"_LO_HM_Effect_2085"),Folder =path_fig,Format = "jpeg")
  print(i)
}


space="FR"
ref=ref_FR
for (i in unique(simu_lst$indic)){
  preds="time"
  load(file=paste0(path_data_hydro,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time_",space,".RData"))
  lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
  pred_name="temps"
  predict="time"
  pred_unit=""
  xlim=c(1990,2105)
  horiz=2085
  horiz3=c(2030,2050,2085)
  freq_col=0.99
  
  for(hor in horiz3){
    
    plt1=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = NA,freq_col=freq_col,pix=F,var="Q",zoom=space)
    plt1=plt1+
      labs(title=NULL)+
      guides(fill = guide_bins(override.aes=list(size=5),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))+
      theme(strip.text = element_text(size = 8, face = "bold"))
    plt1$layers[[3]]$aes_params$size= 1
    plt2=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = NA,freq_col=freq_col,pix=F,var="Q",zoom=space)
    plt2=plt2+
      labs(title=NULL)+
      guides(fill = guide_bins(override.aes=list(size=5),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))+
      theme(strip.text = element_text(size = 8, face = "bold"))
    plt2$layers[[3]]$aes_params$size= 1
    plt3=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,name_eff = "hm",name_eff_plain = "HM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = NA,freq_col=freq_col,pix=F,var="Q",zoom=space)
    plt3=plt3+
      labs(title=NULL)+
      guides(fill = guide_bins(override.aes=list(size=5),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))+
      theme(strip.text = element_text(size = 8, face = "bold"))
    plt3$layers[[3]]$aes_params$size= 1
    plt=ggarrange(plt1,plt2,plt3,heights=c(3,2,2),nrow=3,ncol=1,align="v",labels=c("A","B","C"))
    save.plot(dpi=96,plt,Filename = paste0(i,"_",hor,"_FR_Effect"),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)
    
    
    plt1=map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = NA,freq_col=freq_col,pix=F,var="Q",nbcores=nbcores,zoom=space)
    plt1=plt1+
      labs(title=NULL)+
      guides(fill = guide_bins(override.aes=list(shape=22,size=5),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))+
      theme(legend.box = "horizontal")+
      guides(shape = guide_legend(override.aes=list(fill="grey",size=3),label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))
    plt1$layers[[3]]$aes_params$size= 1
    plt2=map_var_part(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out =NA,pix=F,var="Q",zoom=space,title = F)
    plt2=plt2+
      labs(title=NULL)
    plt2$layers[[3]]$aes_params$size= 1
    plt3=map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = hor,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = i,ind_name_full=i,folder_out = NA,pix=F,var="Q",freq_col=freq_col,zoom=space)
    plt3=plt3+
      labs(title=NULL)+
      guides(fill = guide_bins(override.aes=list(shape=22,size=5),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold"),title.position = "right"))
    plt3$layers[[3]]$aes_params$size= 1
    plt=ggarrange(plt1,plt2,plt3,heights=c(1.75,2,0.75),nrow=3,ncol=1,align="v",labels=c("A","B","C"))
    save.plot(dpi=96,plt,Filename = paste0(i,"_",hor,"_FR_change-incert"),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)
    
    
  }
}



############
#MAIN METEO#
############

load(file=paste0(path_data_meteo,"simu_lst.Rdata"))
load(file=paste0(path_data_meteo,"refs.Rdata"))
load(file=paste0(path_temp,"T_coef_spline1990toGlob.Rdata"))
mask_fr=as.vector(refs$mask)
mask_fr=mask_fr[mask_fr!=0]
mask_fr_prsn=as.vector(refs$mask_prsn)
mask_fr_prsn=mask_fr_prsn[mask_fr_prsn!=0]

########################################################
## Meteo maps

for(v in unique(simu_lst$var)){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
    load(file=paste0(path_data_meteo,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
    lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
    pred_name="temps"
    predict="time"
    pred_unit=""
    xlim=c(1990,2105)
    horiz=2085
    horiz3=c(2030,2050,2085)
    freq_col=0.99

    for(hor in horiz3){
    
      plt1=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,name_eff = "gcm",name_eff_plain = "GCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v)
      plt1=plt1+
        labs(title=NULL)+
        guides(fill=guide_colorbar(barwidth = 1.5, barheight = 7.5,label.theme = element_text(size = 8, face = c("bold"),color=c("black")),title.theme=element_text(size = 12, face = "bold")))+
        theme(strip.text = element_text(size = 8, face = "bold"))
      plt2=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,name_eff = "rcm",name_eff_plain = "RCM",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v)
      plt2=plt2+
        labs(title=NULL)+
        guides(fill=guide_colorbar(barwidth = 1.5, barheight =7.5,label.theme = element_text(size =8, face = c("bold"),color=c("black")),title.theme=element_text(size = 12, face = "bold")))+
        theme(strip.text = element_text(size = 8, face = "bold"))
      plt3=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,name_eff = "bc",name_eff_plain = "BC",pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v)
      plt3=plt3+
        labs(title=NULL)+
        guides(fill=guide_colorbar(barwidth = 1.5, barheight = 7.5,label.theme = element_text(size = 8, face = c("bold"),color=c("black")),title.theme=element_text(size = 12, face = "bold")))+
        theme(strip.text = element_text(size = 8, face = "bold"))
      plt=ggarrange(plt1,plt2,plt3,heights=c(2,3,1),nrow=3,ncol=1,align="v",labels=c("A","B","C"))
      save.plot(dpi=96,plt,Filename = paste0(v,"_",i,"_",hor,"_Effect"),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)
      
      
      
      plt1=map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out =NA,freq_col=freq_col,pix=T,var=v,nbcores=nbcores)
      plt1=plt1+
        labs(title=NULL)+
        guides(fill=guide_colorbar(barwidth = 1.5, barheight = 7.5,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))+
        theme(legend.box = "horizontal")+
        theme(legend.key = element_rect(color="grey",size=0.1),legend.title = element_text(face = "bold",size = 12),legend.text = element_text(face = "bold",size = 8))
      plt2=map_var_part(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out =NA,pix=T,var=v,title = F)
      plt2=plt2+
        labs(title=NULL)
      plt3=map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = hor,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = NA,pix=T,var=v,freq_col=freq_col)
      plt3=plt3+
        labs(title=NULL)+
        guides(fill=guide_colorbar(barwidth = 1.5, barheight = 7.5,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold"),title.position = "right"))
      plt=ggarrange(plt1,plt2,plt3,heights=c(1.75,2,0.75),nrow=3,ncol=1,align="v",labels=c("A","B","C"))
      save.plot(dpi=96,plt,Filename = paste0(v,"_",i,"_",hor,"_change-incert"),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)
    }
  }
}