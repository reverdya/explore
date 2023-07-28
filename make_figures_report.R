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


################################################
## Chroniques hydro

bv_sample_FR=c("W323000000","A850061001")
bv_selec_FR=ref_FR[ref_FR$code %in% bv_sample_FR,]

space="FR"
ref=ref_FR
storyl=F
i="QA"
load(file=paste0(path_data_hydro,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time_",space,".RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
pred_name="temps"
predict="time"
pred_unit=""
xlim=c(1990,2105)
  
for(c in 1:nrow(bv_selec_FR)){
  idx=bv_selec_FR$idx[c]
  plt1=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name =  bv_selec_FR$name[c],bv_full_name =  bv_selec_FR$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var="Q")
  plt1=plt1+
    labs(title=NULL)+
    theme_bw(base_size = 12)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank(),axis.title.x=element_blank())
  plt2=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name =  bv_selec_FR$name[c],bv_full_name =  bv_selec_FR$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var="Q")
  plt2=plt2+
    labs(title=NULL)+
    theme_bw(base_size = 12)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank(),axis.title.x=element_blank())
  plt3=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="hm",plain_nameEff = "HM",pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name =  bv_selec_FR$name[c],bv_full_name =  bv_selec_FR$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var="Q")
  plt3=plt3+
    labs(title=NULL)+
    theme_bw(base_size = 12)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank(),axis.title.x=element_blank())
  
  plt4=plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,ind_name = i,ind_name_full=i,bv_name =  bv_selec_FR$code[c],bv_full_name =  bv_selec_FR$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var="Q",indic = i,idx_pix = idx,path_hadcrut=path_hadcrut,path_processed=path_temp,storyl=storyl)

  plt=ggarrange(plt4,ggarrange(plt1,plt2,plt3,nrow=2,ncol=2,align="v",labels="B"),nrow=2,ncol=1,align="v",labels="A",heights=c(1.5,1))
  save.plot(dpi=96,plt,Filename = paste0(i,"_chronique_",bv_selec_FR$code[c]),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)
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



################################################
## Chroniques meteo

basHy=read.csv(paste0(path_sig,"processed/SAFRAN_ref_basHy.csv"))
colnames(basHy)=c("id","code","name")
idx_ref_bas=c(3,5)

storyl=F
v="prtotAdjust"
i="seassum_JJA"
load(file=paste0(path_data_meteo,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_bas.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
pred_name="temps"
predict="time"
pred_unit=""
xlim=c(1990,2105)


for(c in idx_ref_bas){
  idx=c
  plt1=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = basHy$name[c],bv_full_name = basHy$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var=v)
  plt1=plt1+
    labs(title=NULL)+
    theme_bw(base_size = 12)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank(),axis.title.x=element_blank())
  plt2=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = basHy$name[c],bv_full_name = basHy$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var=v)
  plt2=plt2+
    labs(title=NULL)+
    theme_bw(base_size = 12)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank(),axis.title.x=element_blank())
  plt3=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = basHy$name[c],bv_full_name = basHy$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var=v)
  plt3=plt3+
    labs(title=NULL)+
    theme_bw(base_size = 12)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank(),axis.title.x=element_blank())
  
  plt4=plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred=predict,pred_name = pred_name,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),bv_name = basHy$name[c],bv_full_name = basHy$name[c],pred_unit = pred_unit,folder_out=NA,xlim=xlim,var=v,indic = i,idx_pix = idx,path_hadcrut=path_hadcrut,path_processed=path_temp,storyl=storyl,type="bas")
  
  plt=ggarrange(plt4,ggarrange(plt1,plt2,plt3,nrow=2,ncol=2,align="v",labels="B"),nrow=2,ncol=1,align="v",labels="A",heights=c(1.5,1))
  save.plot(dpi=96,plt,Filename = paste0(v,"_",i,"_chronique_",basHy$name[c]),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)
}


######################################################
## Storylines meteo

storylines=data.frame(gcm=c("HadGEM2-ES","HadGEM2-ES","CNRM-CM5-LR","EC-EARTH"),rcm=c("ALADIN63","CCLM4-8-17","ALADIN63","HadREM3-GA7-05"),bc=rep("ADAMONT",4),type=c("HadGEM2-ES/ALADIN63","HadGEM2-ES/CCLM4-8-17","CNRM-CM5/ALADIN63","EC-EARTH/HadREM3-GA7"))#using longer model names than in hydro
pred_name="temps"
predict="time"
pred_unit=""
xlim=c(1990,2105)
horiz=2085
horiz3=c(2030,2050,2085)
freq_col=0.99

v="tasAdjust"
i="seasmean_DJF"
load(file=paste0(path_data_meteo,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
plt1=map_storyline(lst.QUALYPSOOUT = lst.QUALYPSOOUT,RCP = "rcp85",RCP_plainname="RCP8.5",horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v,storylines=storylines)
plt1=plt1+
  labs(title=NULL)

v="tasAdjust"
i="seasmean_JJA"
load(file=paste0(path_data_meteo,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
plt2=map_storyline(lst.QUALYPSOOUT = lst.QUALYPSOOUT,RCP = "rcp85",RCP_plainname="RCP8.5",horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v,storylines=storylines)
plt2=plt2+
  labs(title=NULL)

plt=ggarrange(plt1,plt2,heights=c(0.5,0.5),nrow=2,ncol=1,align="v",labels=c("A","B"))
save.plot(dpi=96,plt,Filename = paste0(v,"_2085_rcp85_storyline"),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)




v="prtotAdjust"
i="seassum_DJF"
load(file=paste0(path_data_meteo,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
plt1=map_storyline(lst.QUALYPSOOUT = lst.QUALYPSOOUT,RCP = "rcp85",RCP_plainname="RCP8.5",horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v,storylines=storylines)
plt1=plt1+
  labs(title=NULL)

v="prtotAdjust"
i="seassum_JJA"
load(file=paste0(path_data_meteo,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
plt2=map_storyline(lst.QUALYPSOOUT = lst.QUALYPSOOUT,RCP = "rcp85",RCP_plainname="RCP8.5",horiz = horiz,pred = predict,pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v,storylines=storylines)
plt2=plt2+
  labs(title=NULL)

plt=ggarrange(plt1,plt2,heights=c(0.5,0.5),nrow=2,ncol=1,align="v",labels=c("C","D"))
save.plot(dpi=96,plt,Filename = paste0(v,"_2085_rcp85_storyline"),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)


######################################################
## Map comparison basic method with QUALYPSO

v="prtotAdjust"
i="seassum_JJA"
pred_name="temps"
predict="time"
pred_unit=""
xlim=c(1990,2105)
hor=2085
freq_col=0.99

plt1=map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = hor,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out =NA,freq_col=freq_col,pix=T,var=v,nbcores=nbcores)
plt1=plt1+
  labs(title=NULL)+
  guides(fill=guide_colorbar(barwidth = 1.5, barheight = 7.5,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))+
  theme(legend.box = "horizontal")+
  theme(legend.key = element_rect(color="grey",size=0.1),legend.title = element_text(face = "bold",size = 12),legend.text = element_text(face = "bold",size = 8))
plt2=map_3quant_3rcp_1horiz_basic(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz=horiz,pred_name = pred_name,pred = predict,pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),folder_out = NA,freq_col=freq_col,pix=T,var=v,ref0=ref_year,LIM_COL = 75)
plt2=plt2+
  labs(title=NULL)+
  guides(fill=guide_colorbar(barwidth = 1.5, barheight = 7.5,label.theme = element_text(size = 8, face = "bold"),title.theme=element_text(size = 12, face = "bold")))+
  theme(legend.box = "horizontal")+
  theme(legend.key = element_rect(color="grey",size=0.1),legend.title = element_text(face = "bold",size = 12),legend.text = element_text(face = "bold",size = 8))

plt=ggarrange(plt1,plt2,heights=c(0.5,0.5),nrow=2,ncol=1,align="v",labels=c("A","B"))
save.plot(dpi=96,plt,Filename = paste0(v,"_",i,"_2085_comparison_method"),Folder =path_fig,Format = "pdf",Width = 21,Height = 29.7)


#############################################################################
## Illustration spline VS 30 years smoothing

basHy=read.csv(paste0(path_sig,"processed/SAFRAN_ref_basHy.csv"))
v="prtotAdjust"
i="seassum_DJF"
r="rcp26"
g="CNRM-CM5-LR"
rc="ALADIN63"
b="ADAMONT"
idx_basHy=1
path_data=path_data_meteo

scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i&simu_lst$rcp==r&simu_lst$gcm==g&simu_lst$rcm==rc&simu_lst$bc==b,]
c=1
pth_tmp=list.files(paste0(path_data,"indic/",scenAvail$var[c],"/bas/"),full.names=T,pattern=glob2rx(paste0(scenAvail$var[c],"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c][[1]],"_")[[1]][1],"*",scenAvail$period[c],"*")))
nc=load_nc(pth_tmp)
res=ncvar_get(nc,varid=scenAvail$var[c])
full_years=nc$dim$time$vals
full_years=year(as.Date(full_years,origin="1950-01-01"))
nc_close(nc)#for some reason stays opened otherwise
rm(nc)
gc()
res2=data.frame(matrix(nrow=dim(res)[length(dim(res))],ncol=2))
res2[,1]=full_years
j=1
res2[,j+1]=res[basHy$X[j],]
colnames(res2)[1]="year"
data=res2
rm(res)
rm(res2)
gc()

colnames(data)=c("year","val")
data$mean30=c(rep(NA,15),rollmean(data$val,k=31,align="center"),rep(NA,15))
data$spline=predict(stats::smooth.spline(data$year,data$val,spar=1.1),data$year)$y

plt=ggplot(data)+
  geom_line(aes(x=year,y=val,color="val",size="val"))+
  geom_line(aes(x=year,y=mean30,color="mean30",size="mean30"))+
  geom_line(aes(x=year,y=spline,color="spline",size="spline"))+
  scale_color_manual("",values=c("val"=ipcc_6col[5],"mean30"=ipcc_6col[3],"spline"=ipcc_6col[5]),labels=c("Projection","Moyenne glissante 30 ans","Modèle spline"))+
  scale_size_manual("",values=c("val"=0.5,"mean30"=2,"spline"=2),labels=c("Projection","Moyenne glissante 30 ans","Modèle spline"))+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  scale_x_continuous("")+
  scale_y_continuous("Pr_DJF (mm)",limits = c(min(data$val,na.rm=T),max(data$val,na.rm=T)),expand = c(0,0))+
  theme(legend.title = element_text(face="bold",size=18))
save.plot(dpi=300,plt,Filename = paste0("Illustration_lissage_",v,"_",i,"_",r,"_",g,"_",rc,"_",b,"_",basHy$name[idx_basHy]),Folder =path_fig,Format = "jpeg")
