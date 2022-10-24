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

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/Qualypso/"

Var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

load(file=paste0(path_data,"processed/lst_indic.Rdata"))
predict=c("time","temp_3rcp","temp_2rcp")

select_gcm=c("CNRM-CM5-LR","EC-EARTH","IPSL-CM5A-MR")

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions
Unit="%" #so far all changes are relative
first_data_year=1951
last_data_year=2099

######
#MAIN#
######

sim_stations=read.csv(file = paste0(path_data,"raw/SIM2/Infos_stations_modcou_OK.csv"),sep=";")
sim_stations=sim_stations[!sim_stations$Num_ordre_Modcou %in% c(626,669,763,764,861),]#take only stations with no NA

select_stations=read.xlsx(paste0(path_data,"raw/SIM2/selection_bassins_SIM2.xlsx"))# Sample of watersheds with a diversity of characteristic for plotting
idx=which(sim_stations$Num_ordre_Modcou%in%select_stations$Numero_Modcou)
select_stations=select_stations[order(select_stations$Numero_Modcou),]#because idx is reordered
select_stations$idx=idx

load(file = paste0(path_data,"processed/simu_lst.Rdata"))
first_ref_year=1975
last_ref_year=2005
first_full_year=1972# from raw data filenames
last_full_year=2098# from raw data filenames
vecYears=seq(first_full_year,last_full_year,1)

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
      xlim=c(1990,2100)
    }else{
      if(predict[p]=="temp_3rcp"){lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp_3rcp}
      if(predict[p]=="temp_2rcp"){lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp_2rcp}
      if(predict[p]=="temp_1rcp"){lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp_1rcp}
      pred_name="temperature"
      pred_unit="deg C"
      xlim=c(0.7,max(lst.QUALYPSOOUT[[1]]$Xfut))
    }
    for(b in 1:nrow(select_stations)){
      idx=select_stations$idx[b]
      # Warnings "removed rows" okay, due to xlim
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred=predict[p],pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred=predict[p],pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = T,xlim=xlim)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred=predict[p],pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred=predict[p],pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp8.5",xlim=xlim)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred=predict[p],pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred=predict[p],pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=folder_out,includeMean = F,includeRCP="rcp8.5",xlim=xlim)

      plotQUALYPSO_summary_change(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred=predict[p],pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=folder_out,xlim=xlim,simpler = T)
      
     }
  }
}



##############################################
## Change maps Linear method


for (i in 1:length(lst_indic)){
  folder_out=paste0(path_fig,"3GCM_all_basins/",lst_indic[i],"/maps/")
  dir.create(folder_out)
  
  load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_time_lm.RData"))
  lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
  pred_name="temps"
  pred="time"
  pred_unit=""
  horiz=2085
  horiz3=c(2030,2050,2085)
  freq_col=0.99
  
  map_3quant_3rcp_1horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz3,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],rcp_name = "rcp8.5",rcp_plainname="RCP 8.5",folder_out = folder_out,freq_col=freq_col)

  map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp8.5",horiz = horiz,name_eff = "rcm",name_eff_plain = "RCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=F,includeRCP = "rcp8.5",horiz = horiz,name_eff = "gcm",name_eff_plain = "GCM",pred = pred,pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)

  map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="mean",horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="vartot",horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="incert",horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="rcp8.5",horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)
  map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varres",horiz = horiz,pred_name = pred_name,pred = pred,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = folder_out,freq_col=freq_col)

 }



############################################
## Plot GCM temperature

tmp=format_global_tas(path_data,first_data_year,last_data_year,simu_lst,first_ref_year,last_ref_year)
mat_Globaltas_gcm=tmp[[3]]
mat_Globaltas_gcm=data.frame(apply(mat_Globaltas_gcm,MARGIN = 2,function(x) smooth.spline(x=seq(first_data_year,last_data_year),y=x,spar = 1)$y))
#mat_Globaltas_gcm$year=seq(first_data_year,last_data_year)

data=pivot_longer(data=mat_Globaltas_gcm,cols=!year,names_to = "chain",values_to = "val")
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))
data$gcm=unlist(lapply(strsplit(data$chain,"_"),function(x) x[2]))

plt=ggplot(data)+
  geom_line(aes(x=year,y=val,col=rcp,lty=gcm),size=1.2)+
  xlab("")+
  ylab("Changement de température planétaire (°C)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = as.vector(col_3rcp),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  scale_linetype_discrete("GCM")+
  ggtitle("Changement de température planétaire pour les différents RCP/GCM\npar rapport a la référence 1860-1900")
save.plot(plt,Filename = "global_tas",Folder = path_fig,Format = "jpeg")

## Same but start earlier

paths=list.files(paste0(path_data,"raw/Global_temp/"),pattern=glob2rx("global_tas*"),full.names = T)
tas_glob_full=vector(length=length(paths),mode="list")
for ( i in 1:length(paths)){
  tas_glob=read.csv(paths[i],skip=3,sep="",header=F)
  if(grepl("HadGEM2-ES",paths[i],fixed=T)){# no values in 1859
    tas_glob=tas_glob[-1,]
  }
  tas_glob=data.frame(year=tas_glob[,1],tas=apply(tas_glob[,-1],MARGIN = 1,mean))# mean of 12 months
  pre_indus_tas=mean(tas_glob$tas[tas_glob$year>=1860 & tas_glob$year<=1900])
  tas_glob$tas=tas_glob$tas-pre_indus_tas
  colnames(tas_glob)[2]=paste0(strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][5],"_",strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][4])#rcp_gcm name
  tas_glob_full[[i]]=tas_glob[tas_glob$year<=2100,]
}
mat_Globaltas=lapply(tas_glob_full,function(x) cbind(x[,1],smooth.spline(x=x[,1],y=x[,2],spar = 1)$y))
mat_Globaltas=lapply(mat_Globaltas,function(x) x[x[,1]>=1861&x[,1]<=2100,2])# here 1861 because we wanna show GFDL
mat_Globaltas=t(do.call(cbind,mat_Globaltas))
years=seq(1861,2100)
stock=mat_Globaltas

data=pivot_longer(data=mat_Globaltas_gcm,cols=!year,names_to = "chain",values_to = "val")
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))
data$gcm=unlist(lapply(strsplit(data$chain,"_"),function(x) x[2]))

plt=ggplot(data)+
  geom_line(aes(x=year,y=val,col=rcp,lty=gcm),size=1.2)+
  xlab("")+
  ylab("Changement de température planétaire (deg C)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = as.vector( col_3rcp),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  scale_linetype_discrete("GCM")+
  ggtitle("Changement de température planetaire pour les différents RCP/GCM\npar rapport a la référence 1860-1900")
save.plot(plt,Filename = "global_tas_1860",Folder = path_fig,Format = "jpeg")


###################################################
## Plot crossing date

idx=vector(mode = "list")
temp_ref=c(1,1.5,2,3,4,5)
for(temp in temp_ref){
  #stock is variable from previous chunk
  idx[[as.character(temp)]]=apply(stock,MARGIN=1,function(x) min(which(x>=temp)))
}

idx=data.frame(do.call(rbind, idx))
idx[idx==Inf]=NA
idx=data.frame(apply(idx,MARGIN=2,function(x) years[x]))
idx$temp=temp_ref
for(i in 1:length(paths)){
  colnames(idx)[i]=paste0(strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][5],"_",strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][4])
}
data=pivot_longer(data=idx,cols=!temp,names_to = "chain",values_to = "val")
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))


plt=ggplot(data)+
  geom_boxplot(aes(x=val,y=factor(temp),fill=rcp),alpha=0.7)+
  geom_point(aes(x=val,y=factor(temp),fill=rcp), position = position_jitterdodge(jitter.width = 0.2),size=2,alpha=0.5)+
  xlab("")+
  ylab("Changement de température planétaire (deg C)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_x_continuous(breaks=seq(2000,2100,20))+
  scale_fill_discrete("",type = as.vector( col_3rcp),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  guides(fill=guide_legend(reverse=TRUE))+
  ggtitle("Première année de franchissement des seuils\nde changement de température planétaire\npour les différents RCP (et GCMs) (référence 1860-1900)")
save.plot(plt,Filename = "dat_threshold_temp",Folder = path_fig,Format = "jpeg")

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
mat_Globaltas=t(mat_Globaltas[seq(first_data_year,last_data_year) %in% seq(first_full_year,last_full_year),])
mat_Globaltas_spline=mat_Globaltas_spline[,seq(first_data_year,last_data_year) %in% seq(first_full_year,last_full_year)]

load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/Q_mean_year_list_QUALYPSOOUT_3GCM_time_lm.RData"))
for (i in 1:nrow(select_stations)){
  all_chains=vector(length=nrow(simu_lst),mode="list")
  for (j in 1:nrow(simu_lst)){
    load(paste0(path_data,"processed/indic_hydro/Q_mean_year_",simu_lst$rcp[j],"_",simu_lst$gcm[j],"_",simu_lst$rcm[j],"_",simu_lst$bc[j],"_",simu_lst$hm[j],".Rdata"))
    all_chains[[j]]=res
  }
  ClimateProjections=lapply(all_chains, function(x) x[,c(1,select_stations$idx[i])])
  ClimateProjections_spline=lapply(ClimateProjections, function(x) cbind(x[,1],smooth.spline(x=x[,1],y=x[,2],spar = 1.1)$y))
  ClimateProjections=lapply(ClimateProjections,function(x) x[x$year>=first_full_year & x$year<=last_full_year,][,2])
  ClimateProjections_spline=lapply(ClimateProjections_spline,function(x) x[x[,1]>=first_full_year & x[,1]<=last_full_year,][,2])
  ClimateProjections=t(do.call(rbind,ClimateProjections))
  ClimateProjections_spline=t(do.call(rbind,ClimateProjections_spline))
  data=data.frame(tas=as.vector(mat_Globaltas),tas_spline=as.vector(mat_Globaltas_spline),q=as.vector(ClimateProjections),q_spline=as.vector(ClimateProjections_spline))
  
  plt=ggplot(data)+
    geom_point(aes(x=tas,y=q,col="raw"),size=0.7,alpha=0.6)+
    geom_point(aes(x=tas_spline,y=q_spline,col="spline"),size=0.7,alpha=0.6)+
    scale_color_manual("",values = c("raw"=ipcc_6col[1],"spline"=ipcc_6col[2]),labels=c("Brut","Spline"))+
    xlab("Changement de température planétaire (deg C)")+
    ylab("Module annuel")+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    guides(colour = guide_legend(override.aes = list(size=3)))+
    ggtitle(paste0("Module annuel du bassin ",select_stations$Nom[i],"\nen fonction du changement de temperature planetaire (",first_full_year,"/",last_full_year,")"))
  save.plot(plt,Filename = paste0("global_tasVSmodule_",select_stations$Nom[i]),Folder = path_fig,Format = "jpeg")
}

##################################################################################
## Plot map of reference (1990) value of indicator for continuous positive indicator (of discharge)

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

#####################
## Area of basins

folder_out=paste0(path_fig,"3GCM_all_basins/")
plot_bv_areas(folder_out)
