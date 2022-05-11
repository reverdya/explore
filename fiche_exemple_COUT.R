# Alix Reverdy
# Explore 2
# Make figures from Qualypso runs

rm(list=ls())
gc()
dev.off()

#########
#LIBRARY#
#########

library(knitr)
library(markdown)
library(rmarkdown)
library(tinytex)
library(cowplot)##grid plot of png in knitr

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R')

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/"
path_Rmd="C:/Users/reverdya/Documents/Docs/1_code/explore/fiche_exemple_COUT.Rmd"

load(file=paste0(path_data,"processed/lst_indic.Rdata"))
units=c("m3/s","m3/s","log(m3/s)","day of year")
load(file=paste0(path_data,"processed/simu_lst.Rdata"))

centr_ref_year=1990# central year of 1975-2005 reference period
typeChangeVar="rel"
first_ref_year=1975
last_ref_year=2005
first_full_year=1972# from raw data filenames
last_full_year=2098# from raw data filenames

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions

######
#MAIN#
######

sim_stations=read.csv(file = paste0(path_data,"raw/SIM2/Infos_stations_modcou_OK.csv"),sep=";")
sim_stations=sim_stations[!sim_stations$Num_ordre_Modcou %in% c(626,669,763,764,861),]#take only stations with no NA

select_stations=read.xlsx(paste0(path_data,"raw/SIM2/selection_bassins_SIM2.xlsx"))# Sample of watersheds with a diversity of characteristic for plotting
idx=which(sim_stations$Num_ordre_Modcou%in%select_stations$Numero_Modcou)
select_stations=select_stations[order(select_stations$Numero_Modcou),]#because idx is reordered
select_stations$idx=idx
select_stations=select_stations[select_stations$idx==725,]

names_eff=colnames(simu_lst)[2:4]
lst_names_eff=vector(mode="list",length=length(names_eff))#list of effects and their possibilities
for(i in 1:length(lst_names_eff)){
  names(lst_names_eff)[i]=names_eff[i]
  lst_names_eff[[i]]=unique(simu_lst[,names_eff[i]])
}

##########################################################################
## Raw and spline times series


SPAR=1.1
i=1 # Qmean_year
clim_resp=vector(length=nrow(simu_lst),mode="list")
clim_resp_spline=vector(length=nrow(simu_lst),mode="list")

##Merge data frame warnings are okay
for(c in 1:nrow(simu_lst)){# for each chain
  
  load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
  res=res[,c(1,select_stations$idx+1)]
  
  res=res[res$year>=first_full_year&res$year<=last_full_year,]
  zz = !is.na(res[,2])
  vecYears=res[zz,1]
  #spline
  res_spline=res
  for(j in 2:ncol(res)){
    res_spline[zz,j]=smooth.spline(x=vecYears,y=res[zz,j],spar = SPAR)$y
  }
  colnames(res)[1]="year"
  colnames(res_spline)[1]="year"
  clim_resp[[c]]=res
  clim_resp_spline[[c]]=res_spline
}

w=2
plt_raw_time=vector(length=length(lst_names_eff$rcp),mode="list")
plt_spline_time=vector(length=length(lst_names_eff$rcp),mode="list")
for (r in lst_names_eff$rcp){
  
  chain_r=which(simu_lst$rcp==r)#chains with the right rcp
  raw=clim_resp[[chain_r[1]]][,c(1,w)]#first iteration outside loop
  spline=clim_resp_spline[[chain_r[1]]][,c(1,w)]
  for (R in 2:length(chain_r)){
    raw=merge(raw,clim_resp[[chain_r[R]]][,c(1,w)],by="year",all=T)
    spline=merge(spline,clim_resp_spline[[chain_r[R]]][,c(1,w)],by="year",all=T)
    ## Warnings okay
  }
  colnames(raw)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
  colnames(spline)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
  raw=gather(raw,key = "model",value = "val",-year)
  raw$type="raw"
  spline=gather(spline,key = "model",value = "val",-year)
  spline$type="spline"
  data=rbind(raw,spline)
  data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
  data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
  
  plt_raw_time[[r]]=ggplot(data[data$type=="raw",])+#Warnings okay
    geom_line(aes(x=year,y=val,color=rcm),size=0.7)+
    scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    ggtitle(paste0("Time series of absolute change of ",lst_indic[i],"\nfor ",r," at ",select_stations$Nom[w-1]))+
    scale_x_continuous("")+
    scale_y_continuous(paste0("Climate response ( ",units[i]," )"))+
    guides(color = guide_legend(override.aes = list(size = 1.7)))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))
  plt_spline_time[[r]]=ggplot(data)+#Warnings okay
    geom_line(aes(x=year,y=val,size=type,color=rcm))+
    scale_size_manual("",values=c(0.7,1.7),label=c("Climate response","Spline fit"))+
    scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    ggtitle(paste0("Time series of absolute change of ",lst_indic[i],"\nfor ",r," at ",select_stations$Nom[w-1]))+
    scale_x_continuous("")+
    scale_y_continuous(paste0("Climate response ( ",units[i]," )"))+
    guides(color = guide_legend(override.aes = list(size = 1.7)))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))
}

    


###############################################################################################
## Plot raw indicator, and its spline for all models and selection of watersheds by RCP for temperature
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
## Climate response not climate change response

tmp=format_global_tas(path_data,first_full_year,last_full_year,simu_lst,first_ref_year,last_ref_year)
mat_Globaltas=tmp[[1]]

SPAR=1.1
i=1 # Qmean_year
clim_resp=vector(length=nrow(simu_lst),mode="list")
clim_resp_spline=vector(length=nrow(simu_lst),mode="list")
for(c in 1:nrow(simu_lst)){# for each chain
  
  load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
  res=res[,c(1,select_stations$idx+1)]
  res=res[res$year>=first_full_year&res$year<=last_full_year,]
  zz = !is.na(res[,2])
  
  #spline
  res_spline=res
  tas=mat_Globaltas[c,]
  for(j in 2:ncol(res)){
    res_spline[zz,j]=smooth.spline(x=tas,y=res[zz,j],spar = SPAR)$y
  }
  res$tas=tas
  res_spline$tas=tas
  res=res[,-1]
  res_spline=res_spline[,-1]
  clim_resp[[c]]=res
  clim_resp_spline[[c]]=res_spline
}

w=1
plt_raw_temp=vector(length=length(lst_names_eff$rcp),mode="list")
plt_spline_temp=vector(length=length(lst_names_eff$rcp),mode="list")
for (r in lst_names_eff$rcp){
  
  chain_r=which(simu_lst$rcp==r)#chains with the right rcp
  raw=clim_resp[[chain_r[1]]][,c(w,ncol(clim_resp[[1]]))]#first iteration outside loop
  spline=clim_resp_spline[[chain_r[1]]][,c(w,ncol(clim_resp[[1]]))]
  for (R in 2:length(chain_r)){
    raw=merge(raw,clim_resp[[chain_r[R]]][,c(w,ncol(clim_resp[[1]]))],by="tas",all=T)
    spline=merge(spline,clim_resp_spline[[chain_r[R]]][,c(w,ncol(clim_resp[[1]]))],by="tas",all=T)
    ## Warnings okay
  }
  colnames(raw)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
  colnames(spline)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
  raw=gather(raw,key = "model",value = "val",-tas)
  raw$type="raw"
  spline=gather(spline,key = "model",value = "val",-tas)
  spline$type="spline"
  data=rbind(raw,spline)
  data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
  data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
  data=data[!is.na(data$val),]
  
  plt_raw_temp[[r]]=ggplot(data[data$type=="raw",])+#Warnings okay
    geom_line(aes(x=tas,y=val,color=rcm),size=0.7)+
    scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    ggtitle(paste0("Time series of absolute change of ",lst_indic[i],"\nfor ",r," at ",select_stations$Nom[w]))+
    scale_x_continuous("Temperature (degC)")+
    scale_y_continuous(paste0("Climate response ( ",units[i]," )"))+
    guides(color = guide_legend(override.aes = list(size = 1.7)))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))
  plt_spline_temp[[r]]=ggplot(data)+#Warnings okay
    geom_line(aes(x=tas,y=val,size=type,color=rcm))+
    scale_size_manual("",values=c(0.7,1.7),label=c("Climate response","Spline fit"))+
    scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    ggtitle(paste0("Time series of absolute change of ",lst_indic[i],"\nfor ",r," at ",select_stations$Nom[w]))+
    scale_x_continuous("Temperature (degC)")+
    scale_y_continuous(paste0("Climate response ( ",units[i]," )"))+
    guides(color = guide_legend(override.aes = list(size = 1.7)))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))

}

    
####################################################
## Planetary temperature

paths=list.files(paste0(path_data,"raw/Global_temp/"),pattern=glob2rx("global_tas*"),full.names = T)
for ( i in 1:length(paths)){
  tas_glob=read.csv(paths[i],skip=3,sep="",header=F)
  tas_glob=data.frame(year=tas_glob[,1],tas=apply(tas_glob[,-1],MARGIN = 1,mean))# mean of 12 months
  pre_indus_tas=mean(tas_glob$tas[tas_glob$year>=1861 & tas_glob$year<=1900])
  tas_glob$tas=tas_glob$tas-pre_indus_tas
  colnames(tas_glob)[2]=paste0(strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][5],"_",strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][4])#rcp_gcm name
  if(i==1){
    mat_Globaltas_gcm=tas_glob
  }else{
    mat_Globaltas_gcm=merge(mat_Globaltas_gcm,tas_glob,by="year")
  }
}
years=mat_Globaltas_gcm$year
mat_Globaltas_gcm=data.frame(apply(mat_Globaltas_gcm,MARGIN = 2,function(x) smooth.spline(x=years,y=x,spar = 1)$y))
mat_Globaltas_gcm$year=years

data=gather(mat_Globaltas_gcm,key = "chain",value = "val",-year)
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))
data$gcm=unlist(lapply(strsplit(data$chain,"_"),function(x) x[2]))

plt_planet_temp=ggplot(data)+
  geom_line(aes(x=year,y=val,col=rcp,lty=gcm),size=1.2)+
  xlab("")+
  ylab("Changement de température planétaire (deg C)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = as.vector( col_3rcp),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  scale_linetype_discrete("GCM")+
  ggtitle("Changement de temperature planetaire pour les differents RCP/GCM\npar rapport a la reference 1861-1900")

##################################################################################
## Mean change and variance partition per RCP


i=1
load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_time_lm.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
pred_name="temps"
pred_unit=""
xlim=c(1990,2100)
b=1
idx=select_stations$idx[b]



# Warnings removed rows okay, due to xlim
plt_var_rcp2.6=plotQUALYPSOTotalVarianceByScenario_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff = "rcp",nameScenario = "rcp2.6",plain_name_Scen = "RCP 2.6",pred="time",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
plt_var_rcp4.5=plotQUALYPSOTotalVarianceByScenario_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff = "rcp",nameScenario = "rcp4.5",plain_name_Scen = "RCP 4.5",pred="time",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
plt_var_rcp8.5=plotQUALYPSOTotalVarianceByScenario_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff = "rcp",nameScenario = "rcp8.5",plain_name_Scen = "RCP 8.5",pred="time",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)

#####################################
## Effet GCM, effet RCM

plt_gcm_effect=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "RCP",pred="time",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
plt_rcm_effect=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCP",pred="time",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)


####################################
## Effet BC, effet HM

data_bc=data.frame(year=seq(1990,2100))
data_bc$bc1=(data_bc$year-1990)^1.5*0.01
data_bc$bc2=-data_bc$bc1
data_bc=gather(data_bc,key = "bc",value = "val",-year)

plt_bc_effect=ggplot(data_bc)+
  geom_line(aes(x=year,y=val,group=bc,color=bc),size=1)+
  scale_x_continuous("",limits=xlim)+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("Moyenne lissee",type = coolwarm(2),labels=c("Correction A","Correction B"))+
  scale_y_continuous(paste0("Effet principal (%)"))+
  ggtitle(paste0("Effet principaux des corrections de biais\npour le module annuel du débit (",select_stations$Nom[b],")"))


data_hm=data.frame(year=seq(1990,2100))
data_hm$hm1=(data_hm$year-1990)^0.5
data_hm$hm2=-data_hm$hm1
data_hm=gather(data_hm,key = "hm",value = "val",-year)

plt_hm_effect=ggplot(data_hm)+
  geom_line(aes(x=year,y=val,group=hm,color=hm),size=1)+
  scale_x_continuous("",limits=xlim)+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("Moyenne lissee",type = coolwarm(2),labels=c("Modele A","Modele B"))+
  scale_y_continuous(paste0("Effet principal (%)"))+
  ggtitle(paste0("Effet principaux des modèles hydrologiques\npour le module annuel du débit (",select_stations$Nom[b],")"))


########################################
## Variance partition

plt_var_decomp=plotQUALYPSOTotalVarianceDecomposition_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred="time",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)

############################################
## Change by RCP + CI in time

plt_change_rcp_time=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred="time",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,includeMean = T,xlim=xlim,incert=T)

############################################
## Change by RCP + CI in temperature

load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_temp_3rcp_lm.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp_3rcp
pred_name="temperature"
pred_unit="deg C"
xlim=c(0.5,max(lst.QUALYPSOOUT[[1]]$Xfut))

plt_change_3rcp_temp=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred="temp_3rcp",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,includeMean = T,xlim=xlim,incert=T)

load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_temp_2rcp_lm.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp_2rcp
xlim=c(0.5,max(lst.QUALYPSOOUT[[1]]$Xfut))

plt_change_2rcp_temp=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred="temp_2rcp",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],pred_unit = pred_unit,folder_out=NA,includeMean = T,xlim=xlim,incert=T)


##########
#MARKDOWN#
##########

rmarkdown::render(path_Rmd,output_format = "word_document",output_file = "fiche_exemple_COUT_v0.docx",output_dir =paste0(path_fig,"COUT/"),quiet = TRUE)
