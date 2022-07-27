# Alix Reverdy
# Explore 2
# Run Qualypso with a fictive local temperature

rm(list=ls())
gc()

#########
#LIBRARY#
#########

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")
#View(QUALYPSO.ANOVA)
#lsf.str("package:QUALYPSO")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/fictiveT/"
nbcore=detectCores()-2 #Number of cores for parallelization

typeChangeVar="rel"
ref_year=1990# central year of 1975-2005 reference period
first_ref_year=1975
last_ref_year=2005



######
#MAIN#
######


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
mat_Globaltas_local=lapply(tas_glob_full,function(x) x[x[,1]>=1861&x[,1]<=2100,2])# here 1861 because we wanna show GFDL
mat_Globaltas_gcm=data.frame(do.call(cbind,mat_Globaltas))
mat_Globaltas_local=data.frame(do.call(cbind,mat_Globaltas_local))
for (i in 1:ncol(mat_Globaltas_gcm)){
  colnames(mat_Globaltas_gcm)[i]=colnames(tas_glob_full[[i]])[2]
}
colnames(mat_Globaltas_local)=colnames(mat_Globaltas_gcm)

years=seq(1861,2100)
ref_Globaltas_gcm=as.vector(apply(mat_Globaltas_gcm,MARGIN = 2,function(x) mean(x[which(years==first_ref_year):which(years==last_ref_year)])))

scenAvail=data.frame(colnames(mat_Globaltas_gcm))
colnames(scenAvail)="chain"
scenAvail$rcp=unlist(lapply(strsplit(scenAvail$chain,"_"),function(x) x[1]))
scenAvail$gcm=unlist(lapply(strsplit(scenAvail$chain,"_"),function(x) x[2]))
scenAvail=scenAvail[,c("rcp","gcm")]

mat_Globaltas_local= mat_Globaltas_local * 1.3 # local French warming 30% greater than global, no smoothing
# https://www.ecologie.gouv.fr/sites/default/files/ONERC_rapport_Climate%20change_Costs%20of%20impacts%20and%20lines%20of%20adaptation_ENG.pdf

################################
## Linear method

Y=t(mat_Globaltas_local)

##Time predictor
listOption = list(spar=1,typeChangeVariable=typeChangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))

QUALYPSOOUT_time = QUALYPSO(Y=Y, #one Y and run per basin because otherwise we cannot have several future times
                                       scenAvail=scenAvail,
                                       X = years,
                                       Xref = ref_year,
                                       listOption=listOption)# no Xfut or iFut because we want all values
##Temperature predictor
listOption = list(spar=2,typeChangeVariable=typeChangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
# above 2 spline is doing weird stuff
#Warnings spar okay
QUALYPSOOUT_temp_3rcp = QUALYPSO(Y=Y, #one Y and run per basin because otherwise we cannot have several future times
                                            scenAvail=scenAvail,
                                            X = t(mat_Globaltas_gcm),
                                            #Xref = ref_Globaltas_gcm,
                                            Xref=0.7,
                                            Xfut=seq(0.5,1.5,0.01),
                                            listOption=listOption)# no iFut because we want all values

idx_2rcp=which(scenAvail$rcp=="rcp45"|scenAvail$rcp=="rcp85")
QUALYPSOOUT_temp_2rcp = QUALYPSO(Y=Y[idx_2rcp,], #one Y and run per basin because otherwise we cannot have several future times
                                            scenAvail=scenAvail[idx_2rcp,],
                                            X = t(mat_Globaltas_gcm[,idx_2rcp]),
                                            #Xref = ref_Globaltas_gcm[idx_2rcp],
                                            Xref=0.7,
                                            Xfut=seq(0.5,2.5,0.01),
                                            listOption=listOption)# no iFut because we want all values

save(QUALYPSOOUT_time,file=paste0(path_data,"processed/qualypso/QUALYPSOOUT_fictive_localT_time_lm.RData"))
save(QUALYPSOOUT_temp_3rcp,file=paste0(path_data,"processed/qualypso/QUALYPSOOUT_fictive_localT_temp_3rcp_lm.RData"))
save(QUALYPSOOUT_temp_2rcp,file=paste0(path_data,"processed/qualypso/QUALYPSOOUT_fictive_localT_temp_2rcp_lm.RData"))

###############################################
## Figures QUALYPSO

plotQUALYPSO_summary_change(QUALYPSOOUT = QUALYPSOOUT_time,pred="time",pred_name = "temps",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "",folder_out=path_fig)
plotQUALYPSO_summary_change(QUALYPSOOUT = QUALYPSOOUT_temp_3rcp,pred="temp_3rcp",pred_name = "Température",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "deg C",folder_out=path_fig)
plotQUALYPSO_summary_change(QUALYPSOOUT = QUALYPSOOUT_temp_2rcp,pred="temp_2rcp",pred_name = "Température",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "deg C",folder_out=path_fig)


# plotQUALYPSOMeanChangeAndUncertainties_noIV_ggplot(QUALYPSOOUT = QUALYPSOOUT_time,pred="time",pred_name = "temps",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "",folder_out=path_fig,xlim=c(1990,2100),iv_type = "tot")
# plotQUALYPSOMeanChangeAndUncertainties_noIV_ggplot(QUALYPSOOUT = QUALYPSOOUT_temp_3rcp,pred="temp_3rcp",pred_name = "Température",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "deg C",folder_out=path_fig,xlim=c(0.7,max(QUALYPSOOUT_temp_3rcp$Xfut)),iv_type = "tot")
# plotQUALYPSOMeanChangeAndUncertainties_noIV_ggplot(QUALYPSOOUT = QUALYPSOOUT_temp_2rcp,pred="temp_2rcp",pred_name = "Température",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "deg C",folder_out=path_fig,xlim=c(0.7,max(QUALYPSOOUT_temp_2rcp$Xfut)),iv_type = "tot")
# 
# plotQUALYPSOTotalVarianceDecomposition_ggplot(QUALYPSOOUT = QUALYPSOOUT_time,pred="time",pred_name = "temps",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "",folder_out=path_fig,xlim=c(1990,2100))
# plotQUALYPSOTotalVarianceDecomposition_ggplot(QUALYPSOOUT = QUALYPSOOUT_temp_3rcp,pred="temp_3rcp",pred_name = "Température",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "deg C",folder_out=path_fig,xlim=c(0.7,max(QUALYPSOOUT_temp_3rcp$Xfut)))
# plotQUALYPSOTotalVarianceDecomposition_ggplot(QUALYPSOOUT = QUALYPSOOUT_temp_2rcp,pred="temp_2rcp",pred_name = "Température",ind_name = "fictive T",ind_name_full="Température local fictive",bv_name = "",bv_full_name = "",pred_unit = "deg C",folder_out=path_fig,xlim=c(0.7,max(QUALYPSOOUT_temp_2rcp$Xfut)))

###################################################
## Time and temperature series of local fictive Temperature

## Time

SPAR=1
clim_resp=vector(length=nrow(scenAvail),mode="list")
clim_resp_spline=vector(length=nrow(scenAvail),mode="list")
for(c in 1:nrow(scenAvail)){# for each chain
  res=data.frame(years,mat_Globaltas_local[,c])
  colnames(res)=c("year","val")
  res_spline=res
  res_spline$val=smooth.spline(x=years,y=res$val,spar = SPAR)$y
  clim_resp[[c]]=res
  clim_resp_spline[[c]]=res_spline
}
for (r in unique(scenAvail$rcp)){
  chain_r=which(scenAvail$rcp==r)#chains with the right rcp
  raw=clim_resp[[chain_r[1]]]#first iteration outside loop
  spline=clim_resp_spline[[chain_r[1]]]
  for (R in 2:length(chain_r)){
    raw=merge(raw,clim_resp[[chain_r[R]]],by="year",all=T)
    spline=merge(spline,clim_resp_spline[[chain_r[R]]],by="year",all=T)
    ## Warnings okay
  }
  colnames(raw)[-1]=paste0(scenAvail[scenAvail$rcp==r,]$gcm)
  colnames(spline)[-1]=paste0(scenAvail[scenAvail$rcp==r,]$gcm)
  raw=pivot_longer(data=raw,cols=!year,names_to = "gcm",values_to = "val")
  raw$type="raw"
  spline=pivot_longer(data=spline,cols=!year,names_to = "gcm",values_to = "val")
  spline$type="spline"
  data=rbind(raw,spline)
  plt=ggplot(data)+#Warnings okay
    geom_line(aes(x=year,y=val,size=type),color="cornflowerblue")+
    scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse climatique"))+
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    ggtitle(paste0("Chronique temporelle de la T_fictive_locale\npour le ",r))+
    scale_x_continuous("")+
    scale_y_continuous(paste0("Réponse climatique (deg C)"))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))
  save.plot(plt,Filename = paste0("chronique_temps_temperature_fictive_",r),Folder = path_fig,Format = "jpeg")
}


## Global temperature

#Merge data frame warnings are okay
SPAR=2 # above 2 spline is doing weird stuff
clim_resp=vector(length=nrow(scenAvail),mode="list")
clim_resp_spline=vector(length=nrow(scenAvail),mode="list")
for(c in 1:nrow(scenAvail)){# for each chain
  res=data.frame(years,mat_Globaltas_local[,c])
  colnames(res)=c("year","val")
  res_spline=res
  tas=mat_Globaltas_gcm[,c]
  smooth.spline.out=smooth.spline(x=tas,y=res$val,spar = SPAR)
  res_spline$val=predict(smooth.spline.out, tas)$y#need extra step otherwise loose one value for some reason
  res$tas=tas
  res_spline$tas=tas
  res=res[,-1]
  res_spline=res_spline[,-1]
  clim_resp[[c]]=res
  clim_resp_spline[[c]]=res_spline
}
for (r in unique(scenAvail$rcp)){
  chain_r=which(scenAvail$rcp==r)#chains with the right rcp
  raw=clim_resp[[chain_r[1]]]#first iteration outside loop
  spline=clim_resp_spline[[chain_r[1]]]
  for (R in 2:length(chain_r)){
    raw=merge(raw,clim_resp[[chain_r[R]]],by="tas",all=T)
    spline=merge(spline,clim_resp_spline[[chain_r[R]]],by="tas",all=T)
    ## Warnings okay
  }
  colnames(raw)[-1]=paste0(scenAvail[scenAvail$rcp==r,]$gcm)
  colnames(spline)[-1]=paste0(scenAvail[scenAvail$rcp==r,]$gcm)
  raw=pivot_longer(data=raw,cols=!tas,names_to = "gcm",values_to = "val")
  raw$type="raw"
  spline=pivot_longer(data=spline,cols=!tas,names_to = "gcm",values_to = "val")
  spline$type="spline"
  data=rbind(raw,spline)
  data=data[!is.na(data$val),]
  plt=ggplot(data)+#Warnings okay
    geom_line(aes(x=tas,y=val,size=type),color="cornflowerblue")+
    scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse climatique"))+
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    ggtitle(paste0("Chronique en température de la T_fictive_locale\npour le ",r))+
    scale_x_continuous("Température (°C)")+
    scale_y_continuous(paste0("Réponse climatique (deg C)"))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))+
    geom_abline(slope=1.3,intercept=0)
  save.plot(plt,Filename = paste0("chronique_temperature_temperature_fictive_",r),Folder = path_fig,Format = "jpeg") 
}

