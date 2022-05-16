# Alix Reverdy
# Explore 2
# general data science function and data representations



library(svglite) #save svg
library(ggplot2) #plots
library(ncdf4) #netcdf
library(tictoc) #runtime
library(lubridate) #date management
library(QUALYPSO) #Qualypso
library(parallel) #detectCores
library(zoo) #rollmean
library(pals)#colors
library(maptools)#wrld_simpl
library(openxlsx) #handle xlsx
library(rgdal) #readogr ...
library(dplyr) #left_join
library(raster)# CRS management
library(scales)#squish
library(ggformula)#geom_spline
library(tidyr)#pivot_longer
library(parallel) #detectCores
library(plotly)#interactive plot
library(htmlwidgets)#save interactive plot


#####
#Save a graph in svg, pdf or jpeg format (size in cm)
#object ggplot : object in "plot.object"
#function plot base (ou multiplot): plot.object=NULL , Type="plot" , plot.function=plot(x,y)... , Format = "pdf" (ou "svg" ou "jpeg")
#If jpeg and "plot" size in pixels
save.plot=function(plot.object,Filename,Folder,Type="ggplot",plot.function=NULL,Format=NULL,Width=30,Height=20){
  if(Type=="ggplot"){
    if(Format=="svg"){
      fnsave=paste(Folder,Filename,".svg",sep="")
      ggsave(filename = fnsave,plot=plot.object,device = "svg")
    }
    if(Format=="pdf"){
      fnsave=paste(Folder,Filename,".pdf",sep="")
      ggsave(filename = fnsave,plot=plot.object,device = "pdf",units="cm",height=Height,width=Width)
    }
    if(Format=="jpeg"){
      fnsave=paste(Folder,Filename,".jpg",sep="")
      ggsave(filename = fnsave,plot=plot.object,device = "jpeg",units="cm",height=Height,width=Width)
    }
  }
  if(Type=="plot"){
    if(Format=="pdf"){
      graphics.off()
      fnsave=paste(Folder,Filename,".pdf",sep="")
      cairo_pdf(fnsave,height=Height/2.54,width=Width/2.54)#conversion cm to inches
      plot.function
      graphics.off()
    }
    if (Format=="svg"){
      graphics.off()
      fnsave=paste(Folder,Filename,".svg",sep="")
      svg(fnsave,height=Height/2.54,width=Width/2.54)#conversion cm to inches
      plot.function
      graphics.off()
    }
    if (Format=="jpeg"){
      graphics.off()
      fnsave=paste(Folder,Filename,".jpg",sep="")
      jpeg(fnsave,height=Height,width=Width,units="cm", res=72)
      plot.function
      graphics.off()
    }
  }
}

#####
#Cleanly load netcdf
load_nc<-function(path){
  nc<-nc_open(path)
  return(nc)
  nc_close(nc)
}

#####
## From a date returns season DJF,MAM,JJA,SON
date_to_season=function(date){
  month=month(date)
  season=month
  season[season==12]="DJF"
  season[season==1]="DJF"
  season[season==2]="DJF"
  season[season==3]="MAM"
  season[season==4]="MAM"
  season[season==5]="MAM"
  season[season==6]="JJA"
  season[season==7]="JJA"
  season[season==8]="JJA"
  season[season==9]="SON"
  season[season==10]="SON"
  season[season==11]="SON"
  return(season)
}


#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://coolbutuseless.github.io/2020/08/26/run-length-encoding-and-the-problem-of-nas/
# extrait 18/02/2022
#
#' A drop-in replacement for \code{base::rle()} that treats all NAs as identical
#'
#' @param x an atomic vector
#'
#' @return An object of class \code{rle}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rle2 <- function (x)  {
  stopifnot("'x' must be a vector of an atomic type" = is.atomic(x))
  
  n <- length(x)
  if (n == 0L) {
    return(structure(list(
      lengths = integer(), values = x)
    ), class = 'rle')
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Where does next value not equal current value?
  # i.e. y will be TRUE at the last index before a change
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  y <- (x[-1L] != x[-n])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Since NAs are never equal to anything, NAs in 'x' will lead to NAs in 'y'.
  # These current NAs in 'y' tell use nothing - Set all NAs in y to FALSE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  y[is.na(y)] <- FALSE
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # When a value in x is NA and its successor is not (or vice versa) then that
  # should also count as a value change and location in 'y' should be set to TRUE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  y <- y | xor(is.na(x[-1L]), is.na(x[-n]))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Any TRUE locations in 'y' are the end of a run, where the next value
  # changes to something different
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- c(which(y), n)
  
  structure(list(
    lengths = diff(c(0L, i)),
    values  = x[i]
  ), class = 'rle')
}

#######
## read and fortify for ggplot plotting shp file

read_shp=function(path){
  shp=readOGR(path,encoding = "UTF-8") ## parameters allows reading
  shp$id <- row.names(shp)
  shp_fort=fortify(shp)
  shp_fort <- left_join(shp_fort, shp@data, by="id")
}

######
## Make a ggplot2 base map of France with SIM2 outlets as dots and val_name the name of the column for the color scale, that can be customized afterwards
## Data has at least longitude in x, latitude in y and a numeric filling value in val_name
path_river="C:/Users/reverdya/Documents/Docs/2_data/SIG/processed/CoursEau_idx1_wgs84.shp"
path_fr="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/IGN/contours_FR/gadm36_FRA_0.shp"
river=read_shp(path_river)
fr=read_shp(path_fr)

base_map_outlets=function(data,val_name){
  plt=ggplot(data=data)+
    geom_polygon(data=fr,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=0.5)+
    geom_path(data=river,aes(x=long,y=lat,group=group),colour="gray70",size=0.3)+
    geom_point(aes(x=x,y=y,fill=get(val_name)),size=3,shape=21)+
    coord_equal(ratio=111/78,xlim = c(-6, 9.75),ylim = c(41.25,52),expand=F)+## ratio of 1lat by 1long at 45N
    scale_x_continuous("")+
    scale_y_continuous("")+
    theme_bw(base_size = 10)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme(axis.ticks =element_blank(),axis.text = element_blank() )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank())+
    theme(strip.text = element_text(size = 12, face = "bold"))+
    guides(fill = guide_colourbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
  return(plt)
}

##########################################
##IPCC colors and others

#For continuous variable requiring good distinction
yellow_blue_5=c(rgb(255,255,204,maxColorValue=255),rgb(161,218,180,maxColorValue=255),rgb(65,182,196,maxColorValue=255),rgb(44,127,184,maxColorValue=255),rgb(37,52,148,maxColorValue=255))
#For Precipitation
precip_5=c(rgb(166,97,26,maxColorValue=255),rgb(223,194,125,maxColorValue=255),rgb(245,245,245,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(1,133,113,maxColorValue=255))
#For temperature
temp_5=c(rgb(202,0,32,maxColorValue=255),rgb(244,165,130,maxColorValue=255),rgb(247,247,247,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(5,113,176,maxColorValue=255))
#For rcp
col_3rcp=c("#0000FF","#79BCFF","#FF0000")
names(col_3rcp)=c("rcp2.6","rcp4.5","rcp8.5")
#For 2 plot or line charts
ipcc_2col=c(rgb(0,0,0,maxColorValue=255),rgb(112,160,205,maxColorValue=255))

################################################
## Reformat a divergent (around 0) color scale
## Takes a color palette (pal) and a vector of values (values) that should be represented with this color palette centered on 0 
# param is between 0 and 1, the closest it is to 0 the more colors are attributed to the central values, for 1 the palette is unchanged

# ref=seq(0,1,0.01)
# plot(x=ref,y=ref,type = "l",col="black")
# lines(x=ref,y=(ref)^(0.75),col="red")
# lines(x=ref,y=(ref)^(0.5),col="blue")
# lines(x=ref,y=(ref)^(0.25),col="forestgreen")
# lines(x=ref,y=(ref)^(0.1),col="purple")
# legend("bottomright",legend=c("1","0.75","0.5","0.25","0.1"),col=c("black","red","blue","forestgreen","purple"),lty=1)

rescale_divergent_col=function(pal=warmcool(100),values,param){
  pal <- gradient_n_pal(pal)
  tmp=rescale(seq_along(values))
  col_spacing=rescale((abs(tmp-0.5))^param*sign(tmp-0.5))
  return(pal(c(0, col_spacing,1)))
}

################################################
## Reformat a color scale
## Takes a color palette (pal) and a vector of values (values) that should be represented with this color palette centered on 0 
# param is between 0 and 1, the closest it is to 0 the more colors are attributed to the central values, for 1 the palette is unchanged

# ref=seq(0,1,0.01)
# plot(x=ref,y=ref,type = "l",col="black")
# lines(x=ref,y=(ref)^(0.75),col="red")
# lines(x=ref,y=(ref)^(0.5),col="blue")
# lines(x=ref,y=(ref)^(0.25),col="forestgreen")
# lines(x=ref,y=(ref)^(0.1),col="purple")
# legend("bottomright",legend=c("1","0.75","0.5","0.25","0.1"),col=c("black","red","blue","forestgreen","purple"),lty=1)

rescale_col=function(pal=brewer.blues(100),values,param){
  pal <- gradient_n_pal(pal)
  tmp=rescale(seq_along(values))
  col_spacing=rescale(tmp^param)
  return(pal(c(0, col_spacing,1)))
}

###################################
## Format Global temperature for use in Qualypso, to be used inside code run_QUalypso
## Difference to 1861-1900 average and rcp/gcm matching + spline smoothing
## path_data the root of the path
##simu_lst the list of simulations
#first_full year and last_full_year the first an last years with data for all simu all year round

format_global_tas=function(path_data,first_full_year,last_full_year,simu_lst,first_ref_year,last_ref_year){
  
  vecYears=seq(1861,last_full_year,1)#1861 first year for GFDL
  ## Format global temperature: difference to 1861-1900 
  paths=list.files(paste0(path_data,"raw/Global_temp/"),pattern=glob2rx("global_tas*"),full.names = T)
  for ( i in 1:length(paths)){
    tas_glob=read.csv(paths[i],skip=3,sep="",header=F)
    tas_glob=data.frame(year=tas_glob[,1],tas=apply(tas_glob[,-1],MARGIN = 1,mean))# mean of 12 months
    pre_indus_tas=mean(tas_glob$tas[tas_glob$year>=1861 & tas_glob$year<=1900])#because before no data or negative for Hadgem2-ES at least
    tas_glob$tas=tas_glob$tas-pre_indus_tas
    tas_glob=tas_glob[tas_glob$year>=1861&tas_glob$year<=last_full_year,]
    colnames(tas_glob)[2]=paste0(strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][5],"_",strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][4])#rcp_gcm name
    if(i==1){
      mat_Globaltas_gcm=tas_glob
    }else{
      mat_Globaltas_gcm=merge(mat_Globaltas_gcm,tas_glob,by="year")
    }
  }
  
  ## Format global temperature for Qualypso
  mat_Globaltas=vector(length=nrow(simu_lst),mode="list")
  vec_global_tas_gcm=unlist(lapply(colnames(mat_Globaltas_gcm),function(x) strsplit(x,"_")[[1]][2]))
  vec_global_tas_rcp=unlist(lapply(colnames(mat_Globaltas_gcm),function(x) strsplit(x,"_")[[1]][1]))
  for (i in 1:nrow(simu_lst)){
    mat_Globaltas[[i]]=mat_Globaltas_gcm[,which(vec_global_tas_gcm==simu_lst[i,]$gcm & vec_global_tas_rcp==sub(".","",simu_lst[i,]$rcp,fixed=T))]
  }
  mat_Globaltas=do.call(cbind,mat_Globaltas)
  mat_Globaltas=t(apply(mat_Globaltas,MARGIN = 2,function(x) smooth.spline(x=vecYears,y=x,spar = 1)$y))
  ref_Globaltas=apply(mat_Globaltas,MARGIN = 1,function(x) mean(x[which(vecYears==first_ref_year):which(vecYears==last_ref_year)]))
  idx=which(mat_Globaltas_gcm$year %in% seq(first_full_year,last_full_year))
  mat_Globaltas=mat_Globaltas[,idx]
  mat_Globaltas_gcm=mat_Globaltas_gcm[idx,]
  return(list(mat_Globaltas,ref_Globaltas,mat_Globaltas_gcm))
}


##############################################################################
## Time series of main effect from QUALYPSOOUT object or main effect + mu
##returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object
## nameEff the effect name as in QUALYPSOOUT
## CIlevel the uncertainties percentiles
## includeMean if True then adds mean change to effect
## plain_nameEff the plain language name of the effect
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## pre_unit the unit of the predictor
## folder_out the saving folder
## incert=TRUE adds confidence interval equivalent to Vtot-Vint

plotQUALYPSOeffect_ggplot=function(QUALYPSOOUT,nameEff,includeMean=FALSE,plain_nameEff,pred,pred_name,ind_name,bv_name,pred_unit,folder_out,xlim,incert=FALSE){
  
  Xfut = QUALYPSOOUT$Xfut
  iEff = which(QUALYPSOOUT$namesEff == nameEff)
  if (length(iEff) == 0){    stop("wrong value for nameEff")} 
  if (includeMean) {
    EffHat = QUALYPSOOUT$CHANGEBYEFFECT[[nameEff]]
  }  else {
    EffHat = QUALYPSOOUT$MAINEFFECT[[nameEff]]
  }
  nEff = dim(EffHat$MEAN)[2]
  meanRaw = EffHat$MEAN
  if(length(Xfut)<=6){#loess model is under constrained
    meanPred =meanRaw
  }else{
    meanPred = apply(meanRaw, 2, function(x) predict(loess(x ~ Xfut)))
  }
  med=data.frame(meanPred)*100
  colnames(med)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  med$pred=Xfut
  med=gather(med,key = "eff",value = "med",-pred)
  
  add.CI = QUALYPSOOUT$listOption$ANOVAmethod == "QUALYPSO"
  if (add.CI) {
    CIRaw = EffHat$CI
    if(length(Xfut)<=6){#loess model is under constrained
      CIsmooth=CIRaw
    }else{
      CIsmooth = apply(CIRaw, c(2, 3), function(x) predict(loess(x ~ Xfut)))
    }
    binf=data.frame(CIsmooth[,1,])*100
    bsup=data.frame(CIsmooth[,2,])*100
    colnames(binf)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
    colnames(bsup)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
    binf$pred=Xfut
    bsup$pred=Xfut
    binf=gather(binf,key = "eff",value = "binf",-pred)
    bsup=gather(bsup,key = "eff",value = "bsup",-pred)
    data=merge(med,binf,by=c("pred","eff"))
    data=merge(data,bsup,by=c("pred","eff"))
  }else{
    if(incert){
      Veff = QUALYPSOOUT$EFFECTVAR[, -iEff]
      Vbind = rbind(t(Veff), QUALYPSOOUT$RESIDUALVAR$MEAN)
      Vtot = colSums(Vbind)
      Vnorm = Vbind/t(replicate(n = nrow(Vbind), Vtot))
      vNormRev = apply(Vnorm, 2, rev)
      probCI = QUALYPSOOUT$listOption$probCI
      binf = qnorm(p = (1 - probCI)/2, mean = meanPred, sd = sqrt(Vtot))
      bsup = qnorm(p = 0.5 + probCI/2, mean = meanPred, sd = sqrt(Vtot))
      binf=data.frame(apply(binf,MARGIN=2, function(x) predict(loess(x ~ Xfut)))*100)
      bsup=data.frame(apply(bsup,MARGIN=2, function(x) predict(loess(x ~ Xfut)))*100)
      colnames(binf)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
      colnames(bsup)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
      binf$pred=Xfut
      bsup$pred=Xfut
      binf=gather(binf,key = "eff",value = "binf",-pred)
      bsup=gather(bsup,key = "eff",value = "bsup",-pred)
      data=merge(med,binf,by=c("pred","eff"))
      data=merge(data,bsup,by=c("pred","eff"))
    }else{
      data=med
    }
  }

  plt=ggplot(data)+
    geom_line(aes(x=pred,y=med,group=eff,color=eff),size=1)+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits=xlim,expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))
  
  if(add.CI|incert){
    plt=plt+
      geom_ribbon(aes(x=pred,ymin=binf,ymax=bsup,fill=eff),alpha=0.3)+
      geom_line(aes(x=pred,y=binf,group=eff,color=eff),size=0.5,linetype="dotted")+
      geom_line(aes(x=pred,y=bsup,group=eff,color=eff),size=0.5,linetype="dotted")
    if(colnames(QUALYPSOOUT$listScenarioInput$scenAvail)[iEff]=="rcp"){
      plt=plt+
        scale_color_discrete("Moyenne lissee et\nintervalle de confiance",type = col_3rcp[QUALYPSOOUT$listScenarioInput$listEff[[iEff]]],labels=labels_rcp[which(names(col_3rcp)%in%QUALYPSOOUT$listScenarioInput$listEff[[iEff]])])+
        scale_fill_discrete("Moyenne lissee et\nintervalle de confiance",type=col_3rcp[QUALYPSOOUT$listScenarioInput$listEff[[iEff]]],labels=labels_rcp[which(names(col_3rcp)%in%QUALYPSOOUT$listScenarioInput$listEff[[iEff]])])
    }else{
      plt=plt+
        scale_color_discrete("Moyenne lissee et\nintervalle de confiance",type = parula(nEff))+
        scale_fill_discrete("Moyenne lissee et\nintervalle de confiance",type=parula(nEff))
    }
  }else{
    if(colnames(QUALYPSOOUT$listScenarioInput$scenAvail)[iEff]=="rcp"){
      plt=plt+
        scale_color_discrete("Moyenne lissee",type = col_3rcp[QUALYPSOOUT$listScenarioInput$listEff[[iEff]]],labels=labels_rcp[which(names(col_3rcp)%in%QUALYPSOOUT$listScenarioInput$listEff[[iEff]])])+
        scale_fill_discrete("Moyenne lissee",type=col_3rcp[QUALYPSOOUT$listScenarioInput$listEff[[iEff]]],labels=labels_rcp[which(names(col_3rcp)%in%QUALYPSOOUT$listScenarioInput$listEff[[iEff]])])
    }else{
      plt=plt+
        scale_color_discrete("Moyenne lissee",type = parula(nEff))+
        scale_fill_discrete("Moyenne lissee",type=parula(nEff))
    }
  }

  

  if(includeMean){
    plt=plt+
      scale_y_continuous(paste0("Changement (%)"))+
      ggtitle(paste0("Changement des ",plain_nameEff," pour le predicteur ",pred_name,"\net l'indicateur ",ind_name," (",bv_name,")"))
    if (is.na(folder_out)){
      return(plt)
    }else{
      save.plot(plt,Filename = paste0("change_",ind_name,"_",nameEff,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
    }
  }else{
    plt=plt+
      scale_y_continuous(paste0("Effet principal (%)"))+
      ggtitle(paste0("Effet principaux des ",plain_nameEff," pour le predicteur ",pred_name,"\net l'indicateur ",ind_name," (",bv_name,")"))
    if (is.na(folder_out)){
      return(plt)
    }else{
      save.plot(plt,Filename = paste0("effect_",ind_name,"_",nameEff,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
    }
  }
  
}


##############################################################################
## Time series of mu + uncertainties
## returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## pred_unit the unit of the predictor
## folder_out the saving folder

plotQUALYPSOMeanChangeAndUncertainties_ggplot=function(QUALYPSOOUT,pred,pred_name,ind_name,bv_name,pred_unit,folder_out,xlim){
  
  probCI = QUALYPSOOUT$listOption$probCI
  Xfut = QUALYPSOOUT$Xfut
  nFut = length(Xfut)
  meanPred = QUALYPSOOUT$GRANDMEAN$MEAN
  if(length(Xfut)<=6){#loess model is under constrained
    meanPred=meanPred
  }else{
    meanPred=predict(loess(meanPred ~ Xfut))
  }
  vNorm = t(QUALYPSOOUT$DECOMPVAR)
  Vtot = QUALYPSOOUT$TOTALVAR
  nEff = nrow(vNorm) - 2
  vNormRev = apply(vNorm, 2, rev)
  binf = qnorm(p = (1 - probCI)/2, mean = meanPred, sd = sqrt(Vtot))
  bsup = qnorm(p = 0.5 + probCI/2, mean = meanPred, sd = sqrt(Vtot))
  limIntInf = limIntSup = matrix(nrow = nEff + 2, ncol = nFut)
  if(length(Xfut)<=6){#loess model is under constrained
    limIntInf[1, ] =binf
    limIntSup[1, ] =bsup
  }else{
    limIntInf[1, ] = predict(loess(binf ~ Xfut))
    limIntSup[1, ] = predict(loess(bsup ~ Xfut))
  }
  for (i in 1:(nEff + 1)) {
    binfi = limIntInf[i, ] + vNormRev[i, ] * (meanPred - binf)
    bsupi = limIntSup[i, ] - vNormRev[i, ] * (bsup - meanPred)
    if(length(Xfut)<=6){#loess model is under constrained
      limIntInf[i + 1, ] =binfi
      limIntSup[i + 1, ] =bsup
    }else{
      limIntInf[i + 1, ] = predict(loess(binfi ~ Xfut))
      limIntSup[i + 1, ] = predict(loess(bsupi ~ Xfut))
    }
  }
  
  namesEff = QUALYPSOOUT$names
  names_var=c("int","res",rev(namesEff))

  
  data=data.frame(Xfut=Xfut,mean=meanPred)
  data=cbind(data,t(limIntInf))
  colnames(data)[-c(1:2)]=names_var
  data2=gather(data,key = "var",value = "inf",-c(Xfut,mean))
  data=data.frame(Xfut=Xfut)
  data=cbind(data,t(limIntSup))
  colnames(data)[-c(1)]=names_var
  data3=gather(data,key = "var",value = "sup",-Xfut)
  data=merge(data2,data3,by=c("Xfut","var"))
  data$var=factor(data$var,levels=c(names_var))
  data$mean=data$mean*100#pourcentage
  data$inf=data$inf*100
  data$sup=data$sup*100
  
  #These colors work only if the order of the effects does not move
  col_7var=c("orange","yellow", "cadetblue1", "blue1","darkgreen", "darkgoldenrod4","darkorchid1")
  legend_var=c("Var. Int.","Var. Res.","RCM","GCM","RCP","BC","HM")
  alpha_var=c(0.5,0.8,0.8,0.8,0.8,0.8,0.8)
  
  plt=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=inf,ymax=sup,fill=var,alpha=var))+
    geom_line(aes(x=Xfut,y=mean,color="black"))+
    scale_fill_discrete("Incertitude totale et\npartition de variance",type = col_7var[1:length(names_var)],labels=legend_var[1:length(names_var)])+
    scale_color_discrete("",type="black",label="Moyenne lissee")+
    scale_alpha_manual("",values = alpha_var[1:length(names_var)])+
    guides(alpha = "none")+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits = xlim,expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    scale_y_continuous(paste0("Changement moyen (%)"))+
    ggtitle(paste0("Changement moyen et partition de variance pour\nle predicteur ",pred_name," et l'indicateur ",ind_name,"\n(",bv_name,")"))
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("meanchange_variance_",ind_name,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
  }
}

##############################################################################
## Time series of variance partition
## returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## pred_unit the unit of the predictor
## folder_out the saving folder

plotQUALYPSOTotalVarianceDecomposition_ggplot=function(QUALYPSOOUT,pred,pred_name,ind_name,bv_name,pred_unit,folder_out,xlim){
  
  Xfut = QUALYPSOOUT$Xfut
  nFut = length(Xfut)
  nEff = QUALYPSOOUT$listScenarioInput$nEff
  vecEff = 1:nEff
  VARDECOMP = QUALYPSOOUT$DECOMPVAR
  VARDECOMP[, 1:nEff] = VARDECOMP[, vecEff]
  cum = cum.smooth = rep(0, nFut)
  data=VARDECOMP
  for (i in 1:(nEff + 2)) {
    cumPrevious = cum.smooth
    cum = cum + VARDECOMP[, i]
    if(length(Xfut)<=6){#loess model is under constrained
      cum.smooth = cum
    }else{
      cum.smooth = predict(loess(cum ~ Xfut))
    }
    cum.smooth[cum.smooth < 0] = 0
    cum.smooth[cum.smooth > 1] = 1
    data[,i]=cum.smooth
  }
  data=data.frame(cbind(Xfut,data))
  data=gather(data,key = "var",value = "val",-c(Xfut))
  namesEff = QUALYPSOOUT$names
  names_var=c("InternalVar","ResidualVar",rev(namesEff))
  data$var=factor(data$var,levels=c(names_var))
  data$val=data$val*100 #percentage
  
  #These colors work only if the order of the effects does not move
  col_7var=c("orange","yellow", "cadetblue1", "blue1","darkgreen", "darkgoldenrod4","darkorchid1")
  legend_var=c("Var. Int.","Var. Res.","RCM","GCM","RCP","BC","HM")
  
  plt=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=0,ymax=val,fill=var),alpha=0.8)+
    scale_fill_discrete("Partition\nde la variance",type = col_7var[1:length(names_var)],labels=legend_var[1:length(names_var)])+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits = xlim,expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    scale_y_continuous(paste0("Partition de variance (%)"),limits = c(0,100),expand=c(0,0))+
    ggtitle(paste0("Partition de variance pour\nle predicteur ",pred_name," et l'indicateur ",ind_name," (",bv_name,")"))
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("partition_variance_",ind_name,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
  }
}

##############################################################################
## Time series of one scenario + uncertainties
## returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## nameEff name of effect as in QUALYPSOOUT
## nameScenario name of scenario as in nameEff
## plain_name_Scen plain language name of scenario for legend
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## pred_unit the unit of the predictor
## folder_out the saving folder

plotQUALYPSOTotalVarianceByScenario_ggplot=function(QUALYPSOOUT,nameEff, nameScenario,plain_name_Scen,pred,pred_name,ind_name,bv_name,pred_unit,folder_out,xlim){
  
  Xfut = QUALYPSOOUT$Xfut
  nFut = length(Xfut)
  iEff = which(QUALYPSOOUT$namesEff == nameEff)
  if (length(iEff) == 0){stop("wrong value for nameEff")}
  iScenario = which(QUALYPSOOUT$listScenarioInput$listEff[[iEff]] == 
                      nameScenario)
  meanPred = QUALYPSOOUT$CHANGEBYEFFECT[[nameEff]]$MEAN[,iScenario]
  if(length(Xfut)<=6){#loess model is under constrained
    meanPred=meanPred
  }else{
    meanPred=predict(loess(meanPred ~ Xfut))
  }
  Veff = QUALYPSOOUT$EFFECTVAR[, -iEff]
  Vbind = rbind(t(Veff), QUALYPSOOUT$RESIDUALVAR$MEAN, QUALYPSOOUT$INTERNALVAR)
  nEff = nrow(Vbind) - 2
  Vtot = colSums(Vbind)
  Vnorm = Vbind/t(replicate(n = nrow(Vbind), Vtot))
  vNormRev = apply(Vnorm, 2, rev)
  probCI = QUALYPSOOUT$listOption$probCI
  binf = qnorm(p = (1 - probCI)/2, mean = meanPred, sd = sqrt(Vtot))
  bsup = qnorm(p = 0.5 + probCI/2, mean = meanPred, sd = sqrt(Vtot))
  limIntInf = limIntSup = matrix(nrow = nEff + 2, ncol = nFut)
  if(length(Xfut)<=6){#loess model is under constrained
    limIntInf[1, ] =binf
    limIntSup[1, ] =bsup
  }else{
    limIntInf[1, ] = predict(loess(binf ~ Xfut))
    limIntSup[1, ] = predict(loess(bsup ~ Xfut))
  }
  for (i in 1:(nEff + 1)) {
    binfi = limIntInf[i, ] + vNormRev[i, ] * (meanPred - 
                                                binf)
    bsupi = limIntSup[i, ] - vNormRev[i, ] * (bsup - meanPred)
    if(length(Xfut)<=6){#loess model is under constrained
      limIntInf[i + 1, ] =binfi
      limIntSup[i + 1, ] =bsup
    }else{
      limIntInf[i + 1, ] = predict(loess(binfi ~ Xfut))
      limIntSup[i + 1, ] = predict(loess(bsupi ~ Xfut))
    }
  }
  
  namesEff = QUALYPSOOUT$names
  namesEff=namesEff[namesEff!=nameEff]
  names_var=c("int","res",rev(namesEff))
  
  
  data=data.frame(Xfut=Xfut,mean=meanPred)
  data=cbind(data,t(limIntInf))
  colnames(data)[-c(1:2)]=names_var
  data2=gather(data,key = "var",value = "inf",-c(Xfut,mean))
  data=data.frame(Xfut=Xfut)
  data=cbind(data,t(limIntSup))
  colnames(data)[-c(1)]=names_var
  data3=gather(data,key = "var",value = "sup",-Xfut)
  data=merge(data2,data3,by=c("Xfut","var"))
  data$var=factor(data$var,levels=c(names_var))
  data$mean=data$mean*100#pourcentage
  data$inf=data$inf*100
  data$sup=data$sup*100
  
  #These colors work only if the order and the name of the effects does not move
  col_7var=c("orange","yellow", "cadetblue1", "blue1","darkgreen", "darkgoldenrod4","darkorchid1")
  legend_var=c("Var. Int.","Var. Res.","RCM","GCM","RCP","BC","HM")
  legend_var_full=c("int","res","rcm","gcm","rcp","bc","hm")
  alpha_var=c(0.5,0.8,0.8,0.8,0.8,0.8,0.8)
  alpha_var=alpha_var[-which(legend_var_full==nameEff)]
  legend_var=legend_var[-which(legend_var_full==nameEff)]
  col_7var=col_7var[-which(legend_var_full==nameEff)]
  
  
  plt=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=inf,ymax=sup,fill=var,alpha=var))+
    geom_line(aes(x=Xfut,y=mean,color="black"))+
    scale_fill_discrete("Incertitude totale et\npartition de variance",type = col_7var[1:length(names_var)],labels=legend_var[1:length(names_var)])+
    scale_color_discrete("",type="black",label="Moyenne lissee")+
    scale_alpha_manual("",values = alpha_var[1:length(names_var)])+
    guides(alpha = "none")+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits = xlim,expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    scale_y_continuous(paste0("Changement moyen (%)"))+
    ggtitle(paste0("Changement et partition de variance pour les experiences ",plain_name_Scen,"\nle predicteur ",pred_name," et l'indicateur ",ind_name,"\n(",bv_name,")"))
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("change_variance_",nameScenario,"_",ind_name,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
  }
}




#######################################################################
## Map of 3 quantiles by 3 RCP for one horizon
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## horiz a temporal or temperature horizon
## pred_name the plain language name of the predictor
## ind_name thenname of the indicator
## pred_unit the unit of the predictor
## folder_out the saving folder
## scale_col the level of deformation of the color scale
## bin_col the break levels for the color scale

map_3quant_3rcp_1horiz=function(lst.QUALYPSOOUT,horiz,pred_name,pred,pred_unit,ind_name,folder_out,scale_col=1,bin_col=20){
  
  ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
  rcp_names=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]
  quant=c("5%","mean","95%")

  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
  exut$idx=seq(1:nrow(exut))
  
  tmp=exut
  for (i in 1:8){
    exut=rbind(exut,tmp)
  }
  exut$rcp=rep(rcp_names,each=nrow(exut)/3)
  exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  exut$val=0
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    for(r in rcp_names){
      idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==horiz)
      ieff_this_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]]==r)
      chg_mean=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,ieff_this_rcp]
      Veff = lst.QUALYPSOOUT[[i]]$EFFECTVAR[idx_Xfut, -ieff_rcp]
      Vtot = sum(Veff, lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut])
      probCI = lst.QUALYPSOOUT[[i]]$listOption$probCI
      chg_q5 = qnorm(p = (1 - probCI)/2, mean = chg_mean, sd = sqrt(Vtot))
      chg_q95 = qnorm(p = 0.5 + probCI/2, mean = chg_mean, sd = sqrt(Vtot))
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[2]]=chg_mean*100#*100 for percentages
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[1]]=chg_q5*100
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[3]]=chg_q95*100
    }
  }
  
  exut$quant=factor(exut$quant,levels=quant)
  colnames(exut)=c("Num_ordre_Modcou","y","x","idx","rcp","quant","val")
  
  rcp.labs <- c("RCP 2.6", "RCP 4.5", "RCP 8.5")
  names(rcp.labs) <- rcp_names
  quant.labs <- c("Q5", "Moyenne", "Q95")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  q99pos=quantile(exut$val[exut$val>=0],probs=0.99)
  q99neg=abs(quantile(exut$val[exut$val<=0],probs=0.01))
  lim_col=max(q99pos,q99neg)
  lim_col=round(lim_col/bin_col)*bin_col#arrondi au bin_col le plus proche
  #lim_col=(lim_col%/%bin_col+1)*bin_col
  
  plt=base_map_outlets(data = exut,val_name = "val")
  plt=plt+
    facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
    scale_fill_gradientn("Changement relatif (%)",colours = rescale_divergent_col( warmcool(100),exut$val,scale_col),limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,bin_col),oob=squish,labels=c(paste0("< -",lim_col),seq(-lim_col+bin_col,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
    ggtitle(paste0("Changement relatif de ",ind_name," et son incertitude pour\ndifferents RCP et le predicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
    theme(panel.border = element_rect(colour = "black",fill=NA))
  plt$layers[[3]]$aes_params$size= 1.5
  save.plot(plt,Filename = paste0("map_3rcp_3quant_",ind_name,"_",pred,"_",horiz,"_scale-col",scale_col),Folder = folder_out,Format = "jpeg")
}


#######################################################################
##Map of 3 quantiles by 3 horizons for one RCP
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## horiz 3 temporal or temperature horizons
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## pred_unit the unit of the predictor
## rcp_name the name of the wanted rcp
## folder_out the saving folder
## scale_col the level of deformation of the color scale
## bin_col the break levels for the color scale

map_3quant_1rcp_3horiz=function(lst.QUALYPSOOUT,horiz,rcp_name, rcp_plainname,pred,pred_name,pred_unit,ind_name,folder_out,scale_col=1,bin_col=20){

  ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
  ieff_this_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]]==rcp_name)
  quant=c("5%","mean","95%")
  
  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
  exut$idx=seq(1:nrow(exut))
  
  tmp=exut
  for (i in 1:8){
    exut=rbind(exut,tmp)
  }
  exut$horiz=rep(horiz,each=nrow(exut)/3)
  exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  exut$val=0
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    for(h in horiz){
      idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==h)
      chg_mean=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,ieff_this_rcp]
      Veff = lst.QUALYPSOOUT[[i]]$EFFECTVAR[idx_Xfut, -ieff_rcp]
      Vtot = sum(Veff, lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut])
      probCI = lst.QUALYPSOOUT[[i]]$listOption$probCI
      chg_q5 = qnorm(p = (1 - probCI)/2, mean = chg_mean, sd = sqrt(Vtot))
      chg_q95 = qnorm(p = 0.5 + probCI/2, mean = chg_mean, sd = sqrt(Vtot))
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[2]]=chg_mean*100#*100 for percentages
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[1]]=chg_q5*100
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[3]]=chg_q95*100
    }
  }
  
  exut$quant=factor(exut$quant,levels=quant)
  colnames(exut)=c("Num_ordre_Modcou","y","x","idx","horiz","quant","val")
  
  horiz.labs <- paste0(horiz," ",pred_unit)
  names(horiz.labs) <- horiz
  quant.labs <- c("Q5", "Moyenne", "Q95")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  q99pos=quantile(exut$val[exut$val>=0],probs=0.99)
  q99neg=abs(quantile(exut$val[exut$val<=0],probs=0.01))
  lim_col=max(q99pos,q99neg)
  lim_col=round(lim_col/bin_col)*bin_col#arrondi au bin_col le plus proche
  #lim_col=(lim_col%/%bin_col+1)*bin_col
  
  plt=base_map_outlets(data = exut,val_name = "val")
  plt=plt+
    facet_grid(horiz ~ quant,labeller = labeller(horiz=horiz.labs, quant = quant.labs))+
    scale_fill_gradientn("Changement relatif (%)",colours = rescale_divergent_col( warmcool(100),exut$val,scale_col),limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,bin_col),oob=squish,labels=c(paste0("< -",lim_col),seq(-lim_col+bin_col,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
    ggtitle(paste0("Changement relatif de ",ind_name," et son incertitude pour\ndifferents horizons et le predicteur ",pred_name,"\n(",rcp_plainname," avec reference 1990)"))+
    theme(panel.border = element_rect(colour = "black",fill=NA))
  plt$layers[[3]]$aes_params$size= 1.5
  save.plot(plt,Filename = paste0("map_3quant_3horiz_",ind_name,"_",pred,"_",rcp_name,"_scale-col",scale_col),Folder = folder_out,Format = "jpeg")



}


#######################################################################
##Map of 3 quantiles by 3 horizons of temperature
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## horiz 3 temporal or temperature horizons
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## pred_unit the unit of the predictor
## rcp_name the name of the wanted rcp
## folder_out the saving folder
## scale_col the level of deformation of the color scale
## bin_col the break levels for the color scale

map_3quant_1.5_2_2.5_degC=function(lst.QUALYPSOOUT3,lst.QUALYPSOOUT2,ind_name,folder_out,scale_col=1,bin_col=20){
  
  quant=c("5%","mean","95%")
  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
  exut$idx=seq(1:nrow(exut))
  horiz=c(1.5,2,2.5)
  
  tmp=exut
  for (i in 1:8){
    exut=rbind(exut,tmp)
  }
  exut$horiz=rep(horiz,each=nrow(exut)/3)
  exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  exut$val=0
  
  for (i in 1:length(lst.QUALYPSOOUT3)){
    for(h in horiz[1]){# all RCP
      idx_Xfut=which(lst.QUALYPSOOUT3[[i]]$Xfut==h)
      chg_mean=lst.QUALYPSOOUT3[[i]]$GRANDMEAN$MEAN[idx_Xfut]
      Veff = lst.QUALYPSOOUT3[[i]]$EFFECTVAR[idx_Xfut,]
      Vtot = sum(Veff, lst.QUALYPSOOUT3[[i]]$RESIDUALVAR$MEAN[idx_Xfut])
      probCI = lst.QUALYPSOOUT3[[i]]$listOption$probCI
      chg_q5 = qnorm(p = (1 - probCI)/2, mean = chg_mean, sd = sqrt(Vtot))
      chg_q95 = qnorm(p = 0.5 + probCI/2, mean = chg_mean, sd = sqrt(Vtot))
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[2]]=chg_mean*100#*100 for percentages
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[1]]=chg_q5*100
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[3]]=chg_q95*100
    }
    for(h in horiz[2:3]){#2 RCP only
      idx_Xfut=which(lst.QUALYPSOOUT2[[i]]$Xfut==h)
      chg_mean=lst.QUALYPSOOUT2[[i]]$GRANDMEAN$MEAN[idx_Xfut]
      Veff = lst.QUALYPSOOUT2[[i]]$EFFECTVAR[idx_Xfut,]
      Vtot = sum(Veff, lst.QUALYPSOOUT2[[i]]$RESIDUALVAR$MEAN[idx_Xfut])
      probCI = lst.QUALYPSOOUT2[[i]]$listOption$probCI
      chg_q5 = qnorm(p = (1 - probCI)/2, mean = chg_mean, sd = sqrt(Vtot))
      chg_q95 = qnorm(p = 0.5 + probCI/2, mean = chg_mean, sd = sqrt(Vtot))
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[2]]=chg_mean*100#*100 for percentages
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[1]]=chg_q5*100
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[3]]=chg_q95*100
    }
  }
  
  exut$quant=factor(exut$quant,levels=quant)
  colnames(exut)=c("Num_ordre_Modcou","y","x","idx","horiz","quant","val")
  
  horiz.labs <- paste0(as.character(horiz)," deg C")
  names(horiz.labs) <- horiz
  quant.labs <- c("Q5", "Moyenne", "Q95")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  q99pos=quantile(exut$val[exut$val>=0],probs=0.99)
  q99neg=abs(quantile(exut$val[exut$val<=0],probs=0.01))
  lim_col=max(q99pos,q99neg)
  lim_col=round(lim_col/bin_col)*bin_col#arrondi au bin_col le plus proche
  #lim_col=(lim_col%/%bin_col+1)*bin_col
  
  plt=base_map_outlets(data = exut,val_name = "val")
  plt=plt+
    facet_grid(horiz ~ quant,labeller = labeller(horiz=horiz.labs, quant = quant.labs))+
    scale_fill_gradientn("Changement relatif (%)",colours = rescale_divergent_col( warmcool(100),exut$val,scale_col),limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,bin_col),oob=squish,labels=c(paste0("< -",lim_col),seq(-lim_col+bin_col,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
    ggtitle(paste0("Changement relatif de ",ind_name," et son incertitude pour\ndifferents horizons de temperature planetaire (en deg C)\n(avec reference 1990, 3 RCP pour 1.5 degC et 2 RCP pour 2 et 3 deg C)"))+
    theme(panel.border = element_rect(colour = "black",fill=NA))
  plt$layers[[3]]$aes_params$size= 1.5
  save.plot(plt,Filename = paste0("map_3quant_3horiz_",ind_name,"_temp_1.5_2_2.5_scale-col",scale_col),Folder = folder_out,Format = "jpeg")
  
  
  
}

#######################################################################
## Map of effects GCM or RCM (temperature or time)
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## includeMean if true adds mean change to effect
## name_eff "gcm" ou "rcm"...
## pred_name the plain language name of the predictor
## pred the predictor name in the file
## ind_name the plain language name of the indicator
## pred_unit the unit of the predictor
## folder_out the saving folder
## horiz he horizon of predictor wanted
## name_eff_plain plain langauge name of principal effects
## scale_col the level of deformation of the color scale
## bin_col the break levels for the color scale

map_main_effect=function(lst.QUALYPSOOUT,includeMean=FALSE,horiz,name_eff,name_eff_plain,pred,pred_name,pred_unit,ind_name,folder_out,scale_col=1,bin_col=20){
  
  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
  exut$idx=seq(1:nrow(exut))
  
  ieff=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)==name_eff)
  effs.labs <- lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff]]
  effs=seq(1,length(effs.labs))
  names(effs.labs) <- effs
  
  
  tmp=exut
  for (i in 1:(length(effs)-1)){
    exut=rbind(exut,tmp)
  }
  exut$effs=rep(effs,each=nrow(exut)/length(effs))
  exut$val=0
  
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==horiz)
    if(includeMean){
      chg=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT[[name_eff]]$MEAN[idx_Xfut,]*100 #*100 for percentages
    }else{
      chg=lst.QUALYPSOOUT[[i]]$MAINEFFECT[[name_eff]]$MEAN[idx_Xfut,]*100 #*100 for percentages
    }
    for(j in 1:length(chg)){
      exut$val[exut$idx==i & exut$effs==j]=chg[j]
    }
  }
  
  colnames(exut)=c("Num_ordre_Modcou","y","x","idx","effs","val")
  
  #Setting limits for color scale
  q99pos=quantile(exut$val[exut$val>=0],probs=0.99)
  q99neg=abs(quantile(exut$val[exut$val<=0],probs=0.01))
  lim_col=max(q99pos,q99neg)
  lim_col=round(lim_col/bin_col)*bin_col#arrondi au bin_col le plus proche
  #lim_col=(lim_col%/%bin_col+1)*bin_col
  
  plt=base_map_outlets(data = exut,val_name = "val")
  if(includeMean){
    plt=plt+
      facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
      scale_fill_gradientn("Changement (%)",colours = rescale_divergent_col( warmcool(100),exut$val,scale_col),limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,bin_col),oob=squish,labels=c(paste0("< -",lim_col),seq(-lim_col+bin_col,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
      ggtitle(paste0("Changement des ",name_eff_plain,"s pour ",ind_name,"\net le predicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
      theme(panel.border = element_rect(colour = "black",fill=NA))
    plt$layers[[3]]$aes_params$size= 1.5
    save.plot(plt,Filename = paste0("map_change_",name_eff,"_",ind_name,"_",pred,"_",horiz,"_scale-col",scale_col),Folder = folder_out,Format = "jpeg")
    
  }else{
    plt=plt+
      facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
      scale_fill_gradientn("Effet principal (%)",colours = rescale_divergent_col( warmcool(100),exut$val,scale_col),limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,bin_col),oob=squish,labels=c(paste0("< -",lim_col),seq(-lim_col+bin_col,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
      ggtitle(paste0("Effet principaux des ",name_eff_plain,"s pour ",ind_name,"\net le predicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
      theme(panel.border = element_rect(colour = "black",fill=NA))
    plt$layers[[3]]$aes_params$size= 1.5
    save.plot(plt,Filename = paste0("map_effect_",name_eff,"_",ind_name,"_",pred,"_",horiz,"_scale-col",scale_col),Folder = folder_out,Format = "jpeg")
    
  }
}


#######################################################################
## Map mean change or internal variability or total variability
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## vartype the variable to be plotted (one of varint, mean, vartot)
## pred_name the plain language name of the predictor
## pred the predictor name in the file
## ind_name the plain language name of the indicator
## pred_unit the unit of the predictor
## folder_out the saving folder
## horiz he horizon of predictor wanted
## name_eff_plain plain langauge name of principal effects
## scale_col the level of deformation of the color scale
## bin_col the break levels for the color scale

map_one_var=function(lst.QUALYPSOOUT,vartype,horiz,pred,pred_name,pred_unit,ind_name,folder_out,scale_col=1,bin_col=20){
  
  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
  exut$idx=seq(1:nrow(exut))
  exut$val=0
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==horiz)
    if(vartype=="mean"){
      chg=lst.QUALYPSOOUT[[i]]$GRANDMEAN$MEAN[idx_Xfut]
    }
    if(vartype=="varint"){
      chg=lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut]
    }
    if(vartype=="varres"){
      chg=lst.QUALYPSOOUT[[i]]$RESIDUALVAR[idx_Xfut]
    }
    if(vartype=="vartot"){
      Veff = lst.QUALYPSOOUT[[i]]$EFFECTVAR[idx_Xfut,]
      chg = sum(Veff, lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut],lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut])
    }
    exut$val[exut$idx==i]=chg*100
  }
  
  colnames(exut)=c("Num_ordre_Modcou","y","x","idx","val")
  
  plt=base_map_outlets(data = exut,val_name = "val")
  if(vartype=="mean"){
    q99pos=quantile(exut$val[exut$val>=0],probs=0.99)
    q99neg=abs(quantile(exut$val[exut$val<=0],probs=0.01))
    lim_col=max(q99pos,q99neg)
    lim_col=round(lim_col/bin_col)*bin_col
    plt=plt+
      scale_fill_gradientn("Changement relatif (%)",colours = rescale_divergent_col( warmcool(100),exut$val,scale_col),limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,bin_col),oob=squish,labels=c(paste0("< -",lim_col),seq(-lim_col+bin_col,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
      ggtitle(paste0("Changement relatif moyen de ",ind_name,"\npour le predicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))
  }
  if(vartype=="vartot"){
    q99=quantile(exut$val,probs=0.99)
    lim_col=round(q99/bin_col)*bin_col
    plt=plt+
      scale_fill_gradientn("Variabilite\ntotale (%)",colours = parula(100),limits=c(0,lim_col),breaks=seq(0,lim_col,bin_col),oob=squish,labels=c(seq(0,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
      ggtitle(paste0("Variabilite totale de ",ind_name,"\npour le predicteur ",pred_name," (",horiz," ",pred_unit,")"))
  }
  if(vartype=="varint"){
    q99=quantile(exut$val,probs=0.99)
    lim_col=round(q99/bin_col)*bin_col
    plt=plt+
      scale_fill_gradientn("Variabilite\ninterne (%)",colours = parula(100),limits=c(0,lim_col),breaks=seq(0,lim_col,bin_col),oob=squish,labels=c(seq(0,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
      ggtitle(paste0("Variabilite interne de ",ind_name,"\npour le predicteur ",pred_name," (",horiz," ",pred_unit,")"))
  }
  
  if(vartype=="varint"){
    q99=quantile(exut$val,probs=0.99)
    lim_col=round(q99/bin_col)*bin_col
    plt=plt+
      scale_fill_gradientn("Variabilite\nresiduelle (%)",colours = parula(100),limits=c(0,lim_col),breaks=seq(0,lim_col,bin_col),oob=squish,labels=c(seq(0,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
      ggtitle(paste0("Variabilite residuelle de ",ind_name,"\npour le predicteur ",pred_name," (",horiz," ",pred_unit,")"))
  }

  save.plot(plt,Filename = paste0("map_total_change_",vartype,"_",ind_name,"_",pred,"_",horiz,"_scale-col",scale_col),Folder = folder_out,Format = "jpeg")
  
}


#############################################################################################
## Map of 3 quantiles by 3 RCP for one horizon of time using basic mean and q5/q95
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## horiz a temporal horizon
## ind_name then name of the indicator
## folder_out the saving folder
## scale_col the level of deformation of the color scale
## bin_col the break levels for the color scale

map_3quant_3rcp_1horiz_basic=function(lst.QUALYPSOOUT,horiz,ind_name,folder_out,scale_col=1,bin_col=20){
  
  ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
  rcp_names=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]
  quant=c("5%","mean","95%")
  idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==horiz)
  
  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon")]
  exut$idx=seq(1:nrow(exut))
  
  tmp=exut
  for (i in 1:8){
    exut=rbind(exut,tmp)
  }
  exut$rcp=rep(rcp_names,each=nrow(exut)/3)
  exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  exut$val=0
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    for(r in rcp_names){
      
      ieff_this_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]]==r)
      chg_mean=mean(lst.QUALYPSOOUT[[i]]$CLIMATEESPONSE$phiStar[,idx_Xfut])*100 #*100 for percentages
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[2]]=chg_mean
      chg_q5=quantile(lst.QUALYPSOOUT[[i]]$CLIMATEESPONSE$phiStar[,idx_Xfut],probs=0.05)*100 #*100 for percentages ; 1 is for 5%
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[1]]=chg_q5
      chg_q95=quantile(lst.QUALYPSOOUT[[i]]$CLIMATEESPONSE$phiStar[,idx_Xfut],probs=0.95)*100  #*100 for percentages ; 2 is for 95%
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[3]]=chg_q95
    }
  }
  
  exut$quant=factor(exut$quant,levels=quant)
  colnames(exut)=c("Num_ordre_Modcou","y","x","idx","rcp","quant","val")
  
  rcp.labs <- c("RCP 2.6", "RCP 4.5", "RCP 8.5")
  names(rcp.labs) <- rcp_names
  quant.labs <- c("Q5", "Moyenne", "Q95")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  q99pos=quantile(exut$val[exut$val>=0],probs=0.99)
  q99neg=abs(quantile(exut$val[exut$val<=0],probs=0.01))
  lim_col=max(q99pos,q99neg)
  lim_col=round(lim_col/bin_col)*bin_col#arrondi au bin_col le plus proche
  #lim_col=(lim_col%/%bin_col+1)*bin_col
  
  plt=base_map_outlets(data = exut,val_name = "val")
  plt=plt+
    facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
    scale_fill_gradientn("Changement relatif (%)",colours = rescale_divergent_col( warmcool(100),exut$val,scale_col),limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,bin_col),oob=squish,labels=c(paste0("< -",lim_col),seq(-lim_col+bin_col,lim_col-bin_col,bin_col),paste0("> ",lim_col)))+
    ggtitle(paste0('Changement relatif de ',ind_name,' et son incertitude pour\ndifferents RCP (',horiz,' VS 1990), methode "classique"'))+
    theme(panel.border = element_rect(colour = "black",fill=NA))
  plt$layers[[3]]$aes_params$size= 1.5
  save.plot(plt,Filename = paste0("basic_meth_map_3rcp_3quant_",ind_name,"_time_",horiz,"_scale-col",scale_col),Folder = folder_out,Format = "jpeg")
}


#####################################################
## Compare results out of QUALYSPO and LM method
## lm the list of QUALYPSOOUT for all watersheds and the lm method
## qu the list of QUALYPSOOUT for all watersheds and the qualypso method
## type the variable of interest (one of resvar,grandmean or eff)
## nameEff in the case of eff the name of the effect considered as in QUALYPSOOUT
## pred_name the name of the predictor used
## ind_name the name of the indicator used
## var_name the plain language name of the variable of interest (Effet RCP, Moyenne d'ensemble...)
## folder_out the saving folder

plot_comp_anova=function(LM,QU,type,nameEff=NULL,pred_name,ind_name,var_name,folder_out,interactive_plt=F){
  if (type=="resvar"){
    x=unlist(lapply(LM,function(x) x$RESIDUALVAR$MEAN))
    y=unlist(lapply(QU,function(x) x$RESIDUALVAR$MEAN))
    data=data.frame(x=x,y=y)
  }
  if (type=="grandmean"){
    x=unlist(lapply(LM,function(x) x$GRANDMEAN$MEAN))
    y=unlist(lapply(QU,function(x) x$GRANDMEAN$MEAN))
    data=data.frame(x=x,y=y)
  }
  if (type=="eff"){
    ieff=which(colnames(LM[[1]]$listScenarioInput$scenAvail)==nameEff)
    
    x=lapply(LM,function(x) x$MAINEFFECT[[nameEff]]$MEAN)
    x=lapply(x,"colnames<-",LM[[1]]$listScenarioInput$listEff[[ieff]])
    x=lapply(x,function(z) gather(data.frame(z),key="eff",value="x"))
    x=do.call("rbind",x)
    
    y=lapply(QU,function(y) y$MAINEFFECT[[nameEff]]$MEAN)
    y=lapply(y,"colnames<-",LM[[1]]$listScenarioInput$listEff[[ieff]])
    y=lapply(y,function(z) gather(data.frame(z),key="eff",value="y"))
    y=do.call("rbind",y)
    data=cbind(x,y$y)#merge does not work because too many rows
    colnames(data)=c("eff","x","y")
  }
  
  plt=ggplot(data)+
    geom_point(aes(x=x,y=y),size=0.5)+
    geom_abline(slope=1,size=0.3,col="red")+
    xlab("Linear method")+
    ylab("Qualypso")+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    ggtitle(paste0("Comparaison methodes ANOVA (tous les bassins et dates)\npour le predicteur ",pred_name,", l'indicateur ",ind_name,"\net la grandeur ",var_name))
  if (type=="eff"){
    plt=plt+
      facet_wrap(~eff,ncol=3)
  }
  save.plot(plt,Filename = paste0("comparison_anova_",ind_name,"_",pred_name,"_",type,nameEff),Folder = folder_out,Format = "jpeg")
  if (interactive_plt){
    pltly=ggplotly(plt,tooltip = c("x","y"))
    setwd(folder_out)
    saveWidget(pltly,paste0("comparison_anova_",ind_name,"_",pred_name,"_",type,nameEff,".html"))
  }


}

#####################################################
## Plot watershed areas

plot_bv_areas=function(folder_out){
  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon","Surf_mod")]
  colnames(exut)=c("Num_ordre_Modcou","y","x","val")
  plt=base_map_outlets(data = exut,val_name = "val")
  plt=plt+
    scale_fill_gradientn("Superficie (km2)",colours = parula(100),trans="log10")+
    ggtitle("Superficie des bassins versants")
  save.plot(plt,Filename = "superficie_bv",Folder = folder_out,Format = "jpeg")
}
