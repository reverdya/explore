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
    geom_path(data=river,aes(x=long,y=lat,group=group),colour="dodgerblue2",size=0.3)+
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


###################################
## Format Global temperature for use in Qualypso, to be used inside code run_QUalypso
## Difference to 1850-1900 average and rcp/gcm matching + spline smoothing
## path_data the root of the path
##simu_lst the list of simulations
#first_full year and last_full_year the first an last years with data for all simu all year round

format_global_tas=function(path_data,first_full_year,last_full_year,simu_lst,first_ref_year,last_ref_year){
  
  vecYears=seq(first_full_year,last_full_year,1)
  ## Format global temperature: difference to 1850-1900 
  paths=list.files(paste0(path_data,"raw/Global_temp/"),pattern=glob2rx("global_tas*"),full.names = T)
  for ( i in 1:length(paths)){
    tas_glob=read.csv(paths[i],skip=3,sep="",header=F)
    tas_glob=data.frame(year=tas_glob[,1],tas=apply(tas_glob[,-1],MARGIN = 1,mean))
    pre_indus_tas=mean(tas_glob$tas[tas_glob$year>=1850 & tas_glob$year<=1900])
    tas_glob$tas=tas_glob$tas-pre_indus_tas
    tas_glob=tas_glob[tas_glob$year>=first_full_year&tas_glob$year<=last_full_year,]
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
  return(list(mat_Globaltas,ref_Globaltas))
}


##############################################################################
## Time series of main effect from QUALYPSOOUT object or main effect + mu
##returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object
## iEff the number of the given effect in QUALYPSOOUT
## CIlevel the uncertainties percentiles
## includeMean if True then adds mean change to effect
## eff_name the plain language name of the effect
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## ind_unit the unit of the indicator
## pre_unit the unit of the predictor

plotQUALYPSOeffect_ggplot=function(QUALYPSOOUT,iEff,CIlevel=c("5%","95%"),includeMean=FALSE,eff_name,pred_name,ind_name,bv_name,ind_unit,pred_unit,folder_out){
  
  # vector of predictors
  Xfut = QUALYPSOOUT$Xfut
  
  # retrieve effects
  if(includeMean){
    QUANTEff = QUALYPSOOUT$CHANGEBYEFFECT[[iEff]]$QUANT
  }else{
    QUANTEff = QUALYPSOOUT$MAINEFFECT[[iEff]]$QUANT
  }
  nEff = dim(QUANTEff)[3]
  
  # find index quantiles
  iMedian = which(colnames(QUANTEff)=="50%")
  iBinf = which(colnames(QUANTEff)==CIlevel[1])
  iBsup = which(colnames(QUANTEff)==CIlevel[2])
  if((length(iBinf)!=1)|(length(iBsup)!=1)) stop(paste0('Quantiles ',CIlevel, "are not available, check argument CIlevel"))
  
  # retrieve median and limits
  medRaw = QUANTEff[,iMedian,]
  binfRaw = QUANTEff[,iBinf,]
  bsupRaw = QUANTEff[,iBsup,]
  
  # get smooth limits
  med = data.frame(apply(medRaw,2,function(x) predict(loess(x~Xfut))))
  binf = data.frame(apply(binfRaw,2,function(x) predict(loess(x~Xfut))))
  bsup = data.frame(apply(bsupRaw,2,function(x) predict(loess(x~Xfut))))
  
  # Format for ggplot
  colnames(med)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  colnames(binf)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  colnames(bsup)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  med$pred=Xfut
  binf$pred=Xfut
  bsup$pred=Xfut
  med=gather(med,key = "eff",value = "med",-pred)
  binf=gather(binf,key = "eff",value = "binf",-pred)
  bsup=gather(bsup,key = "eff",value = "bsup",-pred)
  data=merge(med,binf,by=c("pred","eff"))
  data=merge(data,bsup,by=c("pred","eff"))
  
  plt=ggplot(data)+
    geom_ribbon(aes(x=pred,ymin=binf,ymax=bsup,fill=eff),alpha=0.3)+
    geom_line(aes(x=pred,y=med,group=eff,color=eff),size=1)+
    geom_line(aes(x=pred,y=binf,group=eff,color=eff),size=0.5,linetype="dotted")+
    geom_line(aes(x=pred,y=bsup,group=eff,color=eff),size=0.5,linetype="dotted")+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))
    
  if(colnames(QUALYPSOOUT$listScenarioInput$scenAvail)[iEff]=="rcp"){
    plt=plt+
      scale_color_discrete("Mediane lissee avec\nintervalle 5-95%",type = col_3rcp,labels=labels_rcp)+
      scale_fill_discrete("Mediane lissee avec\nintervalle 5-95%",type=col_3rcp,labels=labels_rcp)
  }else{
    plt=plt+
      scale_color_discrete("Mediane lissee avec\nintervalle 5-95%",type = parula(nEff))+
      scale_fill_discrete("Mediane lissee avec\nintervalle 5-95%",type=parula(nEff))
  }
  
  if(includeMean){
    plt=plt+
      scale_y_continuous(paste0("Changement (",ind_unit,")"))+
      ggtitle(paste0("Changement des ",eff_name," pour le predicteur ",pred_name,"\net l'indicateur ",ind_name," (",bv_name,")"))
    save.plot(plt,Filename = paste0("change_",ind_name,"_",eff_name,"_",pred_name,"_",bv_name),Folder = folder_out,Format = "jpeg")
  }else{
    plt=plt+
      scale_y_continuous(paste0("Effet principal (",ind_unit,")"))+
      ggtitle(paste0("Effet principaux des ",eff_name," pour le predicteur ",pred_name,"\net l'indicateur ",ind_name," (",bv_name,")"))
    save.plot(plt,Filename = paste0("effect_",ind_name,"_",eff_name,"_",pred_name,"_",bv_name),Folder = folder_out,Format = "jpeg")
  }
  
}


##############################################################################
## Time series of mu + uncertainties
## returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## ind_unit the unit of the indicator
## pred_unit the unit of the predictor

plotQUALYPSOMeanChangeAndUncertainties_ggplot=function(QUALYPSOOUT,pred_name,ind_name,bv_name,ind_unit,pred_unit,folder_out){
  
  probCI = QUALYPSOOUT$listOption$probCI
  Xfut = QUALYPSOOUT$Xfut
  nFut = length(Xfut)
  meanPred = QUALYPSOOUT$GRANDMEAN$MEAN
  meanPred=predict(loess(meanPred ~ Xfut))
  vNorm = t(QUALYPSOOUT$DECOMPVAR)
  Vtot = QUALYPSOOUT$TOTALVAR
  nEff = nrow(vNorm) - 2
  vNormRev = apply(vNorm, 2, rev)
  binf = qnorm(p = (1 - probCI)/2, mean = meanPred, sd = sqrt(Vtot))
  bsup = qnorm(p = 0.5 + probCI/2, mean = meanPred, sd = sqrt(Vtot))
  limIntInf = limIntSup = matrix(nrow = nEff + 2, ncol = nFut)
  limIntInf[1, ] = predict(loess(binf ~ Xfut))
  limIntSup[1, ] = predict(loess(bsup ~ Xfut))
  for (i in 1:(nEff + 1)) {
    binfi = limIntInf[i, ] + vNormRev[i, ] * (meanPred - 
                                                binf)
    limIntInf[i + 1, ] = predict(loess(binfi ~ Xfut))
    bsupi = limIntSup[i, ] - vNormRev[i, ] * (bsup - meanPred)
    limIntSup[i + 1, ] = predict(loess(bsupi ~ Xfut))
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
  
  #These colors work only if the order of the effects does not move
  col_7var=c("orange","yellow", "cadetblue1", "blue1","darkgreen", "darkgoldenrod4","darkorchid1")
  legend_var=c("Var. Int.","Var. Res.","RCM","GCM","RCP","BC","HM")
  
  plt=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=inf,ymax=sup,fill=var),alpha=0.8)+
    geom_line(aes(x=Xfut,y=mean,color="black"))+
    scale_fill_discrete("Incertitude totale et\npartition de variance",type = col_7var[1:length(names_var)],labels=legend_var[1:length(names_var)])+
    scale_color_discrete("",type="black",label="Moyenne lissee")+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits = c(min(data$Xfut),max(data$Xfut)),expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    scale_y_continuous(paste0("Changement moyen (",ind_unit,")"))+
    ggtitle(paste0("Changement moyen et partition de variance pour\nle predicteur ",pred_name," et l'indicateur ",ind_name," (",bv_name,")"))
  save.plot(plt,Filename = paste0("meanchange_variance_",ind_name,"_",pred_name,"_",bv_name),Folder = folder_out,Format = "jpeg")
}

##############################################################################
## Time series of variance partition
## returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## ind_unit the unit of the indicator
## pred_unit the unit of the predictor

plotQUALYPSOTotalVarianceDecomposition_ggplot=function(QUALYPSOOUT,pred_name,ind_name,bv_name,pred_unit,folder_out){
  
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
    cum.smooth = predict(loess(cum ~ Xfut))
    cum.smooth[cum.smooth < 0] = 0
    data[,i]=cum.smooth
  }
  data=data.frame(cbind(Xfut,data))
  data=gather(data,key = "var",value = "val",-c(Xfut))
  namesEff = QUALYPSOOUT$names
  names_var=c("InternalVar","ResidualVar",rev(namesEff))
  data$var=factor(data$var,levels=c(names_var))
  
  #These colors work only if the order of the effects does not move
  col_7var=c("orange","yellow", "cadetblue1", "blue1","darkgreen", "darkgoldenrod4","darkorchid1")
  legend_var=c("Var. Int.","Var. Res.","RCM","GCM","RCP","BC","HM")
  
  plt=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=0,ymax=val,fill=var),alpha=0.8)+
    scale_fill_discrete("Partition\nde la variance",type = col_7var[1:length(names_var)],labels=legend_var[1:length(names_var)])+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits = c(min(data$Xfut),max(data$Xfut)),expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    scale_y_continuous(paste0("Partition de variance"),limits = c(0,1.01),expand=c(0,0))+
    ggtitle(paste0("Partition de variance pour\nle predicteur ",pred_name," et l'indicateur ",ind_name," (",bv_name,")"))
  save.plot(plt,Filename = paste0("partition_variance_",ind_name,"_",pred_name,"_",bv_name),Folder = folder_out,Format = "jpeg")
}

##############################################################################
## Time series of one scenario + uncertainties
## returns a ggplot 2 object
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## nameEff name of effect as in QUALYPSOOUT
## nameScenario name of scenario as in nameEff
## plain_name_Scen plain language name of scenario for legend
## pred_name the plain language name of the predictor
## ind_name the plain language name of the indicator
## bv_name the plain language name of the watershed if needed
## ind_unit the unit of the indicator
## pred_unit the unit of the predictor

plotQUALYPSOTotalVarianceByScenario_ggplot=function(QUALYPSOOUT,nameEff, nameScenario,plain_name_Scen,pred_name,ind_name,bv_name,ind_unit,pred_unit,folder_out){
  
  Xfut = QUALYPSOOUT$Xfut
  nFut = length(Xfut)
  iEff = which(QUALYPSOOUT$namesEff == nameEff)
  if (length(iEff) == 0){stop("wrong value for nameEff")}
  iScenario = which(QUALYPSOOUT$listScenarioInput$listEff[[iEff]] == 
                      nameScenario)
  meanPred = QUALYPSOOUT$CHANGEBYEFFECT[[nameEff]]$MEAN[,iScenario]
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
  limIntInf[1, ] = predict(loess(binf ~ Xfut))
  limIntSup[1, ] = predict(loess(bsup ~ Xfut))
  for (i in 1:(nEff + 1)) {
    binfi = limIntInf[i, ] + vNormRev[i, ] * (meanPred - 
                                                binf)
    limIntInf[i + 1, ] = predict(loess(binfi ~ Xfut))
    bsupi = limIntSup[i, ] - vNormRev[i, ] * (bsup - meanPred)
    limIntSup[i + 1, ] = predict(loess(bsupi ~ Xfut))
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
  
  #These colors work only if the order and the name of the effects does not move
  col_7var=c("orange","yellow", "cadetblue1", "blue1","darkgreen", "darkgoldenrod4","darkorchid1")
  legend_var=c("Var. Int.","Var. Res.","RCM","GCM","RCP","BC","HM")
  legend_var_full=c("int","res","rcm","gcm","rcp","bc","hm")
  legend_var=legend_var[-which(legend_var_full==nameEff)]
  col_7var=col_7var[-which(legend_var_full==nameEff)]
  
  plt=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=inf,ymax=sup,fill=var),alpha=0.8)+
    geom_line(aes(x=Xfut,y=mean,color="black"))+
    scale_fill_discrete("Incertitude totale et\npartition de variance",type = col_7var[1:length(names_var)],labels=legend_var[1:length(names_var)])+
    scale_color_discrete("",type="black",label="Moyenne lissee")+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits = c(min(data$Xfut),max(data$Xfut)),expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    scale_y_continuous(paste0("Changement moyen (",ind_unit,")"))+
    ggtitle(paste0("Changement et partition de variance pour les experiences ",plain_name_Scen,"\nle predicteur ",pred_name," et l'indicateur ",ind_name," (",bv_name,")"))
  save.plot(plt,Filename = paste0("change_variance_",nameScenario,"_",ind_name,"_",pred_name,"_",bv_name),Folder = folder_out,Format = "jpeg")
}