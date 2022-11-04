# Alix Reverdy
# Explore 2
# general data science function and data representations

library(svglite) #save svg
library(ggplot2) #plots
library(ggrepel)# labels
library(ggpubr) #ggarrange
library(ggpattern) #pattern fill
# library(ggforce) #geom_circle
library(ncdf4) #netcdf
library(tictoc) #runtime
library(lubridate) #date management
# git submodule update --remote --merge # in terminal
# install.packages(paste0(getwd(),"/QUALYPSO/"), repos = NULL, type="source")
library(QUALYPSO) #Qualypso
library(parallel) #detectCores
library(foreach) #parallelization
library(doParallel) #parallelization
library(zoo) #rollmean
library(pals)#colors
library(maptools)#wrld_simpl
library(openxlsx) #handle xlsx
library(rgdal) #readogr ...
library(dplyr) #left_join
library(raster)# CRS management
library(scales)#squish
library(ggformula)#geom_spline
library(ggnewscale) #new_scale
library(tidyr)#pivot_longer
library(plotly)#interactive plot
library(htmlwidgets)#save interactive plot


#############################################################
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



#########################################
## CMYK to RGB colors
#' assumes integer input for CMYK
#' 
cmyk <- function(C,M,Y,K) {
  C <- C / 100.0
  M <- M / 100.0
  Y <- Y / 100.0
  K <- K / 100.0
  n.c <- (C * (1-K) + K)
  n.m <- (M * (1-K) + K)  
  n.y <- (Y * (1-K) + K)
  r.col <- ceiling(255 * (1-n.c))
  g.col <- ceiling(255 * (1-n.m))
  b.col <- ceiling(255 * (1-n.y))
  tmp=col2rgb(sprintf("#%02s%02s%02s",
                      as.hexmode(r.col), 
                      as.hexmode(g.col), 
                      as.hexmode(b.col)))
  tmp=as.vector(tmp)
  return(rgb(tmp[1],tmp[2],tmp[3],maxColorValue=255))
  
}

##########################################
##IPCC colors and others

#For continuous variable requiring good distinction
ipcc_yelblue_5=c(rgb(255,255,204,maxColorValue=255),rgb(161,218,180,maxColorValue=255),rgb(65,182,196,maxColorValue=255),rgb(44,127,184,maxColorValue=255),rgb(37,52,148,maxColorValue=255))

#For Precipitation
precip_11=c(rgb(84,48,5,maxColorValue=255),rgb(140,81,10,maxColorValue=255),rgb(191,129,45,maxColorValue=255),rgb(223,194,125,maxColorValue=255),rgb(246,232,195,maxColorValue=255),rgb(245,245,245,maxColorValue=255),rgb(199,234,229,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(53,151,143,maxColorValue=255),rgb(1,102,94,maxColorValue=255),rgb(0,60,48,maxColorValue=255))
precip_10=c(rgb(84,48,5,maxColorValue=255),rgb(140,81,10,maxColorValue=255),rgb(191,129,45,maxColorValue=255),rgb(223,194,125,maxColorValue=255),rgb(246,232,195,maxColorValue=255),rgb(199,234,229,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(53,151,143,maxColorValue=255),rgb(1,102,94,maxColorValue=255),rgb(0,60,48,maxColorValue=255))
precip_9=c(rgb(140,81,10,maxColorValue=255),rgb(191,129,45,maxColorValue=255),rgb(223,194,125,maxColorValue=255),rgb(246,232,195,maxColorValue=255),rgb(245,245,245,maxColorValue=255),rgb(199,234,229,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(53,151,143,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_8=c(rgb(140,81,10,maxColorValue=255),rgb(191,129,45,maxColorValue=255),rgb(223,194,125,maxColorValue=255),rgb(246,232,195,maxColorValue=255),rgb(199,234,229,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(53,151,143,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_7=c(rgb(140,81,10,maxColorValue=255),rgb(216,179,101,maxColorValue=255),rgb(246,232,195,maxColorValue=255),rgb(245,245,245,maxColorValue=255),rgb(199,234,229,maxColorValue=255),rgb(90,180,172,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_6=c(rgb(140,81,10,maxColorValue=255),rgb(216,179,101,maxColorValue=255),rgb(246,232,195,maxColorValue=255),rgb(199,234,229,maxColorValue=255),rgb(90,180,172,maxColorValue=255),rgb(1,102,94,maxColorValue=255))
precip_5=c(rgb(166,97,26,maxColorValue=255),rgb(223,194,125,maxColorValue=255),rgb(245,245,245,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(1,133,113,maxColorValue=255))

#For température
temp_11=c(rgb(103,0,31,maxColorValue=255),rgb(178,24,43,maxColorValue=255),rgb(214,96,77,maxColorValue=255),cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),rgb(247,247,247,maxColorValue=255),rgb(209,229,240,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(67,147,195,maxColorValue=255),rgb(33,102,172,maxColorValue=255),rgb(5,48,97,maxColorValue=255))
temp_10=c(rgb(103,0,31,maxColorValue=255),rgb(178,24,43,maxColorValue=255),rgb(214,96,77,maxColorValue=255),cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),rgb(209,229,240,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(67,147,195,maxColorValue=255),rgb(33,102,172,maxColorValue=255),rgb(5,48,97,maxColorValue=255))
temp_9=c(rgb(178,24,43,maxColorValue=255),rgb(214,96,77,maxColorValue=255),cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),rgb(247,247,247,maxColorValue=255),rgb(209,229,240,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(67,147,195,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_8=c(rgb(178,24,43,maxColorValue=255),rgb(214,96,77,maxColorValue=255),cmyk(0,44,49,0),rgb(253,219,199,maxColorValue=255),rgb(209,229,240,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(67,147,195,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_7=c(rgb(178,24,43,maxColorValue=255),rgb(239,138,98,maxColorValue=255),rgb(253,219,199,maxColorValue=255),rgb(247,247,247,maxColorValue=255),rgb(209,229,240,maxColorValue=255),rgb(103,169,207,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_6=c(rgb(178,24,43,maxColorValue=255),rgb(239,138,98,maxColorValue=255),rgb(253,219,199,maxColorValue=255),rgb(209,229,240,maxColorValue=255),rgb(103,169,207,maxColorValue=255),rgb(33,102,172,maxColorValue=255))
temp_5=c(rgb(202,0,32,maxColorValue=255),rgb(244,165,130,maxColorValue=255),rgb(247,247,247,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(5,113,176,maxColorValue=255))
temp_5=c(rgb(202,0,32,maxColorValue=255),rgb(244,165,130,maxColorValue=255),rgb(247,247,247,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(5,113,176,maxColorValue=255))

#For rcp
col_3rcp=c(rgb(0,52,102,maxColorValue=255),rgb(112,160,205,maxColorValue=255),rgb(153,0,2,maxColorValue=255))
names(col_3rcp)=c("rcp2.6","rcp4.5","rcp8.5")
col_3rcp_shade=c(rgb(67,147,195,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(252,209,197,maxColorValue=255))
names(col_3rcp_shade)=c("rcp2.6","rcp4.5","rcp8.5")

#For line charts
ipcc_6col=c(rgb(0,0,0,maxColorValue=255),rgb(112,160,205,maxColorValue=255),rgb(196,121,0,maxColorValue=255),rgb(178,178,178,maxColorValue=255),rgb(0,52,102,maxColorValue=255),rgb(0,79,0,maxColorValue=255))


# for variance partition
col_7var=rev(viridis(7))
names(col_7var)=c("rcp","gcm","rcm","bc","hm","res","int")
legend_7var=c("RCP","GCM","RCM","BC","HM","Variabilité Résiduelle","Variabilité Interne")


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


###########################
#Cleanly load netcdf
load_nc<-function(path){
  nc<-nc_open(path)
  return(nc)
  nc_close(nc)
}

################################################
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


##############################################################################################
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
  # changes to something différent
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- c(which(y), n)
  
  structure(list(
    lengths = diff(c(0L, i)),
    values  = x[i]
  ), class = 'rle')
}






#############################################################
## Prepare specific climate response for QUALYPSO
## Y the nS x nY or nG x nS x nY indicator, can take in NA
## X the predictor (vector or same size as Y)
## Xref the reference value (or vector)
## typeChangeVariable "rel" or "abs"
## spar the spline smoothing in case of spline (vector of  size nS)
## type the type of smoothing applied: spline (classic but can pick each spar, log_spline (log transform, spline , then unlog)

## etaStar is by definition of X dimensions's and not Xfut and can contain NA (logical when thinking of tempreature predictor)

prepare_clim_resp=function(Y, X, Xref, Xfut, typeChangeVariable, spar,type){
  
  # dimensions
  d = dim(Y)
  if(length(d)==3){
    # Y is an array: GridPoints x Scenario x Time
    nG = d[1]
    nS = d[2]
    nY = d[3]
  }else{
    # Y is a matrix: Scenario x Time
    nS = nrow(Y)
    nY = ncol(Y)
  }
  if(is.vector(X)){
    if(nY!=length(X)){
      stop('if X is provided as a vector, its length must equal the number of columns of Y')
    }else{
      # repeat the vector to obtain a matrix
      Xmat = matrix(rep(X,nS),byrow=T,nrow=nS,ncol=nY)
    }
  }else if(is.matrix(X)){
    if(any(dim(X)!=dim(Y))){
      stop('if X is provided as a matrix, its size must match the size of Y')
    }else{
      Xmat = X
    }
  }else{
    stop('X must be a vector or a matrix')
  }
  # if Xref is provided, we check that is a single value within the values of X
  if(!any(length(Xref)==c(1,nS))|!is.numeric(Xref)){
    stop('Xref must be a single numeric value or a vector of length nS')
  }
  # recycle Xref if it is a single value
  if(length(Xref)==1){
    Xref = rep(Xref,nS)
  }
  # number of future time/global.tas
  nFut = length(Xfut)
  
  
  # prepare outputs
  phiStar = phi = matrix(nrow=nS,ncol=nFut)
  etaStar = YStar = matrix(nrow=nS,ncol=nY)
  climateResponse = list()
  for(iS in 1:nS){
    # projection for this simulation chain
    Ys = Y[iS,]
    Xs = Xmat[iS,]
    Xrefs = Xref[iS]
    # fit a smooth signal
    zz = !is.na(Ys)
    
    if(type=="spline"){
      smooth.spline.out<-stats::smooth.spline(Xs[zz],Ys[zz],spar=spar[iS])
      # store spline object
      climateResponse[[iS]] = smooth.spline.out
      # fitted responses at the points of the fit (for etaStar)
      phiY = predict(smooth.spline.out, Xs)$y
      # fitted responses at unknown points ("Xfut")
      phiS = predict(smooth.spline.out, Xfut)$y
      # climate response of the reference/control time/global tas
      phiC = predict(smooth.spline.out, Xrefs)$y
    }
    if(type=="log_spline"){
      Yslog10=log10(Ys)
      smooth.spline.out<-stats::smooth.spline(Xs[zz],Yslog10[zz],spar=spar[iS])
      # store spline object
      climateResponse[[iS]] = smooth.spline.out
      # fitted responses at the points of the fit (for etaStar)
      phiY = 10^predict(smooth.spline.out, Xs)$y
      # fitted responses at unknown points ("Xfut")
      phiS = 10^predict(smooth.spline.out, Xfut)$y
      # climate response of the reference/control time/global tas
      phiC = 10^predict(smooth.spline.out, Xrefs)$y
    }
    
    # store climate response for this simulation chain
    phi[iS,] = phiS
    # Climate change response: phiStar, and internal variability expressed as a change: etaStar
    if(typeChangeVariable=='abs'){
      # Eq. 5
      phiStar[iS,] = phiS-phiC
      etaStar[iS,] = Ys-phiY
      YStar[iS,] = Ys-phiC
    }else if(typeChangeVariable=='rel'){
      # Eq. 6
      phiStar[iS,] = phiS/phiC-1
      etaStar[iS,] = (Ys-phiY)/phiC
      YStar[iS,] = (Ys-phiC)/phiC
    }else{
      stop("fit.climate.response: argument type.change.var must be equal to 'abs' (absolute changes) or 'rel' (relative changes)")
    }
  }
  # Variance related to the internal variability: considered constant over the time period
  # (see Eq. 22 and 23 in Hingray and Said, 2014). We use a direct empirical estimate
  # of the variance of eta for each simulation chain and take the mean, see Eq. 19
  varInterVariability = mean(apply(etaStar,2,function(x) var(x)),na.rm=T)
  # return objects
  return(list(phiStar=phiStar,etaStar=etaStar,YStar=YStar,phi=phi,climateResponse=climateResponse,varInterVariability=varInterVariability))
  
}



#######################################################################################
## Takes a QUALYPSOOUT object and reconstructs chains from mean change +effects
## QUALYPSOOUT a Qualypso output
## line 1= mean+eff_rcp1+eff_gcm1+eff_rcm1...
## line 2= mean+eff_rcp2+eff_gcm1+eff_rcm1...
## line 3= mean+eff_rcp3+eff_gcm1+eff_rcm1...
## line 4= mean+eff_rcp1+eff_gcm2+eff_rcm1...
## line 5= mean+eff_rcp2+eff_gcm2+eff_rcm1...

reconstruct_chains=function(QUALYPSOOUT){
  neff=length(QUALYPSOOUT$namesEff)
  n_eachEff=vector()
  for(i in 1:neff){
    n_eachEff[i]=ncol(QUALYPSOOUT$MAINEFFECT[[i]]$MEAN)
  }
  chains=data.frame(do.call("rbind", replicate(prod(n_eachEff),QUALYPSOOUT$GRANDMEAN$MEAN, simplify = FALSE)))
  lst_eff=vector(mode = "list",length=neff)
  for (j in 1:length(QUALYPSOOUT$Xfut)){
    for(i in 1:neff){
      lst_eff[[i]]=QUALYPSOOUT$MAINEFFECT[[i]]$MEAN[j,]
    }
    chains[,j]=chains[,j]+rowSums(expand.grid(lst_eff))
  }
  return(chains)
}

###################################
## Format Global température for use in Qualypso, to be used inside code run_QUalypso
## spline calculated with data between 1860 and 1900
## Difference to 1860-1900 average and rcp/gcm matching + spline smoothing
## path_data the root of the path
##simu_lst the list of simulations
#first_data_year and last_data_year the first an last years with data for simu all year round

format_global_tas=function(path_data,first_data_year,last_data_year,simu_lst,first_ref_year,last_ref_year){
  
  vecYears=seq(1860,last_data_year,1)#1860 first year for HadGEM (1861 for GFDL but not used in QUALYPSO and before first_data_year so does not matter)
  ## Format global température: difference to 1860-1900 
  paths=list.files(paste0(path_data,"raw/Global_temp/"),pattern=glob2rx("global_tas*"),full.names = T)
  tas_glob_full=vector(length=length(paths),mode="list")
  for ( i in 1:length(paths)){
    tas_glob=read.csv(paths[i],skip=3,sep="",header=F)
    if(grepl("HadGEM2-ES",paths[i],fixed=T)){# -999 values in 1859
      tas_glob=tas_glob[-1,]
    }
    tas_glob=data.frame(year=tas_glob[,1],tas=apply(tas_glob[,-1],MARGIN = 1,mean))# mean of 12 months
    pre_indus_tas=mean(tas_glob$tas[tas_glob$year>=1860 & tas_glob$year<=1900])#because before no data for HadGEM
    tas_glob$tas=tas_glob$tas-pre_indus_tas
    colnames(tas_glob)[2]=paste0(strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][5],"_",strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][4])#rcp_gcm name
    tas_glob_full[[i]]=tas_glob[tas_glob$year<=2100,]
    tas_glob=tas_glob[tas_glob$year>=1860&tas_glob$year<=2100,]
    if(i==1){
      mat_Globaltas_gcm=tas_glob
    }else{
      mat_Globaltas_gcm=merge(mat_Globaltas_gcm,tas_glob,by="year")
    }
  }
  
  ## Format global température for Qualypso
  mat_Globaltas=vector(length=nrow(simu_lst),mode="list")
  vec_global_tas_gcm=unlist(lapply(colnames(mat_Globaltas_gcm)[-1],function(x) strsplit(x,"_")[[1]][2]))
  vec_global_tas_rcp=unlist(lapply(colnames(mat_Globaltas_gcm)[-1],function(x) strsplit(x,"_")[[1]][1]))
  for (i in 1:nrow(simu_lst)){
    mat_Globaltas[[i]]=tas_glob_full[[which(vec_global_tas_gcm==simu_lst[i,]$gcm & vec_global_tas_rcp==sub(".","",simu_lst[i,]$rcp,fixed=T))]]
  }
  mat_Globaltas=lapply(mat_Globaltas,function(x) cbind(x[,1],smooth.spline(x=x[,1],y=x[,2],spar = 1)$y))
  mat_Globaltas=lapply(mat_Globaltas,function(x) x[x[,1]>=1860&x[,1]<=2100,2])
  mat_Globaltas=t(do.call(cbind,mat_Globaltas))
  ref_Globaltas=apply(mat_Globaltas,MARGIN = 1,function(x) mean(x[which(vecYears==first_ref_year):which(vecYears==last_ref_year)]))
  idx=which(mat_Globaltas_gcm$year %in% seq(first_data_year,last_data_year))#often 1951-2099
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
## include RCP if RCP given then adds mean change of an RCP to effect, cannot be combined with includeMean=T or used if effect is already RCP
## plain_nameEff the plain language name of the effect
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the name of the indicator
## ind_name_full the plain language name of the indicator
## bv_name the name of the watershed
## bc_full_name plain language name of the watershed
## pre_unit the unit of the predictor
## folder_out the saving folder
## incert=TRUE adds confidence interval equivalent to Vtot-Vint

plotQUALYPSOeffect_ggplot=function(QUALYPSOOUT,nameEff,includeMean=FALSE,includeRCP=NULL,plain_nameEff,pred,pred_name,ind_name,ind_name_full,bv_name,bv_full_name,pred_unit,folder_out,xlim,var="toto"){
  
  Xfut = QUALYPSOOUT$Xfut
  iEff = which(QUALYPSOOUT$namesEff == nameEff)
  nEff=QUALYPSOOUT$listScenarioInput$nTypeEff[iEff]
  if (length(iEff) == 0){    stop("wrong value for nameEff")} 
  if (includeMean) {
    EffHat = QUALYPSOOUT$CHANGEBYEFFECT[[nameEff]]
    ylims=c(min(unlist(QUALYPSOOUT$CHANGEBYEFFECT)),max(unlist(QUALYPSOOUT$CHANGEBYEFFECT)))
  }  else {
    EffHat = QUALYPSOOUT$MAINEFFECT[[nameEff]]
    ylims=c(min(unlist(QUALYPSOOUT$MAINEFFECT)),max(unlist(QUALYPSOOUT$MAINEFFECT)))
  }
  if(!is.null(includeRCP)){
    ircp=which(QUALYPSOOUT$namesEff == "rcp")
    i_thisrcp=which(QUALYPSOOUT$listScenarioInput$listEff[[ircp]]==includeRCP)
    EffHat$MEAN = apply(EffHat$MEAN,MARGIN=2,function(x) x+QUALYPSOOUT$CHANGEBYEFFECT$rcp$MEAN[,i_thisrcp])
    ref_mean=QUALYPSOOUT$CHANGEBYEFFECT$rcp$MEAN[,i_thisrcp]
    ylims[1]=ylims[1]+min(ref_mean)
    ylims[2]=ylims[2]+max(ref_mean)
  }
  EffhanEff = dim(EffHat$MEAN)[2]
  meanRaw = EffHat$MEAN
  meanPred =meanRaw
  if(var!="tasAdjust"){
    med=data.frame(meanPred)*100
  }else{
    med=data.frame(meanPred)
  }
  
  colnames(med)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  med$pred=Xfut
  med=pivot_longer(data=med,cols=!pred,names_to = "eff",values_to = "med")
  data=med
  
  color_select=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  if(substr(color_select[1],5,5)!="."){
    for (cs in 1:length(color_select)){
      color_select[cs]=paste(c(substr(color_select[cs], 1, 4), substr(color_select[cs], 5,nchar(color_select[cs]))), collapse=".")#rcp26 to rcp2.6
      
    }
  }
  
  plt=ggplot(data)+
    geom_line(aes(x=pred,y=med,group=eff,color=eff),size=1.2)+
    scale_x_continuous(paste0(pred_name," (",pred_unit,")"),limits=xlim,expand=c(0,0))+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))
  
  if(colnames(QUALYPSOOUT$listScenarioInput$scenAvail)[iEff]=="rcp"){
    plt=plt+
      scale_color_discrete("",type = as.vector(col_3rcp[color_select]),labels=labels_rcp[which(names(col_3rcp)%in%color_select)])+
      scale_fill_discrete("",type= as.vector(col_3rcp[color_select]),labels=labels_rcp[which(names(col_3rcp)%in%color_select)])
  }else{
    plt=plt+
      scale_color_discrete("",type = viridis(nEff))+
      scale_fill_discrete("",type=viridis(nEff))
  }
  
  if(includeMean){
    if(var!="tasAdjust"){
      plt=plt+
        scale_y_continuous(paste0("Changement relatif moyen (%)"),limits = ylims*100)+
        ggtitle(paste0("Changements des ",plain_nameEff," pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full," (",bv_full_name,")"))
    }else{
      plt=plt+
        scale_y_continuous(paste0("Changement moyen (°C)"),limits = ylims)+
        ggtitle(paste0("Changements des ",plain_nameEff," pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full," (",bv_full_name,")"))
    }
    if (is.na(folder_out)){
      return(plt)
    }else{
      save.plot(plt,Filename = paste0("change_",ind_name,"_",nameEff,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
    }
  }else{
    if(is.null(includeRCP)){
      if(var!="tasAdjust"){
        plt=plt+
          scale_y_continuous(paste0("Effet principal (%)"),limits = ylims*100)+
          ggtitle(paste0("Effets principaux des ",plain_nameEff," pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full," (",bv_full_name,")"))
      }else{
        plt=plt+
          scale_y_continuous(paste0("Effet principal (°C)"),limits = ylims)+
          ggtitle(paste0("Effets principaux des ",plain_nameEff," pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full," (",bv_full_name,")"))
      }
      if (is.na(folder_out)){
        return(plt)
      }else{
        save.plot(plt,Filename = paste0("effect_",ind_name,"_",nameEff,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
      }
    }else{
      if(var!="tasAdjust"){
        plt=plt+
          scale_y_continuous(paste0("Changement relatif moyen (%)"),limits = ylims*100)+
          geom_line(data=data.frame(Xfut,ref_mean),aes(x=Xfut,y=ref_mean*100,linetype="dashed"),size=1.2)+
          scale_linetype_manual("",values=c("dashed"="dashed"),labels="Moyennne\nd'ensemble\n du RCP")+
          theme(legend.key.width = unit(1.5,"cm"))+
          ggtitle(paste0("Changements des des ",plain_nameEff," pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full," et le ",includeRCP,"\n(",bv_full_name,")"))
      }else{
        plt=plt+
          scale_y_continuous(paste0("Changement moyen (°C)"),limits = ylims)+
          geom_line(data=data.frame(Xfut,ref_mean),aes(x=Xfut,y=ref_mean,linetype="dashed"),size=1.2)+
          scale_linetype_manual("",values=c("dashed"="dashed"),labels="Moyennne\nd'ensemble\n du RCP")+
          theme(legend.key.width = unit(1.5,"cm"))+
          ggtitle(paste0("Changements des des ",plain_nameEff," pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full," et le ",includeRCP,"\n(",bv_full_name,")"))
      }
      if (is.na(folder_out)){
        return(plt)
      }else{
        save.plot(plt,Filename = paste0("change_",ind_name,"_",nameEff,"_",pred,"_",includeRCP,"_",bv_name),Folder = folder_out,Format = "jpeg")
      }
    }
  }
  
}

#############################################################################
## Make the summarize plot of a time series
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the of the indicator
## ind_name_full the plain language name of the indicator
## bv_name the name of the watershed
## bc_full_name plain language name of the watershed
## pred_unit the unit of the predictor
## folder_out the saving folder
## xlim the starting value (often 1990 or 0.7°C)


plotQUALYPSO_summary_change=function(QUALYPSOOUT,pred,pred_name,ind_name,ind_name_full,bv_name,bv_full_name,pred_unit,folder_out,xlim,var="toto",indic="titi",simpler=F,idx_pix="tata",idx_row="tata",idx_col="tata"){
  
  scenAvail=QUALYPSOOUT$listScenarioInput$scenAvail
  Xfut = QUALYPSOOUT$Xfut
  probCI = QUALYPSOOUT$listOption$probCI
  
  iEff = which(QUALYPSOOUT$namesEff == "rcp")
  EffHat = QUALYPSOOUT$CHANGEBYEFFECT[["rcp"]]
  nEff = dim(EffHat$MEAN)[2]
  meanRaw = EffHat$MEAN
  meanPred =meanRaw
  if(var!="tasAdjust"){
    med=data.frame(meanPred)*100
  }else{
    med=data.frame(meanPred)
  }
  colnames(med)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  med$pred=Xfut
  med=pivot_longer(data=med,cols=!pred,names_to = "eff",values_to = "med")
  
  Binf=NULL
  Bsup=NULL
  for (r in 1:nEff){
    idx_rcp=which(scenAvail$rcp==QUALYPSOOUT$listScenarioInput$listEff[[iEff]][r])
    # phiStar
    phiStar = QUALYPSOOUT$CLIMATEESPONSE$phiStar[idx_rcp,]
    #Reconstructed chains
    chains=reconstruct_chains(QUALYPSOOUT)
    #Replace for this rcp the reconstructed values by the true values
    idx_phistar_rcp=which(QUALYPSOOUT$listScenarioInput$scenComp==QUALYPSOOUT$listScenarioInput$listEff[[iEff]][r]&!QUALYPSOOUT$listScenarioInput$isMissing)
    chains[idx_phistar_rcp,]=phiStar
    chains=chains[seq(r,nrow(chains),3),]#only this rcp
    # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
    # obtained from QUALYPSO
    sd.qua = sqrt(QUALYPSOOUT$TOTALVAR-QUALYPSOOUT$INTERNALVAR-QUALYPSOOUT$EFFECTVAR[,iEff])
    sd.emp = apply(chains,2,sd)
    sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
    sd.corr = sd.qua/sd.emp
    phiStar.corr = chains*t(replicate(nrow(chains),sd.corr))
    # compute the lower bound if the distribution is gaussian
    binf = apply(phiStar.corr,2,quantile,probs = (1-probCI)/2)
    bsup = apply(phiStar.corr,2,quantile,probs = 0.5+probCI/2)
    if(var!="tasAdjust"){
      if(any(binf<(-1))){warning("Lower bound forced to -100%")}
      binf[binf<-1]=(-1)
      Binf=cbind(Binf,binf*100)
      Bsup=cbind(Bsup,bsup*100)
    }else{
      Binf=cbind(Binf,binf)
      Bsup=cbind(Bsup,bsup)
    }
  }
  
  
  Binf=data.frame(Binf)
  Bsup=data.frame(Bsup)
  colnames(Binf)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  colnames(Bsup)=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  Binf$pred=Xfut
  Bsup$pred=Xfut
  Binf=pivot_longer(data=Binf,cols=!pred,names_to = "eff",values_to = "binf")
  Bsup=pivot_longer(data=Bsup,cols=!pred,names_to = "eff",values_to = "bsup")
  data=merge(med,Binf,by=c("pred","eff"))
  data=merge(data,Bsup,by=c("pred","eff"))
  
  if(var!="tasAdjust"){
    chains=data.frame(as.vector(QUALYPSOOUT$Xmat),as.vector(QUALYPSOOUT$CLIMATEESPONSE$YStar)*100,rep(scenAvail$rcp,times=dim(QUALYPSOOUT$Xmat)[2]),rep(paste0(scenAvail$rcp,scenAvail$gcm,scenAvail$rcm),times=dim(QUALYPSOOUT$Xmat)[2]))
  }else{
    chains=data.frame(as.vector(QUALYPSOOUT$Xmat),as.vector(QUALYPSOOUT$CLIMATEESPONSE$YStar),rep(scenAvail$rcp,times=dim(QUALYPSOOUT$Xmat)[2]),rep(paste0(scenAvail$rcp,scenAvail$gcm,scenAvail$rcm),times=dim(QUALYPSOOUT$Xmat)[2]))
  }
  colnames(chains)=c("pred","val","eff","chain")
  data=data[data$pred>=xlim[1],]
  
  if(pred=="time"){
    chains=chains[chains$pred>=xlim[1] & chains$pred<=2098,]
    xlim2=c(1950,xlim[2])
    
    if(var=="tasAdjust"|var=="prtotAdjust"|var=="prsnAdjust"|var=="evspsblpotAdjust"){
      pth_tmp=list.files(paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/safran/indic/",var,"/"),full.names=T,pattern=glob2rx(paste0(var,"*",indic,"*")))
      nc=load_nc(pth_tmp)
      if(var=="tasAdjust"|var=="prtotAdjust"|var=="prsnAdjust"){
        full_years=nc$dim$Time$vals
        full_years=year(as.Date(full_years,origin="1958-07-31"))
        if(var=="tasAdjust"){
          res=ncvar_get(nc,varid="Tair")
        }else if(var=="prtotAdjust"){
          res=ncvar_get(nc,varid="Rain")
        }else if(var=="prsnAdjust"){
          res=ncvar_get(nc,varid="Snow")
        }
        fake_obs=res[idx_pix,]
      }
      if(var=="evspsblpotAdjust"){
        full_years=nc$dim$time$vals
        full_years=year(as.Date(full_years,origin="1975-01-01"))
        res=ncvar_get(nc,varid="etp")
        fake_obs=res[idx_row,idx_col,]
      }
      rm(nc)
      gc()
      if(var!="tasAdjust"){
        fake_obs=data.frame(full_years,(fake_obs-fake_obs[full_years==1990])/fake_obs[full_years==1990]*100)
      }else{
        fake_obs=data.frame(full_years,fake_obs-fake_obs[full_years==1990])
      }
    }else{
      fake_obs=data.frame(QUALYPSOOUT$Xmat[2,],QUALYPSOOUT$CLIMATEESPONSE$YStar[2,]*100)
    }
    colnames(fake_obs)=c("pred","val")
    fake_obs$trend=smooth.spline(x=fake_obs$pred,y=fake_obs$val,spar = QUALYPSOOUT$listOption$spar)$y
    #fake_obs$trend=as.vector(predict(lm(fake_obs$val~fake_obs$pred),data.frame(fake_obs$pred)))*-0.7-2
  }
  
  if(!simpler){
    if(pred=="time"){
      idx_shrtlst=c(1,4,8,12,15,19,23,25,27,29,30,34,36,38,40,41,45,49,52,57,58,64,68,71,76,77)
    }
    if(var!="tasAdjust"){
      shortlist=data.frame(t(QUALYPSOOUT$CLIMATEESPONSE$phiStar[idx_shrtlst,]))*100
    }else{
      shortlist=data.frame(t(QUALYPSOOUT$CLIMATEESPONSE$phiStar[idx_shrtlst,]))
    }
    colnames(shortlist)=paste0(scenAvail$rcp[idx_shrtlst],"/",scenAvail$gcm[idx_shrtlst],"/",scenAvail$rcm[idx_shrtlst],"/",scenAvail$bc[idx_shrtlst])
    shortlist$pred=Xfut
    shortlist=shortlist[shortlist$pred>=xlim[1],]
    shortlist=pivot_longer(shortlist,cols=!pred,names_to="chain",values_to = "val")
    shortlist$rcp=unlist(lapply(strsplit(shortlist$chain,"/"),function(x) x[1]))
  }
  
  color_select=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  if(substr(color_select[1],5,5)!="."){
    for (cs in 1:length(color_select)){
      color_select[cs]=paste(c(substr(color_select[cs], 1, 4), substr(color_select[cs], 5,nchar(color_select[cs]))), collapse=".")#rcp26 to rcp2.6
    }
  }
  
  if(!simpler){
    plt1=ggplot(data)+
      geom_line(data=chains,aes(x=pred,y=val,group=chain,color=eff,size="aa"),alpha=0.4)+#raw chains
      geom_ribbon(aes(x=pred,ymin=binf,ymax=bsup,fill=eff),alpha=0.3)+#uncertainty band
      geom_line(aes(x=pred,y=med,group=eff,color=eff,size="cc"))+#RCP mean
      geom_line(data=shortlist,aes(x=pred,y=val,group=chain,size="bb",color=rcp))+#shortlist
      scale_x_continuous("",limits=xlim2,expand=c(0,0))+
      scale_fill_discrete("Incertitude liée aux modèles\n(intervalle 5-95%)",type= as.vector(col_3rcp[color_select]),labels=NULL)+
      guides(fill=guide_legend(order=2,nrow=1, byrow=TRUE,title.theme=element_text(size = 13)))+
      scale_size_manual("",values=c("aa"=0.15,"bb"=0.5,"cc"=1.5),labels=c('Ensemble de projections "brutes"','Réponse en changement\ndes chaînes de référence (shortlist)',"Réponse moyenne\nen changement"))+
      guides(size = guide_legend(order=3,byrow = TRUE,override.aes = list(size=c(0.15,1,2))))+#byrow necessary for option of spacing between lines
      theme_bw(base_size = 16)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
      theme(legend.key.width = unit(1.5,"cm"))+
      theme(legend.title = element_text(size=13))+
      theme( legend.margin = margin(-2, 0, -2, 0))+
      annotate("text",  x=-Inf, y = Inf, label = "atop(bold(a))", vjust=1, hjust=-2,parse=T,size=8)
    if(var!="tasAdjust"){
      plt1=plt1+
        scale_y_continuous(paste0("Changement relatif moyen (%)"),expand=c(0,0))
    }else{
      plt1=plt1+
        scale_y_continuous(paste0("Changement moyen (°C)"),expand=c(0,0))
    }
    if(pred=="time"){
      plt1=plt1+
        geom_line(data=fake_obs,aes(x=pred,y=val,size="aa"),alpha=0.7,color="gray50")+#raw obs
        geom_line(data=fake_obs,aes(x=pred,y=trend,color="gray50",size="cc"))+#obs trend
        scale_color_discrete("",type = c("gray50", as.vector(col_3rcp[color_select])),labels=c("Observations",labels_rcp[which(names(col_3rcp)%in%color_select)]))+
        guides(color = guide_legend(order=1,override.aes = list(size = 1.2)))
    }
    
  }else{
    chain_band=aggregate(chains$val,by=list(chains$pred,chains$eff),max)
    colnames(chain_band)=c('pred',"eff","max")
    chain_band$min=aggregate(chains$val,by=list(chains$pred,chains$eff),min)[,3]
    a_label=data.frame(lab="a",eff="rcp26")
    plt1=ggplot(data)+
      geom_ribbon(data=chain_band,aes(x=pred,ymin=min,ymax=max,fill=eff),alpha=0.3)+#raw chain band
      scale_fill_discrete("Dispersion des chaînes\nde modélisation",type= as.vector(col_3rcp_shade[color_select]),labels=NULL)+
      guides(fill=guide_legend(order=2,nrow=1, byrow=TRUE,title.theme=element_text(size = 13)))+
      new_scale_fill()+
      geom_ribbon(aes(x=pred,ymin=binf,ymax=bsup,fill=eff),alpha=0.5)+#uncertainty band
      scale_fill_discrete("Incertitude liée aux modèles\n(intervalle 5-95%)",type= as.vector(col_3rcp[color_select]),labels=NULL)+
      guides(fill=guide_legend(order=2,nrow=1, byrow=TRUE,title.theme=element_text(size = 13)))+
      geom_line(aes(x=pred,y=med,group=eff,color=eff),size=1.5,lty="21")+#RCP mean
      scale_x_continuous("",limits=xlim2,expand=c(0,0))+
      scale_color_discrete("Moyenne d'ensemble",type= as.vector(col_3rcp[color_select]),labels=labels_rcp)+
      theme_bw(base_size = 16)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
      theme(legend.key.width = unit(1.5,"cm"))+
      theme(legend.title = element_text(size=13))+
      theme( legend.margin = margin(-2, 0, -2, 0))+
      facet_wrap(vars(eff),nrow = length(unique(data$eff)))+
      theme(strip.background = element_blank(),strip.text.x = element_blank())+
      geom_text(data=a_label,aes(x=-Inf, y = Inf, label = "a"), vjust=1, hjust=-2,parse=T,size=12)
    if(var!="tasAdjust"){
      plt1=plt1+
        scale_y_continuous(paste0("Changement relatif moyen (%)"),expand=c(0,0))
    }else{
      plt1=plt1+
        scale_y_continuous(paste0("Changement moyen (°C)"),expand=c(0,0))
    }
    if(pred=="time"){
      plt1=plt1+
        geom_line(data=fake_obs,aes(x=pred,y=val,size="aa"),alpha=0.7,color="gray50")+#raw obs
        scale_size_manual("",values = c("aa"=1),labels=c("Observations"))
    }
  }
  
  
  
  
  
  perc_pos=vector(mode="list",length=nEff)
  for (r in 1:nEff){
    idx_rcp=which(scenAvail$rcp==QUALYPSOOUT$listScenarioInput$listEff[[iEff]][r])
    phiStar = QUALYPSOOUT$CLIMATEESPONSE$phiStar[idx_rcp,]
    phiStar=phiStar[,Xfut>xlim[1]]
    n_chain=dim(phiStar)[1]
    perc_pos[[r]]=apply(phiStar,MARGIN=2,function(x) sum(x>=0)/n_chain*100)
  }
  data=data.frame(Xfut[Xfut>xlim[1]],do.call("cbind",perc_pos))
  colnames(data)=c("pred",QUALYPSOOUT$listScenarioInput$listEff[[iEff]])
  data=pivot_longer(data,cols=!pred,names_to = "rcp",values_to = "val")
  if(pred=="time"){
    rcp.labs <- c("RCP 2.6", "RCP 4.5", "RCP 8.5")
  }
  
  plt2=ggplot(data)+
    geom_point(aes(x=pred,y=factor(rcp,levels=rev(c("rcp26","rcp45","rcp85"))),fill=val),color="black",size=5,shape=21)+
    binned_scale(aesthetics = "fill",scale_name = "toto",name="Accord entre les chaînes sur\nle signe de la tendance",ggplot2:::binned_pal(scales::manual_pal(precip_5)),guide="coloursteps",show.limits = T,oob=squish,limits=c(0,100),breaks=c(20,40,60,80),labels=~ if(length(.x) == 2) {c("- à 100%","+ à 100%")} else {c("- à 80%","- à 60%","+ à 60%","+ à 80%")})+#that way because stepsn deforms colors
    scale_y_discrete("",labels=rev(rcp.labs))+
    theme_bw(base_size = 16)+
    theme(legend.title = element_text(size=13))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),axis.ticks = element_blank(),axis.text.x = element_blank())+
    theme(axis.text.y = element_text(face="bold"))+
    theme(legend.key.height = unit(0.5,"cm"))+
    scale_x_continuous("",limits=xlim2,expand=c(0,0))+
    ylab("")+
    annotate("text",  x=-Inf, y = Inf, label = "atop(bold(b))", vjust=1, hjust=-2,parse=T,size=8)
  
  
  
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
    cum.smooth = cum
    cum.smooth[cum.smooth < 0] = 0
    cum.smooth[cum.smooth > 1] = 1
    data[,i]=cum.smooth
  }
  data=data.frame(cbind(Xfut,data))
  data=data[data$Xfut>=xlim[1],]
  namesEff = QUALYPSOOUT$names
  names_var=c(namesEff,"res","int")
  colnames(data)=c("Xfut",names_var)
  data=pivot_longer(data=data,cols=-c(Xfut),names_to = "var",values_to = "val")
  data$val=data$val*100 #percentage
  
  vec_color=col_7var[names(col_7var) %in% names_var]
  data$var=factor(data$var,levels=rev(names(vec_color)))
  labels_var=rev(legend_7var[names(col_7var) %in% names_var])
  
  plt3=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=0,ymax=val,fill=var,alpha=var))+
    geom_line(aes(x=Xfut,y=val,group=var),color="white",linetype="dashed",size=0.3)+
    scale_fill_discrete("",type = vec_color,labels=labels_var)+
    scale_alpha_manual("",values=c(0.2,0.4,1,1,1,1),labels=labels_var)+
    scale_x_continuous("",limits = xlim2,expand=c(0,0))+
    scale_y_continuous(paste0("Partition de variance (%)"),limits = c(0,100),expand=c(0,0))+
    theme_bw(base_size = 16)+
    theme(legend.title = element_text(size=13))+
    theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.border = element_blank(),axis.ticks.x = element_blank(),axis.text.x = element_blank(),axis.line.y = element_line(colour = "black"),axis.line.x=element_blank())+
    annotate("text",  x=-Inf, y = Inf, label = "atop(bold(c))", vjust=1, hjust=-2,parse=T,size=8)+
    theme(legend.position="right",legend.justification="left", legend.box.spacing = unit(0, "pt"))
  
  if(var!="tasAdjust"){
    plt=ggarrange(plt1,plt2,plt3,heights=c(2,0.6,1),nrow=3,ncol=1,align="v")
  }else{
    plt=ggarrange(plt1,plt3,heights=c(2,1),nrow=2,ncol=1,align="v")
  }
  
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("summary_change_",ind_name,"_",pred,"_",bv_name,"_simpler",simpler),Folder = folder_out,Format = "jpeg")
  }
  
}


#############################################################################
## Make boxplot of chain dispersion for an horizon and all RCP
## QUALYPSOOUT a Qualypso output object (effects order should be coherent with color coding inside this function)
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the of the indicator
## ind_name_full the plain language name of the indicator
## bv_name the name of the watershed
## bc_full_name plain language name of the watershed
## pred_unit the unit of the predictor
## folder_out the saving folder


plotQUALYPSO_boxplot_horiz_rcp=function(QUALYPSOOUT,pred,pred_name,ind_name,ind_name_full,bv_name,bv_full_name,pred_unit,folder_out,var="toto",indic="titi",horiz=c(2030,2050,2085)){
  
  scenAvail=QUALYPSOOUT$listScenarioInput$scenAvail
  Xfut = QUALYPSOOUT$Xfut
  iEff = which(QUALYPSOOUT$namesEff == "rcp")
  h=which(Xfut %in% horiz)
  
  phiStar.corr=list()
  for (r in 1:length(unique(scenAvail$rcp))){
    idx_rcp=which(scenAvail$rcp==QUALYPSOOUT$listScenarioInput$listEff[[iEff]][r])
    # phiStar
    phiStar = QUALYPSOOUT$CLIMATEESPONSE$phiStar[idx_rcp,h]
    #Reconstructed chains
    chains=reconstruct_chains(QUALYPSOOUT)[,h]
    #Replace for this rcp the reconstructed values by the true values
    idx_phistar_rcp=which(QUALYPSOOUT$listScenarioInput$scenComp==QUALYPSOOUT$listScenarioInput$listEff[[iEff]][r]&!QUALYPSOOUT$listScenarioInput$isMissing)
    chains[idx_phistar_rcp,]=phiStar
    chains=chains[seq(r,nrow(chains),3),]#only this rcp
    # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
    # obtained from QUALYPSO
    sd.emp = apply(chains,2,sd)
    sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
    sd.qua=sd.emp
    i0=1
    for (i in h){
      sd.qua[i0] = sqrt(QUALYPSOOUT$TOTALVAR[i]-QUALYPSOOUT$INTERNALVAR[i]-QUALYPSOOUT$EFFECTVAR[i,iEff])
      i0=i0+1
    }
    sd.corr = sd.qua/sd.emp
    phiStar.corr[[r]] = data.frame(chains*t(replicate(nrow(chains),sd.corr)))
    colnames(phiStar.corr[[r]])=paste0("h_",horiz)
    phiStar.corr[[r]]$rcp=unique(scenAvail$rcp)[r]
  }
  
  phiStar.corr=bind_rows(phiStar.corr)
  if(var!="tasAdjust"){
    phiStar.corr[,c(1:3)]=phiStar.corr[,c(1:3)]*100
  }
  phiStar.corr=pivot_longer(phiStar.corr,cols=-c(rcp),names_to = "horiz",values_to = "val")
  
  color_select=QUALYPSOOUT$listScenarioInput$listEff[[iEff]]
  if(substr(color_select[1],5,5)!="."){
    for (cs in 1:length(color_select)){
      color_select[cs]=paste(c(substr(color_select[cs], 1, 4), substr(color_select[cs], 5,nchar(color_select[cs]))), collapse=".")#rcp26 to rcp2.6
    }
  }
  
  plt=ggplot(phiStar.corr)+
    geom_boxplot(aes(x=horiz,y=val,fill=rcp),lwd=2,outlier.size=4)+
    scale_fill_discrete("",type=as.vector(col_3rcp_shade[color_select]),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
    scale_x_discrete("",labels = horiz)+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme(legend.text = element_text(size=12,face="bold"))+
    theme(strip.background = element_blank(),strip.text.x = element_blank(),legend.key.height =unit(5, "lines"))+
    ggtitle(paste0("Distribution de l'ensemble balancé pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full,"\n(",bv_full_name,", référence 1990)"))
  if(var!="tasAdjust"){
    plt=plt+
      scale_y_continuous(paste0("Changement relatif (%)"),expand=c(0,0))
  }else{
    plt=plt+
      scale_y_continuous(paste0("Changement (°C)"),expand=c(0,0))
  }
  
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("boxplot_",ind_name,"_",pred,"_",bv_name),Folder = folder_out,Format = "jpeg")
  }
  
}


########################################################
## read and fortify for ggplot plotting shp file

read_shp=function(path,wgs84_to_l2=F){
  shp=readOGR(path,encoding = "UTF-8") ## parameters allows reading
  if(wgs84_to_l2){
    crs_L2=crs("+init=epsg:27572")#Lambert2
    shp=spTransform(shp,crs_L2)
  }
  shp$id <- row.names(shp)
  shp_fort=fortify(shp)
  shp_fort <- left_join(shp_fort, shp@data, by="id")
}


##################################
## Basemaps

rotate <- function(x) apply(t(x), 2, rev)

path_river="C:/Users/reverdya/Documents/Docs/2_data/SIG/processed/CoursEau_idx1_wgs84.shp"
path_fr="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/IGN/contours_FR/gadm36_FRA_0.shp"
river=read_shp(path_river)
fr=read_shp(path_fr)
river_L2=read_shp(path_river,wgs84_to_l2 = T)
fr_L2=read_shp(path_fr,wgs84_to_l2 = T)

data(wrld_simpl)
options(warn=-1)
wrld <- fortify(wrld_simpl)
options(warn=0)

#####################################################################################################################################
## Make a ggplot2 base map of France with SIM2 outlets as dots and val_name the name of the column for the color scale, that can be customized afterwards
## Data has at least longitude in x, latitude in y and a numeric filling value in val_name


base_map_outlets=function(data,val_name,alpha_name=NULL){
  plt=ggplot(data=data)+
    geom_polygon(data=fr,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=0.1)+
    geom_path(data=river,aes(x=long,y=lat,group=group),colour="gray80",size=0.1)+
    coord_equal(ratio=111/78,xlim = c(-6, 9.75),ylim = c(41.25,52),expand=F)+## ratio of 1lat by 1long at 45N
    scale_x_continuous("")+
    scale_y_continuous("")+
    theme_bw(base_size = 10)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme(axis.ticks =element_blank(),axis.text = element_blank() )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank())+
    theme(strip.text = element_text(size = 12, face = "bold"))#+
  if(!is.null(alpha_name)){
    plt=plt+
      geom_point(aes(x=x,y=y,fill=get(val_name),alpha=get(alpha_name)),size=3,shape=21,stroke=0.1)
  }else{
    plt=plt+
      geom_point(aes(x=x,y=y,fill=get(val_name)),size=3,shape=21,stroke=0.1)
  }
  return(plt)
}

#####################################################################################################################################
## Make a ggplot2 base map of France with SAFRAN grid as pixels and val_name the name of the column for the color scale, that can be customized afterwards
## Data has at least longitude in x, latitude in y and a numeric filling value in val_name and index of pixel in idx


nc=load_nc("C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/SAFRAN_mask_France.nc")
load(file=paste0("C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-meteo/refs.Rdata"))
res=ncvar_get(nc,varid="mask")
mask_fr=as.vector(res)
mask_fr=mask_fr[as.vector(refs$mask)==1]


base_map_grid=function(data,val_name,pattern_name=NULL,facet_vert_name=NULL,facet_horizontal_name=NULL,threshold="<80%",exclude_horizontal=c("5%","95%")){
  n=nrow(data)
  n_rep=n/length(mask_fr)
  data$idx=data$idx*rep(mask_fr,n_rep)
  data=data[data$idx!=0,]
  if(!is.null(pattern_name)){
    data$sign_bin=data[,val_name]
    data$sign_bin[data[,pattern_name]==threshold]=0
    data$sign_bin[data$sign_bin!=0]=1
    data$sign_bin[data[,facet_horizontal_name] %in% exclude_horizontal]=1 
    for(i in unique(data[,facet_vert_name])){
      for(j in unique(data[,facet_horizontal_name])){
        data_raster=rasterFromXYZ(data[data[,facet_vert_name]==i&data[,facet_horizontal_name]==j,c("x","y","sign_bin")])
        data_polygon=rasterToPolygons(data_raster==1,dissolve=T)
        data_polygon$id <- row.names(data_polygon)
        data_polygon_fort=fortify(data_polygon)
        data_polygon_fort <- left_join(data_polygon_fort, data_polygon@data, by="id")
        data_polygon_fort[,facet_vert_name]=i
        data_polygon_fort[,facet_horizontal_name]=j
        if(i!=unique(data[,facet_vert_name])[1]|j!=unique(data[,facet_horizontal_name])[1]){
          mask_polygon=rbind(mask_polygon,data_polygon_fort)
        }else{
          mask_polygon=data_polygon_fort
        }
      }
    }
    mask_polygon[,facet_horizontal_name]=factor(mask_polygon[,facet_horizontal_name],levels=levels(data[,facet_horizontal_name]))
    plt=ggplot(data=data)+
      geom_tile(aes(x=x,y=y,fill=get(val_name)))+
      geom_polygon_pattern(data=mask_polygon,aes(x = long, y = lat,group=group,pattern_density=as.character(layer)),pattern_alpha=0.5,pattern_color="black",pattern_fill="white",pattern_size=0.6,color=NA,fill=NA,pattern="circle")+
      scale_pattern_density_discrete(range = c(0.4,0))
  }else{
    plt=ggplot(data=data)+
      geom_tile(aes(x=x,y=y,fill=get(val_name)))+
      geom_path(data=river_L2,aes(x=long,y=lat,group=group),colour="gray80",size=0.1,alpha=0.5)
  }
  plt=plt+
    geom_polygon(data=fr_L2,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=0.1)+
    scale_x_continuous("")+
    scale_y_continuous("")+
    theme_bw(base_size = 10)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.2))+
    theme(axis.ticks =element_blank(),axis.text = element_blank() )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank())+
    theme(strip.text = element_text(size = 12, face = "bold"))+
    guides(fill=guide_colorbar(barwidth = 2, barheight = 15,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
  return(plt)
}

#######################################################################
## Map of 3 quantiles by 3 RCP for one horizon
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## horiz a temporal or température horizon
## pred_name the plain language name of the predictor
## ind_name thenname of the indicator
## ind_name_full the plain language name of the indicator
## pred_unit the unit of the predictor
## folder_out the saving folder

map_3quant_3rcp_1horiz=function(lst.QUALYPSOOUT,horiz,pred_name,pred,pred_unit,ind_name,ind_name_full,folder_out,freq_col=0.99,pix=F,var="toto",nbcores=6){
  
  ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
  rcp_names=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]
  quant=c("5%","mean","95%")
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=sim_stations[,c("Lon","Lat")]
  }
  exut$idx=seq(1:nrow(exut))
  
  tmp=exut
  for (i in 1:8){
    exut=rbind(exut,tmp)
  }
  exut$rcp=rep(rcp_names,each=nrow(exut)/3)
  exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  exut$val=0
  exut$sign=0
  exut$sign_agree="<80%"
  
  
  cl <- makeCluster(nbcores) # create a cluster with n cores
  registerDoParallel(cl) # register the cluster
  
  chg.list = foreach(i =1:length(lst.QUALYPSOOUT), .export='reconstruct_chains') %dopar% { ## no .combine by default result is a list (and no need for .packages)
    
    #for (i in 1:length(lst.QUALYPSOOUT)){
    tmp=list()
    for(r in rcp_names){
      idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==horiz)
      ieff_this_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]]==r)
      chg_mean=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,ieff_this_rcp]
      probCI = lst.QUALYPSOOUT[[i]]$listOption$probCI
      
      idx_this_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail$rcp==r)
      # phiStar
      phiStar = lst.QUALYPSOOUT[[i]]$CLIMATEESPONSE$phiStar[idx_this_rcp,idx_Xfut]
      chains=reconstruct_chains(lst.QUALYPSOOUT[[i]])[,idx_Xfut]
      #Replace for this rcp the reconstructed values by the true values
      idx_phistar_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenComp==lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]][ieff_this_rcp]&!lst.QUALYPSOOUT[[i]]$listScenarioInput$isMissing)
      chains[idx_phistar_rcp]=phiStar
      chains=chains[seq(ieff_this_rcp,length(chains),3)]#only this rcp
      # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
      # obtained from QUALYPSO
      sd.qua = sqrt(lst.QUALYPSOOUT[[i]]$TOTALVAR[idx_Xfut]-lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut]-lst.QUALYPSOOUT[[i]]$EFFECTVAR[idx_Xfut,ieff_rcp])
      sd.emp = sd(chains)
      sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
      sd.corr = sd.qua/sd.emp
      phiStar.corr = phiStar*t(replicate(length(phiStar),sd.corr))
      chg_q5 = quantile(phiStar.corr,probs = (1-probCI)/2)
      chg_q95 = quantile(phiStar.corr,probs = 0.5+probCI/2)
      
      # exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[2]]=chg_mean*100#*100 for percentages
      # exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[1]]=chg_q5*100
      # exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[3]]=chg_q95*100
      # 
      resp=lst.QUALYPSOOUT[[i]]$CLIMATEESPONSE$phiStar[which(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail$rcp==r),idx_Xfut]*100
      # exut$sign[exut$idx==i & exut$rcp==r]=sum(resp>0)/length(resp)*100
      if(var!="tasAdjust"){
        if(any(chg_q5<(-1))){warning("Lower bound forced to -100%")}
        chg_q5[chg_q5<(-1)]=-1
        tmp[[as.character(r)]]=c(chg_mean*100,chg_q5*100,chg_q95*100,sum(resp>0)/length(resp)*100)
      }else{
        tmp[[as.character(r)]]=c(chg_mean,chg_q5,chg_q95,sum(resp>0)/length(resp)*100)
      }
    }
    tmp
  }
  stopCluster(cl) # shut down the cluster
  
  for(i in 1:length(lst.QUALYPSOOUT)){
    for(r in rcp_names){
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[2]]=chg.list[[i]][[as.character(r)]][1]
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[1]]=chg.list[[i]][[as.character(r)]][2]
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[3]]=chg.list[[i]][[as.character(r)]][3]
      exut$sign[exut$idx==i & exut$rcp==r ]=chg.list[[i]][[as.character(r)]][4]
    }
  }
  
  for(i in 1:nrow(exut)){
    if(exut$sign[i]>80|exut$sign[i]<20){
      exut$sign_agree[i]=">80%"
    }
  }
  
  
  exut$quant=factor(exut$quant,levels=quant)
  colnames(exut)=c("x","y","idx","rcp","quant","val","sign","sign_agree")
  
  rcp.labs <- c("RCP 2.6", "RCP 4.5", "RCP 8.5")
  names(rcp.labs) <- rcp_names
  quant.labs <- c("Q5 ensemble", "Moyenne ensemble", "Q95 ensemble")
  names(quant.labs) <- quant
  
  
  #Setting limits for color scale
  if(var!="tasAdjust"){
    q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
    q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg)
    lim_col=round(lim_col/25)*25#arrondi au 25 le plus proche
  }else{
    q99=quantile(exut$val,probs=freq_col)
    q01=quantile(exut$val,probs=(1-freq_col))
    lim_col=as.numeric(c(q01,q99))
    lim_col=round(lim_col)#arrondi au 1 le plus proche
    br=seq(lim_col[1],lim_col[2],length.out=11)
  }
  
  
  if(!pix){
    plt=base_map_outlets(data = exut[exut$sign_agree=="<80%",],val_name = "val",alpha_name = "sign_agree")+
      binned_scale(aesthetics = "fill",scale_name = "toto",name="Pas d'accord [%]",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+
      guides(fill = guide_bins(override.aes=list(alpha=0.2,size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
    plt$layers[[3]]$aes_params$alpha= 0.2
    plt=plt+
      new_scale_fill()+
      geom_point(data=exut[exut$sign_agree==">80%",],aes(x=x,y=y,fill=val),size=3,shape=21,stroke=0.1)+
      binned_scale(aesthetics = "fill",scale_name = "toto2",name="Accord [%]",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))+
      theme(legend.box = "horizontal")
    plt=plt+
      facet_grid(horiz ~ quant,labeller = labeller(horiz=horiz.labs, quant = quant.labs))+
      ggtitle(paste0("Changement relatif du ",ind_name_full," et son incertitude pour\ndifférents RCP et le prédicteur ",pred_name,"\n(Horizon ",horiz," avec référence 1990)"))+
      theme(panel.border = element_rect(colour = "black",fill=NA))
    plt$layers[[3]]$aes_params$size= 3
    plt$layers[[4]]$aes_params$size= 3
  }else{
    if(var!="tasAdjust"){
      plt=base_map_grid(data = exut,val_name = "val",pattern_name = "sign_agree",facet_vert_name ="rcp",threshold="<80%",facet_horizontal_name="quant",exclude_horizontal=c("5%","95%"))
      plt=plt+
        facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif du ",ind_name_full," et son incertitude pour\ndifférents RCP et le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        scale_pattern_density_discrete("Accord sur le\nsigne du changement",range = c(0.4,0),labels=c("<80%",">80%"))+
        theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20))
      
    }else{
      plt=base_map_grid(data = exut,val_name = "val")
      plt=plt+
        facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement du ",ind_name_full," et son incertitude pour\ndifférents RCP et le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
    }
    
  }
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("map_3rcp_3quant_",ind_name,"_",pred,"_",horiz),Folder = folder_out,Format = "jpeg")
  }
}


#######################################################################
##Map of 3 quantiles by 3 horizons for one RCP
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## horiz 3 temporal or température horizons
## pred the predictor name in the file
## pred_name the plain language name of the predictor
## ind_name the name of the indicator
## ind_name_full the plain language name of the indicator
## pred_unit the unit of the predictor
## rcp_name the name of the wanted rcp
## folder_out the saving folder

map_3quant_1rcp_3horiz=function(lst.QUALYPSOOUT,horiz,rcp_name, rcp_plainname,pred,pred_name,pred_unit,ind_name,ind_name_full,folder_out,freq_col=0.99,pix=F,var="toto",nbcores=6){
  
  ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
  ieff_this_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]==rcp_name)
  probCI=lst.QUALYPSOOUT[[1]]$listOption$probCI
  quant=c("5%","mean","95%")
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=sim_stations[,c("Lon","Lat")]
  }
  
  exut$idx=seq(1:nrow(exut))
  
  tmp=exut
  for (i in 1:8){
    exut=rbind(exut,tmp)
  }
  exut$horiz=rep(horiz,each=nrow(exut)/3)
  exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  exut$val=0
  exut$sign=0
  
  exut$sign_agree="<80%"
  
  
  cl <- makeCluster(nbcores) # create a cluster with n cores
  registerDoParallel(cl) # register the cluster
  
  chg.list = foreach(i =1:length(lst.QUALYPSOOUT), .export='reconstruct_chains') %dopar% { ## no .combine by default result is a list (and no need for .packages)
    
    # for (i in 1:length(lst.QUALYPSOOUT)){
    tmp=list()
    for(h in horiz){
      idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==h)
      chg_mean=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,ieff_this_rcp]
      
      idx_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail$rcp==rcp_name)
      # phiStar
      phiStar = lst.QUALYPSOOUT[[i]]$CLIMATEESPONSE$phiStar[idx_rcp,idx_Xfut]
      chains=reconstruct_chains(lst.QUALYPSOOUT[[i]])[,idx_Xfut]
      #Replace for this rcp the reconstructed values by the true values
      idx_phistar_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenComp==lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]][ieff_this_rcp]&!lst.QUALYPSOOUT[[i]]$listScenarioInput$isMissing)
      chains[idx_phistar_rcp]=phiStar
      chains=chains[seq(ieff_this_rcp,length(chains),3)]#only this rcp
      # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
      # obtained from QUALYPSO
      sd.qua = sqrt(lst.QUALYPSOOUT[[i]]$TOTALVAR[idx_Xfut]-lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut]-lst.QUALYPSOOUT[[i]]$EFFECTVAR[idx_Xfut,ieff_rcp])
      sd.emp = sd(chains)
      sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
      sd.corr = sd.qua/sd.emp
      phiStar.corr = chains*t(replicate(length(chains),sd.corr))
      chg_q5 = quantile(phiStar.corr,probs = (1-probCI)/2)
      chg_q95 = quantile(phiStar.corr,probs = 0.5+probCI/2)
      
      # exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[2]]=chg_mean*100#*100 for percentages
      # exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[1]]=chg_q5*100
      # exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[3]]=chg_q95*100
      
      resp=lst.QUALYPSOOUT[[i]]$CLIMATEESPONSE$phiStar[which(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail$rcp==rcp_name),idx_Xfut]*100
      # exut$sign[exut$idx==i & exut$horiz==h]=sum(resp>0)/length(resp)*100
      if(var!="tasAdjust"){
        if(any(chg_q5<(-1))){warning("Lower bound forced to -100%")}
        chg_q5[chg_q5<(-1)]=-1
        tmp[[as.character(h)]]=c(chg_mean*100,chg_q5*100,chg_q95*100,sum(resp>0)/length(resp)*100)
      }else{
        tmp[[as.character(h)]]=c(chg_mean,chg_q5,chg_q95,sum(resp>0)/length(resp)*100)
      }
    }
    tmp
  }
  stopCluster(cl) # shut down the cluster
  
  for(i in 1:length(lst.QUALYPSOOUT)){
    for(h in horiz){
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[2]]=chg.list[[i]][[as.character(h)]][1]
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[1]]=chg.list[[i]][[as.character(h)]][2]
      exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[3]]=chg.list[[i]][[as.character(h)]][3]
      exut$sign[exut$idx==i & exut$horiz==h ]=chg.list[[i]][[as.character(h)]][4]
    }
  }
  
  for(i in 1:nrow(exut)){
    if(exut$sign[i]>80|exut$sign[i]<20){
      exut$sign_agree[i]=">80%"
    }
  }
  
  
  exut$quant=factor(exut$quant,levels=quant)
  colnames(exut)=c("x","y","idx","horiz","quant","val","sign","sign_agree")
  
  horiz.labs <- paste0(horiz," ",pred_unit)
  names(horiz.labs) <- horiz
  quant.labs <- c("Q5 ensemble", "Moyenne ensemble", "Q95 ensemble")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  if(var!="tasAdjust"){
    q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
    q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg)
    lim_col=round(lim_col/25)*25#arrondi au 25 le plus proche
  }else{
    q99=quantile(exut$val,probs=freq_col)
    q01=quantile(exut$val,probs=(1-freq_col))
    lim_col=as.numeric(c(q01,q99))
    lim_col=round(lim_col)#arrondi au 1 le plus proche
    br=seq(lim_col[1],lim_col[2],length.out=11)
  }
  
  if(!pix){
    plt=base_map_outlets(data = exut[exut$sign_agree=="<80%",],val_name = "val",alpha_name = "sign_agree")+
      binned_scale(aesthetics = "fill",scale_name = "toto",name="Pas d'accord [%]",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+
      guides(fill = guide_bins(override.aes=list(alpha=0.2,size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
    plt$layers[[3]]$aes_params$alpha= 0.2
    plt=plt+
      new_scale_fill()+
      geom_point(data=exut[exut$sign_agree==">80%",],aes(x=x,y=y,fill=val),size=3,shape=21,stroke=0.1)+
      binned_scale(aesthetics = "fill",scale_name = "toto2",name="Accord [%]",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))+
      theme(legend.box = "horizontal")
    plt=plt+
      facet_grid(horiz ~ quant,labeller = labeller(horiz=horiz.labs, quant = quant.labs))+
      ggtitle(paste0("Changement relatif du ",ind_name_full," et son incertitude pour\ndifférents horizons et le prédicteur ",pred_name,"\n(",rcp_plainname," avec référence 1990)"))+
      theme(panel.border = element_rect(colour = "black",fill=NA))
    plt$layers[[3]]$aes_params$size= 3
    plt$layers[[4]]$aes_params$size= 3
  }else{
    if(var!="tasAdjust"){
      plt=base_map_grid(data = exut,val_name = "val",pattern_name="sign_agree",facet_vert_name ="horiz",threshold="<80%",facet_horizontal_name="quant",exclude_horizontal=c("5%","95%"))
      plt=plt+
        facet_grid(horiz ~ quant,labeller = labeller(horiz = horiz.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif du ",ind_name_full," et son incertitude pour\ndifférents horizons et le prédicteur ",pred_name,"\n(",rcp_plainname," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        scale_pattern_density_discrete("Accord sur le\nsigne du changement",range = c(0.4,0),labels=c("<80%",">80%"))+
        theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20))
    }else{
      plt=base_map_grid(data = exut,val_name = "val")
      plt=plt+
        facet_grid(horiz ~ quant,labeller = labeller(horiz = horiz.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement du ",ind_name_full," et son incertitude pour\ndifférents horizons et le prédicteur ",pred_name,"\n(",rcp_plainname," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
    }
  }
  
  
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("map_3quant_3horiz_",ind_name,"_",pred,"_",rcp_name),Folder = folder_out,Format = "jpeg")
  }
  
  
}



#############################################################################################
## Map of 3 quantiles by 3 RCP for one horizon of time using basic mean and q5/q95
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## horiz a temporal horizon
## ind_name the name of the indicator
## ind_name_full the plain language name of the indicator
## folder_out the saving folder


map_3quant_3rcp_1horiz_basic=function(lst.QUALYPSOOUT,horiz,ind_name,ind_name_full,folder_out,freq_col=0.99,pix=F,var="toto",ref0=1990){
  
  ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
  rcp_names=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]
  quant=c("5%","mean","95%")
  idx_Xfut=which(lst.QUALYPSOOUT[[1]]$Xfut==horiz)
  idx_ref0=which(lst.QUALYPSOOUT[[1]]$Xfut==ref0)
  
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=sim_stations[,c("Lon","Lat")]
  }
  exut$idx=seq(1:nrow(exut))
  
  tmp=exut
  for (i in 1:8){
    exut=rbind(exut,tmp)
  }
  exut$rcp=rep(rcp_names,each=nrow(exut)/3)
  exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  exut$val=0
  exut$sign=0
  exut$sign_agree="<80%"
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    for(r in rcp_names){
      
      idx_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail$rcp==r)
      ref0=apply(lst.QUALYPSOOUT[[i]]$Y[idx_rcp,(idx_ref0-15):(idx_ref0+15)],MARGIN=1,mean)
      
      if(var!="tasAdjust"){
        rel_chg=(apply(lst.QUALYPSOOUT[[i]]$Y[idx_rcp,(idx_Xfut-15):(idx_Xfut+15)],MARGIN=1,mean,na.rm=T)-ref0)/ref0*100
      }else{
        rel_chg=apply(lst.QUALYPSOOUT[[i]]$Y[idx_rcp,(idx_Xfut-15):(idx_Xfut+15)],MARGIN=1,mean,na.rm=T)-ref0
      }
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[2]]=mean(rel_chg)
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[1]]=quantile(rel_chg,probs=0.05)
      exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[3]]=quantile(rel_chg,probs=0.95)
      
      exut$sign[exut$idx==i & exut$rcp==r]=sum(rel_chg>0)/length(rel_chg)*100
    }
  }
  
  for(i in 1:nrow(exut)){
    if(exut$sign[i]>80|exut$sign[i]<20){
      exut$sign_agree[i]=">80%"
    }
  }
  
  exut$quant=factor(exut$quant,levels=quant)
  colnames(exut)=c("x","y","idx","rcp","quant","val","sign","sign_agree")
  
  rcp.labs <- c("RCP 2.6", "RCP 4.5", "RCP 8.5")
  names(rcp.labs) <- rcp_names
  quant.labs <- c("Q5 ensemble", "Moyenne ensemble", "Q95 ensemble")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  if(var!="tasAdjust"){
    q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
    q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg)
    lim_col=round(lim_col/25)*25#arrondi au 25 le plus proche
  }else{
<<<<<<< HEAD
    # q99=quantile(exut$val,probs=freq_col)
    # q01=quantile(exut$val,probs=(1-freq_col))
    # lim_col=as.numeric(c(q01,q99))
    # lim_col=round(lim_col)#arrondi au 1 le plus proche
    lim_col=c(1,5) #forced to allow comparison with QUALYPSO
=======
    q99=quantile(exut$val,probs=freq_col)
    q01=quantile(exut$val,probs=(1-freq_col))
    lim_col=as.numeric(c(q01,q99))
    lim_col=round(lim_col)#arrondi au 1 le plus proche
>>>>>>> 8085ee2d43c8be1e3f0fc13620b1985805c5e5ff
    br=seq(lim_col[1],lim_col[2],length.out=11)
  }
  
  if(!pix){
    plt=base_map_outlets(data = exut[exut$sign_agree=="<80%",],val_name = "val",alpha_name = "sign_agree")+
      binned_scale(aesthetics = "fill",scale_name = "toto",name="Pas d'accord [%]",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+
      guides(fill = guide_bins(override.aes=list(alpha=0.2,size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
    plt$layers[[3]]$aes_params$alpha= 0.2
    plt=plt+
      new_scale_fill()+
      geom_point(data=exut[exut$sign_agree==">80%",],aes(x=x,y=y,fill=val),size=3,shape=21,stroke=0.1)+
      binned_scale(aesthetics = "fill",scale_name = "toto2",name="Accord [%]",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))+
      theme(legend.box = "horizontal")
    plt=plt+
      facet_grid(horiz ~ quant,labeller = labeller(horiz=horiz.labs, quant = quant.labs))+
      ggtitle(paste0("Changement relatif du ",ind_name_full," et son incertitude pour\ndifférents horizons et le prédicteur ",pred_name,"\n(",rcp_plainname," avec référence 1990),\n ensemble non balancé"))+
      theme(panel.border = element_rect(colour = "black",fill=NA))
    plt$layers[[3]]$aes_params$size= 3
    plt$layers[[4]]$aes_params$size= 3
  }else{
    if(var!="tasAdjust"){
      plt=base_map_grid(data = exut,val_name = "val",pattern_name = "sign_agree",facet_vert_name ="rcp",threshold="<80%",facet_horizontal_name="quant",exclude_horizontal=c("5%","95%"))
      plt=plt+
        facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif du ",ind_name_full," et son incertitude pour\ndifférents RCP et le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990),\n ensemble non balancé"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        scale_pattern_density_discrete("Accord sur le\nsigne du changement",range = c(0.4,0),labels=c("<80%",">80%"))+
        theme(legend.key =element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20))
    }else{
      plt=base_map_grid(data = exut,val_name = "val")
      plt=plt+
        facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement du ",ind_name_full," et son incertitude pour\ndifférents RCP et le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990),\n ensemble non balancé"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
    }
  }
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("basic_meth_map_3rcp_3quant_",ind_name,"_time_",horiz),Folder = folder_out,Format = "jpeg")
  }
}





#######################################################################
## Map of effects GCM or RCM (température or time)
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## includeMean if true adds mean change to effect or if include RCP is non null adds RCP
## name_eff "gcm" ou "rcm"...
## pred_name the plain language name of the predictor
## pred the predictor name in the file
## ind_name the name of the indicator
## ind_name_full the plain language name of the indicator
## pred_unit the unit of the predictor
## folder_out the saving folder
## horiz he horizon of predictor wanted
## name_eff_plain plain langauge name of principal effects

map_main_effect=function(lst.QUALYPSOOUT,includeMean=FALSE,includeRCP=NULL,horiz,name_eff,name_eff_plain,pred,pred_name,pred_unit,ind_name,ind_name_full,folder_out,freq_col=0.99,pix=F,var="toto"){
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=sim_stations[,c("Lon","Lat")]
  }
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
    if(var!="tasAdjust"){
      if(includeMean){
        chg=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT[[name_eff]]$MEAN[idx_Xfut,]*100 #*100 for percentages
      }else{
        chg=lst.QUALYPSOOUT[[i]]$MAINEFFECT[[name_eff]]$MEAN[idx_Xfut,]*100 #*100 for percentages
        if(!is.null(includeRCP)){
          ircp=which(lst.QUALYPSOOUT[[i]]$namesEff == "rcp")
          i_thisrcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ircp]]==includeRCP)
          chg = chg+lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,i_thisrcp]*100
        }
      }
    }else{
      if(includeMean){
        chg=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT[[name_eff]]$MEAN[idx_Xfut,]
      }else{
        chg=lst.QUALYPSOOUT[[i]]$MAINEFFECT[[name_eff]]$MEAN[idx_Xfut,]
        if(!is.null(includeRCP)){
          ircp=which(lst.QUALYPSOOUT[[i]]$namesEff == "rcp")
          i_thisrcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ircp]]==includeRCP)
          chg = chg+lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,i_thisrcp]
        }
      }
    }
    
    
    for(j in 1:length(chg)){
      exut$val[exut$idx==i & exut$effs==j]=chg[j]
    }
  }
  
  colnames(exut)=c("x","y","idx","effs","val")
  
  #Setting limits for color scale
  if(includeMean){
    tmp=unlist(lapply(lst.QUALYPSOOUT,function(x) lapply(x$CHANGEBYEFFECT,function(xx) xx$MEAN[idx_Xfut,])))
  }else{
    tmp=unlist(lapply(lst.QUALYPSOOUT,function(x) lapply(x$MAINEFFECT,function(xx) xx$MEAN[idx_Xfut,])))
    if(!is.null(includeRCP)){
      tmp=unlist(lapply(lst.QUALYPSOOUT,function(x) unlist(lapply(x$MAINEFFECT,function(xx) xx$MEAN[idx_Xfut,]))+x$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,i_thisrcp]))
    }
  }
  if(var!="tasAdjust"){
    tmp=tmp*100
  }
  if(var!="tasAdjust"|(is.null(includeRCP)&includeMean==F)){
    q99pos=quantile(tmp[tmp>=0],probs=freq_col)
    q99neg=abs(quantile(tmp[tmp<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg)
    if(var!="tasAdjust"){
      lim_col=round(lim_col/10)*10#arrondi au 10 le plus proche
    }else{
      lim_col=round(lim_col/0.5)*0.5#arrondi au 0.5 le plus proche
    }
  }else{
    q99=quantile(exut$val,probs=freq_col)
    q01=quantile(exut$val,probs=(1-freq_col))
    lim_col=as.numeric(c(q01,q99))
    lim_col=round(lim_col)#arrondi au 1 le plus proche
    br=seq(lim_col[1],lim_col[2],0.5)
  }
  
  if(!pix){
    plt=base_map_outlets(data = exut,val_name = "val")
  }else{
    plt=base_map_grid(data = exut,val_name = "val")
  }
  
  if(includeMean){
    if(var!="tasAdjust"){
      plt=plt+
        facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
    }else{
      plt=plt+
        facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C))",ggplot2:::binned_pal(scales::brewer.ylorrd(length(br)-1)),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
    }
    if(!pix){
      plt$layers[[3]]$aes_params$size= 3
    }
    if (is.na(folder_out)){
      return(plt)
    }else{
      save.plot(plt,Filename = paste0("map_change_",name_eff,"_",ind_name,"_",pred,"_",horiz),Folder = folder_out,Format = "jpeg")
    }
    
  }else{
    if(is.null(includeRCP)){
<<<<<<< HEAD
      if(var!="tasAdjust"){
        plt=plt+
          facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Effet principal (%)",ggplot2:::binned_pal(scales::manual_pal(temp_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),oob=squish,show.limits = T)+#that way because stepsn deforms colors
          ggtitle(paste0("Effet principaux des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
          theme(panel.border = element_rect(colour = "black",fill=NA))+
          guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
      }else{
        plt=plt+
          facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Effet principal (%)",ggplot2:::binned_pal(scales::manual_pal(rev(temp_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),oob=squish,show.limits = T)+#that way because stepsn deforms colors
          ggtitle(paste0("Effet principaux des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
          theme(panel.border = element_rect(colour = "black",fill=NA))+
          guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
      }
=======
      plt=plt+
        facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Effet principal (%)",ggplot2:::binned_pal(scales::manual_pal(temp_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),oob=squish,show.limits = T)+#that way because stepsn deforms colors
        ggtitle(paste0("Effet principaux des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
>>>>>>> 8085ee2d43c8be1e3f0fc13620b1985805c5e5ff
      if(!pix){
        plt$layers[[3]]$aes_params$size= 3
      }
      if (is.na(folder_out)){
        return(plt)
      }else{
        save.plot(plt,Filename = paste0("map_effect_",name_eff,"_",ind_name,"_",pred,"_",horiz),Folder = folder_out,Format = "jpeg")
      }
      
    }else{
      if(var!="tasAdjust"){
        plt=plt+
          facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+#that way because stepsn deforms colors
          ggtitle(paste0("Changement des ",name_eff_plain,"s pour le ",ind_name_full," le ",includeRCP,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
          theme(panel.border = element_rect(colour = "black",fill=NA))+
          guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
      }else{
        plt=plt+
          facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (%)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
          ggtitle(paste0("Changement des ",name_eff_plain,"s pour le ",ind_name_full," le ",includeRCP,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
          theme(panel.border = element_rect(colour = "black",fill=NA))+
          guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
      }
      if(!pix){
        plt$layers[[3]]$aes_params$size= 3
      }
      if (is.na(folder_out)){
        return(plt)
      }else{
        save.plot(plt,Filename = paste0("map_change_",name_eff,"_",ind_name,"_",pred,"_",includeRCP,"_",horiz),Folder = folder_out,Format = "jpeg")
      }
      
    }
    
    
  }
}


############################################################################
## Map mean change or internal variability or total variability or res var
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## vartype the variable to be plotted (one of varint, mean, vartot)
## pred_name the plain language name of the predictor
## pred the predictor name in the file
## ind_name the name of the indicator
## ind_name_full the plain language name of the indicator
## pred_unit the unit of the predictor
## folder_out the saving folder
## horiz he horizon of predictor wanted
## name_eff_plain plain langauge name of principal effects

map_one_var=function(lst.QUALYPSOOUT,vartype,horiz,pred,pred_name,pred_unit,ind_name,ind_name_full,folder_out,freq_col=0.99,pix=F,var="toto"){
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=sim_stations[,c("Lon","Lat")]
  }
  exut$idx=seq(1:nrow(exut))
  exut$val=0
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==horiz)
    
    if(vartype=="mean"){
      if(var!="tasAdjust"){
        chg=lst.QUALYPSOOUT[[i]]$GRANDMEAN$MEAN[idx_Xfut]*100
      }else{
        chg=lst.QUALYPSOOUT[[i]]$GRANDMEAN$MEAN[idx_Xfut]
      }
    }
    if(vartype=="varint"){
      if(var!="tasAdjust"){
        chg=lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut]*100^2#because variance is square of standard deviation unit
      }else{
        chg=lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut]
      }
      chg=sqrt(chg)
    }
    if(vartype=="varres"){
      if(var!="tasAdjust"){
        chg=lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut]*100^2
      }else{
        chg=lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut]
      }
      chg=sqrt(chg)
    }
    if(vartype=="vartot"){
      Veff = lst.QUALYPSOOUT[[i]]$EFFECTVAR[idx_Xfut,]
      if(var!="tasAdjust"){
        chg = sum(Veff, lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut],lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut])*100^2
      }else{
        chg = sum(Veff, lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut],lst.QUALYPSOOUT[[i]]$INTERNALVAR[idx_Xfut])
      }
      chg=sqrt(chg)
    }
    if(vartype=="incert"){# sans IV
      Veff = lst.QUALYPSOOUT[[i]]$EFFECTVAR[idx_Xfut,]
      if(var!="tasAdjust"){
        chg = sum(Veff, lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut])*100^2
      }else{
        chg = sum(Veff, lst.QUALYPSOOUT[[i]]$RESIDUALVAR$MEAN[idx_Xfut])
      }
      chg=sqrt(chg)
    }
    if(vartype=="rcp8.5"|vartype=="rcp85"){
      ircp=which(lst.QUALYPSOOUT[[i]]$namesEff == "rcp")
      i_thisrcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ircp]]==vartype)
      if(var!="tasAdjust"){
        chg=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,i_thisrcp]*100
      }else{
        chg=lst.QUALYPSOOUT[[i]]$CHANGEBYEFFECT$rcp$MEAN[idx_Xfut,i_thisrcp]
      }
    }
    exut$val[exut$idx==i]=chg
  }
  
  colnames(exut)=c("x","y","idx","val")
  
  ## We show uncertainties not variances
  
  if(!pix){
    plt=base_map_outlets(data = exut,val_name = "val")
  }else{
    plt=base_map_grid(data = exut,val_name = "val")
  }
  if(vartype=="mean"){
    if(var!="tasAdjust"){
      q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
      q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
      lim_col=max(q99pos,q99neg)
      lim_col=round(lim_col/5)*5#arrondi au 5 le plus proche
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(temp_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),oob=squish,show.limits = T)+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif moyen du ",ind_name_full,"\npour le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990)"))#+
    }else{
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=(1-freq_col))
      lim_col=as.numeric(c(q01,q99))
      lim_col=round(lim_col/0.25)*0.25#arrondi au 1 le plus proche
      br=seq(lim_col[1],lim_col[2],0.25)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement moyen du ",ind_name_full,"\npour le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990)"))#+
    }
  }
  if(vartype=="vartot"){
    if(var!="tasAdjust"){
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01/5)*5,round(q99/5)*5)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude\ntotale (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),1),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité totale du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }else{
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01/0.5)*0.5,round(q99/0.5)*0.5)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude\ntotale (°C)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),1),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité totale du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }
  }
  if(vartype=="incert"){
    if(var!="tasAdjust"){
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01/5)*5,round(q99/5)*5)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),1),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité (sauf interne) du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }else{
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01/0.5)*0.5,round(q99/0.5)*0.5)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude (°C)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),1),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité (sauf interne) du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }
  }
  if(vartype=="varint"){
    if(var!="tasAdjust"){
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01/5)*5,round(q99/5)*5)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude\ninterne (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),1),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité interne du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }else{
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01/0.5)*0.5,round(q99/0.5)*0.5)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude\ninterne (°C)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),1),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité interne du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }
  }
  
  if(vartype=="varres"){
    if(var!="tasAdjust"){
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01),round(q99))
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude\nrésiduelle (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),1),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité résiduelle du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }else{
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=1-freq_col)
      lim_col=c(round(q01,2),round(q99,2))
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Incertitude\nrésiduelle (°C)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=lim_col,breaks=seq(lim_col[1],lim_col[2],length.out=6),labels= c(paste0("< ",lim_col[1]),round(seq(lim_col[1]+(lim_col[2]-lim_col[1])/6,lim_col[1]+(lim_col[2]-lim_col[1])/6*4,length.out=4),2),paste0("> ",lim_col[2])),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Incertitude\nliée à la variabilité résiduelle du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz," ",pred_unit,")"))
    }
  }
  if(vartype=="rcp8.5"|vartype=="rcp85"){
    if(var!="tasAdjust"){
      q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
      q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
      lim_col=max(q99pos,q99neg)
      lim_col=round(lim_col/5)*5#arrondi au 5 le plus proche
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif moyen du ",ind_name_full,"\npour le prédicteur ",pred_name," et le RCP 8.5\n(",horiz," ",pred_unit," VS 1990)"))
    }else{
      q99=quantile(exut$val,probs=freq_col)
      q01=quantile(exut$val,probs=(1-freq_col))
      lim_col=as.numeric(c(q01,q99))
      lim_col=round(lim_col/0.5)*0.5#arrondi au 1 le plus proche
      br=seq(lim_col[1],lim_col[2],0.5)
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement moyen du ",ind_name_full,"\npour le prédicteur ",pred_name," et le RCP 8.5\n(",horiz," ",pred_unit," VS 1990)"))
    }
  }
  if(!pix){
    plt$layers[[3]]$aes_params$size=5
  }
  
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("map_total_change_",vartype,"_",ind_name,"_",pred,"_",horiz),Folder = folder_out,Format = "jpeg")
  }
  
}

############################################################################
## Map variance partition
## lst.QUALYPSOOUT a list of QUALYPSOOUT by watershed
## pred_name the plain language name of the predictor
## pred the predictor name in the file
## ind_name the name of the indicator
## ind_name_full the plain language name of the indicator
## pred_unit the unit of the predictor
## folder_out the saving folder
## horiz he horizon of predictor wanted

map_var_part=function(lst.QUALYPSOOUT,horiz,pred,pred_name,pred_unit,ind_name,ind_name_full,folder_out,pix=F,var="toto"){
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=sim_stations[,c("Lon","Lat")]
  }
  colnames(exut)=c("x","y")
  exut$idx=seq(1:nrow(exut))
  exut$rcp=exut$gcm=exut$rcm=exut$rv=0
  
  if(!is.null(lst.QUALYPSOOUT[[1]]$CONTRIB_EACH_EFFECT$hm)){
    exut$hm=0
  }
  if(!is.null(lst.QUALYPSOOUT[[1]]$CONTRIB_EACH_EFFECT$bc)){
    exut$bc=0
  }
  
  for (i in 1:length(lst.QUALYPSOOUT)){
    idx_Xfut=which(lst.QUALYPSOOUT[[i]]$Xfut==horiz)
    exut$rcp[exut$idx==i]=lst.QUALYPSOOUT[[i]]$DECOMPVAR[idx_Xfut,"rcp"]*100
    exut$gcm[exut$idx==i]=lst.QUALYPSOOUT[[i]]$DECOMPVAR[idx_Xfut,"gcm"]*100
    exut$rcm[exut$idx==i]=lst.QUALYPSOOUT[[i]]$DECOMPVAR[idx_Xfut,"rcm"]*100
    if(!is.null(lst.QUALYPSOOUT[[i]]$CONTRIB_EACH_EFFECT$bc)){
      exut$bc[exut$idx==i]=lst.QUALYPSOOUT[[i]]$DECOMPVAR[idx_Xfut,"bc"]*100
    }
    if(!is.null(lst.QUALYPSOOUT[[i]]$CONTRIB_EACH_EFFECT$hm)){
      exut$hm[exut$idx==i]=lst.QUALYPSOOUT[[i]]$DECOMPVAR[idx_Xfut,"hm"]*100
    }
    exut$rv[exut$idx==i]=lst.QUALYPSOOUT[[i]]$DECOMPVAR[idx_Xfut,"ResidualVar"]*100
  }
  
  exut=pivot_longer(exut,cols=-c(x,y,idx),names_to="source",values_to = "val")
  exut=exut[order(exut$source),]
  labs_part=list("rv"="Variabilité Résiduelle","rcp"="RCP","gcm"="GCM","rcm"="RCM","bc"="Correction de biais","hm"="Modèle hydrologique")
  labs_part_labeller <- function(variable,value){
    return(labs_part[value])
  }
  lim_col=as.numeric(round(quantile((exut$val),probs=0.99),-1))
  
  if(!pix){
    plt=base_map_outlets(data = exut,val_name = "val")
  }else{
    plt=base_map_grid(data = exut,val_name = "val")
  }
  plt=plt+
    binned_scale(aesthetics = "fill",scale_name = "toto",name="Partition de variance (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=c(0,lim_col),breaks=seq(0,lim_col,length.out=6),show.limits = T,labels= c(0,round(seq(0+(lim_col-0)/6,0+(lim_col-0)/6*4,length.out=4),1),paste0("> ",lim_col)),oob=squish)+#that way because stepsn deforms colors
    ggtitle(paste0("Partition de variance du ",ind_name_full,"\npour le prédicteur ",pred_name,"\n(",horiz," ",pred_unit,")"))+
    facet_wrap(vars(factor(source,levels=c("rv","rcp","gcm","rcm","bc","hm"))),labeller=labs_part_labeller )
  if(!pix){
    plt$layers[[3]]$aes_params$size=3
  }
  
  
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("map_var_part_",ind_name,"_",pred,"_",horiz),Folder = folder_out,Format = "jpeg")
  }
  
}


#####################################################
## Plot watershed areas

plot_bv_areas=function(folder_out){
  exut=sim_stations[,c("Num_ordre_Modcou","Lat","Lon","Surf_mod")]
  colnames(exut)=c("Num_ordre_Modcou","y","x","val")
  plt=base_map_outlets(data = exut,val_name = "val")
  plt=plt+
    binned_scale(aesthetics = "fill",scale_name = "toto",name="Superficie (km2)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",trans="log10",show.limits = T,breaks=c(500,1000,5000,1e4),limits=c(100,5e4),oob=squish)+#that way because stepsn deforms colors and with trans cannot show limits
    ggtitle("Superficie des bassins versants")
  plt$layers[[3]]$aes_params$size=5
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = "superficie_bv",Folder = folder_out,Format = "jpeg")
  }
}


