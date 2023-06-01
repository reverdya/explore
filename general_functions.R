# Alix Reverdy
# Explore 2
# general data science function and data representations

library(fst)#read_fst
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
# install.packages(paste0(getwd(),"/QUALYPSO/"), repos = NULL, type="source") # and close R so that git does not freak out
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
library(plotwidgets)#hsl colors (would be better to use hsluv model but not available on my R version)
library(abind)#abind
library(gridExtra)#grid.arrange
library(rstudioapi)#restartSession
library(stringr)#str_replace
library(onewaytests)#bf.test


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


###########################################################################################################
## Restart session in code for memory issues
## fct the heavy function
## cpt the first iteration (do not change default=1)
## last the last iteration
## step the step to which have the restart launched

restart_loop=function(fct,cpt=1,last,step){
  fct(cpt) 
  print(cpt)
  if(cpt==1){
    assign("STEP",step,envir = globalenv())
    assign("LAST",last,envir = globalenv())
    assign("FCT",fct,envir = globalenv())
  }
  assign("CPT",cpt+1,envir = globalenv())
  if((cpt)!=last){
    if(cpt%%step==0){
      restartSession(command="restart_loop(fct=FCT,cpt=CPT,last=LAST,step=STEP)")#using only stuff in global environment
    }else{
      restart_loop(fct=FCT,cpt=CPT,last=LAST,step=STEP)
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
ipcc_yelred_5=c(rgb(255,255,178,maxColorValue=255),rgb(254,204,92,maxColorValue=255),rgb(253,141,60,maxColorValue=255),rgb(240,59,32,maxColorValue=255),rgb(189,0,38,maxColorValue=255))

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

tmp=col2hsl(c(plasma(6),"grey90"))
tmp["S",1:6]=1
tmp["L",1]=0.15
tmp["L",2]=0.35
tmp["L",4]=0.7
tmp["L",6]=0.425
# pal.safe(hsl2col(tmp))

# col=rev(c("orange","yellow","cadetblue1","blue1","darkgreen","darkgoldenrod4","darkorchid1"))
# col_7var=rev(viridis(7))
col_7var=hsl2col(tmp)
tmp=col_7var
##alternating colors for easier rading
col_7var[1]=tmp[2]
col_7var[2]=tmp[5]
col_7var[3]=tmp[3]
col_7var[4]=tmp[6]
col_7var[5]=tmp[4]
col_7var[6]=tmp[1]
col_7var[7]=tmp[7]
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
## Takes a color palette (pal) and a vector of values (values) that should be represented with this color palette
# param is between 0 and 1, the closest it is to 0 the more colors are attributed to the lower values, for 1 the palette is unchanged

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
## typeChangeVariable "rel" or "abs"
## spar the spline smoothing in case of spline (vector of  size nS)
## type the type of smoothing applied: spline (classic but can pick each spar, log_spline (log transform, spline , then unlog)

## etaStar is by definition of X dimensions's and not Xfut and can contain NA (logical when thinking of tempreature predictor)

prepare_clim_resp_2D=function(Y, Xmat, Xfut, Xref, typeChangeVariable, spar,type,nY,nS,nFut,scenAvail){
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
    zz = (!is.na(Ys))&(!is.na(Xs)) #!is.na(Xs) because for HadGEM2-ES tasAdjust_yearmean cannot be calculated in 2099
    phiY=Xs
    
    if(type=="spline"){
      smooth.spline.out<-stats::smooth.spline(Xs[zz],Ys[zz],spar=spar[iS])
      # store spline object
      climateResponse[[iS]] = smooth.spline.out
      # fitted responses at the points of the fit (for etaStar)
      phiY[zz] = predict(smooth.spline.out, Xs[zz])$y
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
      phiY[zz] = 10^predict(smooth.spline.out, Xs[zz])$y
      # fitted responses at unknown points ("Xfut")
      phiS = 10^predict(smooth.spline.out, Xfut)$y
      # climate response of the reference/control time/global tas
      phiC = 10^predict(smooth.spline.out, Xrefs)$y
    }
    if(type=="linear_rcp26"){
      name_rcp=scenAvail$rcp[iS]
      if(name_rcp!="rcp26"){
        smooth.spline.out<-stats::smooth.spline(Xs[zz],Ys[zz],spar=spar[iS])
        # store spline object
        climateResponse[[iS]] = smooth.spline.out
        # fitted responses at the points of the fit (for etaStar)
        phiY[zz] = predict(smooth.spline.out, Xs[zz])$y
        # fitted responses at unknown points ("Xfut")
        phiS = predict(smooth.spline.out, Xfut)$y
        # climate response of the reference/control time/global tas
        phiC = predict(smooth.spline.out, Xrefs)$y
      }else{
        y=Ys[zz]
        x=Xs[zz]
        smooth.spline.out=lm(y~x)
        # store spline object
        climateResponse[[iS]] = smooth.spline.out
        # fitted responses at the points of the fit (for etaStar)
        phiY[zz] = as.vector(predict(smooth.spline.out, data.frame(x=Xs[zz])))
        # fitted responses at unknown points ("Xfut")
        phiS = as.vector(predict(smooth.spline.out, data.frame(x=Xfut)))
        # climate response of the reference/control time/global tas
        phiC = as.vector(predict(smooth.spline.out, data.frame(x=Xrefs)))
      }
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
  
  if(typeChangeVariable=='rel'& any(phiStar<(-1))){
    warning_store="logSpline"
    for(iS in 1:nS){
      # projection for this simulation chain
      Ys = Y[iS,]
      Xs = Xmat[iS,]
      Xrefs = Xref[iS]
      # fit a smooth signal
      zz = (!is.na(Ys))&(!is.na(Xs))
      phiY=Xs
      Yslog10=log10(Ys)
      smooth.spline.out<-stats::smooth.spline(Xs[zz],Yslog10[zz],spar=spar[iS])
      # store spline object
      climateResponse[[iS]] = smooth.spline.out
      # fitted responses at the points of the fit (for etaStar)
      phiY[zz] = 10^predict(smooth.spline.out, Xs[zz])$y
      # fitted responses at unknown points ("Xfut")
      phiS = 10^predict(smooth.spline.out, Xfut)$y
      # climate response of the reference/control time/global tas
      phiC = 10^predict(smooth.spline.out, Xrefs)$y
      phi[iS,] = phiS
      phiStar[iS,] = phiS/phiC-1
      etaStar[iS,] = (Ys-phiY)/phiC
      YStar[iS,] = (Ys-phiC)/phiC
      phi[iS,] = phiS
      # Climate change response: phiStar, and internal variability expressed as a change: etaStar
      phiStar[iS,] = phiS/phiC-1
      etaStar[iS,] = (Ys-phiY)/phiC
      YStar[iS,] = (Ys-phiC)/phiC
    }
  }else{
    warning_store="NA"
  }
  
  tmp=pivot_longer(data.frame(t(etaStar)),cols=everything(),names_to = "rhs",values_to = "lhs")
  pval=bf.test(lhs ~ rhs,tmp,na.rm=T,verbose = F)$p.value
  if(pval<=0.05){
    warning_store=paste0(warning_store,"_interchain-p:",pval)
  }
  
  if(any(Xmat==2045,na.rm = T)){
    pval=vector(mode="numeric",length=nS)
    for(iS in 1:nS){
      tmp=data.frame(X=Xmat[iS,],etaS=etaStar[iS,])
      tmp=tmp[tmp$X>=1990,]
      tmp[tmp$X>=2045,]$X="B"
      tmp[tmp$X!="B",]$X="A"
      pval[iS]=bf.test(etaS ~ X,tmp,na.rm=T,verbose = F)$p.value
    }
    if(any(pval<=0.05)){
      warning_store=paste0(warning_store,"_intrachain-p:",min(pval))
    }
  }
  
  # Variance related to the internal variability: considered constant over the time period
  # (see Eq. 22 and 23 in Hingray and Said, 2014). We use a direct empirical estimate
  # of the variance of eta for each simulation chain and take the mean, see Eq. 19
  varInterVariability = mean(apply(etaStar,2,function(x) var(x)),na.rm=T)
  
  # return objects
  return(list(phiStar=phiStar,etaStar=etaStar,YStar=YStar,phi=phi,climateResponse=climateResponse,varInterVariability=varInterVariability,warning_store=warning_store))
  
}

prepare_clim_resp=function(Y, X, Xfut, typeChangeVariable, spar,type,nbcores=6,scenAvail=scenAvail){
  
  # dimensions
  d = dim(Y)
  if(length(d)==3){
    # Y is an array: GridPoints x Scenario x Time
    nG = d[1]
    nS = d[2]
    nY = d[3]
    paralType = 'Grid'
  }else{
    # Y is a matrix: Scenario x Time
    nS = nrow(Y)
    nY = ncol(Y)
    paralType = 'Time'
  }
  if(is.null(X)){
    Xmat = matrix(rep(1:nY,nS),byrow=T,nrow=nS,ncol=nY)
  }else if(is.vector(X)){
    if(nY!=length(X)){
      stop('if X is provided as a vector, its length must equal the number of columns of Y')
    }else{
      # repeat the vector to obtain a matrix
      Xmat = matrix(rep(X,nS),byrow=T,nrow=nS,ncol=nY)
    }
  }else if(is.matrix(X)){
    Xmat = X
    if(length(dim(Y))==2){
      if(any(dim(X)!=dim(Y))){
        stop('if X is provided as a matrix and Y as a grid, its size must match the size of Y')
      }
    }else if(length(dim(Y))==3){
      if(any(dim(X)!=dim(Y)[c(2,3)])){
        stop('if X is provided as a matrix, its first dimension must match the second
             dimension of Y (number of scenarios) and and its second dimension must match
             the third dimension of Y (number of future predictions)')
      }
    }
  }else{
    stop('X must be NULL, a vector or a matrix')
  }
  
  Xref=Xfut[1]
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
  
  
  
  if(paralType=="Time"){
    out=prepare_clim_resp_2D(Y = Y,Xmat = Xmat,Xfut = Xfut,Xref = Xref,typeChangeVariable = typeChangeVariable,spar = spar,type = type,nS = nS,nFut = nFut,scenAvail=scenAvail,nY=nY)
  }
  
  if(paralType=="Grid"){
    # initialise outputs
    phiStar = phi = array(dim=c(nG,nS,length(Xfut)))
    etaStar = YStar = array(dim=d)
    varInterVariability = vector(length=nG)
    climateResponse=vector(mode="list",length=nG)
    climResponse=vector(mode="list",length=nG)
    warning_store=vector(mode="list",length=nG)

    gc()
    # no parallelisation because memory issue in loading Y on each core (and parallelisation on lower level function would require too much core communication)
    for(g in (1:nG)){
      # check is some simulation chains are entirely missing
      hasAllNa = apply(Y[g,,],1,function(x) all(is.na(x)))
      if(any(hasAllNa)){
        climResponse[[g]]=list(phiStar = NA, etaStar = NA, phi = NA,
                               varInterVariability = NA)
      }else{
        climResponse[[g]]= prepare_clim_resp_2D(Y = Y[g,,],Xmat = Xmat,Xfut = Xfut,Xref = Xref,typeChangeVariable = typeChangeVariable,spar = spar,type = type,nY=nY,nS = nS,nFut = nFut,scenAvail=scenAvail)
      }
      if(g%%100==0){gc()}
    }
    gc()
    # fill the matrices
    for(g in 1:nG){
      phiStar[g,,] = climResponse[[g]]$phiStar
      etaStar[g,,] = climResponse[[g]]$etaStar
      phi[g,,] = climResponse[[g]]$phi
      varInterVariability[g] = climResponse[[g]]$varInterVariability
      YStar[g,,] = climResponse[[g]]$YStar
      climateResponse[[g]]=climResponse[[g]]$climateResponse
      warning_store[[g]]=climResponse[[g]]$warning_store
    }
    out=list(phiStar=phiStar,etaStar=etaStar,YStar=YStar,phi=phi,climateResponse=climateResponse,varInterVariability=varInterVariability,warning_store=warning_store)
  }
  
  # return objects
  return(out)
  
}


#######################################################################################
## Takes a QUALYPSOOUT object and reconstructs chains from mean change +effects
## QUALYPSOOUT a Qualypso output
## line 1= mean+eff_rcp1+eff_gcm1+eff_rcm1...
## line 2= mean+eff_rcp2+eff_gcm1+eff_rcm1...
## line 3= mean+eff_rcp3+eff_gcm1+eff_rcm1...
## line 4= mean+eff_rcp1+eff_gcm2+eff_rcm1...
## line 5= mean+eff_rcp2+eff_gcm2+eff_rcm1...

reconstruct_chains=function(lst.QUALYPSOOUT,idx_space=NULL,idx_pred=NULL){
  neff=length(lst.QUALYPSOOUT[[1]]$namesEff)
  n_eachEff=vector()
  for(i in 1:neff){
    n_eachEff[i]=ncol(lst.QUALYPSOOUT[[1]]$MAINEFFECT[[i]]$MEAN)
  }
  if(!is.null(idx_pred)){
    chains=data.frame(do.call("rbind", replicate(prod(n_eachEff),lapply(lst.QUALYPSOOUT,function(x) x$GRANDMEAN$MEAN[idx_pred]), simplify = FALSE)))
  }
  if(!is.null(idx_space)){
    chains=data.frame(do.call("rbind", replicate(prod(n_eachEff),lst.QUALYPSOOUT[[idx_space]]$GRANDMEAN$MEAN, simplify = FALSE)))
  }
  lst_eff=vector(mode = "list",length=neff)
  
  if(!is.null(idx_pred)){
    for (j in 1:length(lst.QUALYPSOOUT[[1]]$Xfut)){
      for(i in 1:neff){
        lst_eff[[i]]=lapply(lst.QUALYPSOOUT,function(x) x$MAINEFFECT[[i]]$MEAN[idx,])[[j]]
      }
      chains[,j]=unlist(chains[,j])+rowSums(expand.grid(lst_eff))
    }
  }
  if(!is.null(idx_space)){
    for (j in 1:length(lst.QUALYPSOOUT[[idx_space]]$GRANDMEAN$MEAN)){
      for(i in 1:neff){
        lst_eff[[i]]=lst.QUALYPSOOUT[[idx_space]]$MAINEFFECT[[i]]$MEAN[j,]
      }
      chains[,j]=chains[,j]+rowSums(expand.grid(lst_eff))
    }
  }
  return(chains)
}

###################################
## Format Global temperature for use in Qualypso
## path_temp the path to the file with France temperatures
##simu_lst the list of simulations


prep_global_tas=function(path_temp,simu_lst,cat="meteo"){
  load(file=paste0(path_temp,"pred_temp.Rdata"))
  chains_pred=names(pred_temp)
  chains_pred=str_replace(chains_pred,"CNRM-CM5-LR","CNRM-CM5")
  chains_pred=str_replace(chains_pred,"HadREM3-GA7-05","HadREM3-GA7")
  chains_var=paste0(simu_lst$rcp,"_",simu_lst$gcm,"_",simu_lst$rcm,"_",simu_lst$bc)
  mat_Globaltas=vector(mode="list",length=length(chains_var))
  for (j in 1:length(chains_var)){
    mat_Globaltas[[j]]=pred_temp[[which(chains_pred==chains_var[j])]][c("year","temp_spline1990")]
  }
  mat_Globaltas=Reduce(function(...) merge(...,by="year", all=T), mat_Globaltas)
  gcm_years=mat_Globaltas[,1]
  mat_Globaltas=mat_Globaltas[,-1]
  if(cat=="hydro"){
    colnames(mat_Globaltas)=paste0(chains_var,"_",simu_lst$hm)
  }else{
    colnames(mat_Globaltas)=chains_var
  }
  return(list(mat_Globaltas=mat_Globaltas,gcm_years=gcm_years))
}

###################################
## Format Global température for use in Qualypso, to be used inside code run_QUalypso
## spline calculated with data between 1860 and 1900
## Difference to 1860-1900 average and rcp/gcm matching + spline smoothing
## path_data the root of the path
##simu_lst the list of simulations
#first_data_year and last_data_year the first an last years with data for simu all year round

## This is outdated by prep_global_tas that uses Meteo France methodology
# 
# prep_global_tas_old=function(path_temp,ref_year=1990,simu_lst,var="meteo"){
#   ## Prepare temperatures RCP/GCM
#   paths=list.files(path_temp,pattern=glob2rx("global_tas*"),full.names = T)
#   for ( i in 1:length(paths)){
#     tas_glob=read.csv(paths[i],skip=3,sep="",header=F)
#     if(grepl("HadGEM2-ES",paths[i],fixed=T)){# -999 values in 1859
#       tas_glob=tas_glob[-1,]
#     }
#     tas_glob=data.frame(year=tas_glob[,1],tas=apply(tas_glob[,-1],MARGIN = 1,mean))# mean of 12 months
#     colnames(tas_glob)[2]=paste0(strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][5],"_",strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][4])#rcp_gcm name
#     tas_glob=tas_glob[tas_glob$year>=1860&tas_glob$year<=2100,]
#     if(i==1){
#       mat_Globaltas_gcm=tas_glob
#     }else{
#       mat_Globaltas_gcm=merge(mat_Globaltas_gcm,tas_glob,by="year")
#     }
#   }
#   mat_Globaltas_gcm[,-1]=apply(mat_Globaltas_gcm[,-1],2,function(x) smooth.spline(mat_Globaltas_gcm[,1],x,spar=1)$y)
#   mat_Globaltas_gcm[,-1]=apply(mat_Globaltas_gcm[,-1],2,function(x) x-x[mat_Globaltas_gcm[,1]==ref_year])
#   
#   gcm_years=mat_Globaltas_gcm$year
#   
#   ## Format global temperature for Qualypso
#   if(var=="hydro"){
#     colnames(mat_Globaltas_gcm)[colnames(mat_Globaltas_gcm)=="rcp85_CNRM-CM5-LR"]="rcp85_CNRM-CM5"
#     colnames(mat_Globaltas_gcm)[colnames(mat_Globaltas_gcm)=="rcp45_CNRM-CM5-LR"]="rcp45_CNRM-CM5"
#     colnames(mat_Globaltas_gcm)[colnames(mat_Globaltas_gcm)=="rcp26_CNRM-CM5-LR"]="rcp26_CNRM-CM5"
#   }
#   mat_Globaltas=vector(length=nrow(simu_lst),mode="list")
#   vec_global_tas_gcm=unlist(lapply(colnames(mat_Globaltas_gcm)[-1],function(x) strsplit(x,"_")[[1]][2]))
#   vec_global_tas_rcp=unlist(lapply(colnames(mat_Globaltas_gcm)[-1],function(x) strsplit(x,"_")[[1]][1]))
#   for (i in 1:nrow(simu_lst)){
#     mat_Globaltas[[i]]= mat_Globaltas_gcm[,which(vec_global_tas_gcm==simu_lst[i,]$gcm & vec_global_tas_rcp==sub(".","",simu_lst[i,]$rcp,fixed=T))+1]
#   }
#   mat_Globaltas=t(do.call(cbind,mat_Globaltas))
#   
#   ## Calculate 1860-1990 warming from HADCRUT5 with spline
#   nc=load_nc(paste0(path_temp,"HadCRUT.5.0.1.0.analysis.summary_series.global.annual.nc"))
#   res=ncvar_get(nc,varid="tas_mean")
#   full_years=nc$dim$time$vals
#   full_years=year(as.Date(full_years,origin="1850-01-01"))
#   nc_close(nc)#for some reason stays opened otherwise
#   rm(nc)
#   gc()
#   
#   tas_obs=smooth.spline(x=full_years,y=res,spar=1)$y
#   warming_1990=tas_obs[full_years==1990]-tas_obs[full_years==1875]
#   mat_Globaltas=mat_Globaltas+warming_1990
#   mat_Globaltas_gcm[,-1]=mat_Globaltas_gcm[,-1]+warming_1990
#   
#   return(list(mat_Globaltas=mat_Globaltas,mat_Globaltas_gcm=mat_Globaltas_gcm,gcm_years=gcm_years,warming_1990=warming_1990))
# }


###########################################################
## Plot raw indicator and spline (2 functions necessary)

extract_chains=function(scenAvail,ref_cities,type="cities",cat="meteo"){
 
  if(cat=="meteo"){
    all_chain=vector(length=nrow(scenAvail),mode="list")
    for(c in 1:nrow(scenAvail)){# for each chain
      
      if(type=="cities"){
        pth_tmp=list.files(paste0(path_data,"indic/",scenAvail$var[c],"/"),full.names=T,pattern=glob2rx(paste0(scenAvail$var[c],"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c][[1]],"_")[[1]][1],"*",scenAvail$period[c],"*")))#by default recursive=F
      }
      if(type=="bas"){
        pth_tmp=list.files(paste0(path_data,"indic/",scenAvail$var[c],"/bas/"),full.names=T,pattern=glob2rx(paste0(scenAvail$var[c],"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c][[1]],"_")[[1]][1],"*",scenAvail$period[c],"*")))
      }
      nc=load_nc(pth_tmp)
      res=ncvar_get(nc,varid=scenAvail$var[c])
      full_years=nc$dim$time$vals
      if(scenAvail$bc[c]=="ADAMONT"){
        full_years=year(as.Date(full_years,origin="1950-01-01"))
      }
      if(scenAvail$bc[c]=="CDFt"){
        full_years=year(as.Date(full_years,origin="1850-01-01"))
      }
      nc_close(nc)#for some reason stays opened otherwise
      rm(nc)
      gc()
      res2=data.frame(matrix(nrow=dim(res)[length(dim(res))],ncol=nrow(ref_cities)+1))
      res2[,1]=full_years
      for (j in 1 :nrow(ref_cities)){
        if(type=="cities"){
          res2[,j+1]=res[ref_cities$row[j],ref_cities$col[j],]
        }
        if(type=="bas"){
          res2[,j+1]=res[ref_cities$X[j],]
        }
      }
      colnames(res2)[1]="year"
      all_chain[[c]]=res2
      rm(res)
      rm(res2)
      gc()
    }
  }
  if(cat=="hydro"){
    all_chain=vector(length=nrow(scenAvail),mode="list")
    for(c in 1:nrow(scenAvail)){# for each chain
      dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*",scenAvail$gcm[c],"*",scenAvail$rcp[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",scenAvail$hm[c],"*")))
      pth_tmp=Sys.glob(paths=paste0(dir_tmp,"/*/*/*",scenAvail$indic[c],"*"))#sys.glob allow accents
      res=read_fst(pth_tmp)
      colnames(res)=c("gcm","rcp","rcm","bc","hm","code","year","indic")
      RES=res[res$code %in% ref_cities$code,]
      rm(res);gc()
      res=RES
      res$year=year(res$year)
      res=res[,c("year","code","indic")]
      res=pivot_wider(res,names_from = code,values_from = indic)
      all_chain[[c]]=res
    }
    rm(res)
    gc()
  }
  
  return(all_chain)
  rm(all_chain)
}


plot_spline=function(all_chains,type,pred,scenAvail,globaltas=NULL,SPAR,rcp,spline_type="spline",city_name,place="cities",idx,cat="meteo",cut_ymax=F,print_warning=T){
  
  scen_rcp=which(scenAvail$rcp==rcp)
  chains_rcp=all_chains[scen_rcp]
  ClimateProjections=lapply(chains_rcp, function(x) x[,c(1,idx+1)])
  Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections))
  Y=Y[,Y[1,]<=2100]
  X=Y[1,]
  if(pred=="temp"){
    vec_years=X
    X=as.matrix(t(globaltas[["mat_Globaltas"]][globaltas[["gcm_years"]] %in% vec_years,]))
    Xmax=round(max(X,na.rm=T),1)-0.1#goes far to be able to make the correspondance with global temperature
    X=X[scen_rcp,]
    Xfut=seq(0,Xmax,0.1)
    Y=Y[,vec_years %in% globaltas[["gcm_years"]]]
  }else{
    Xfut=seq(centr_ref_year,X[length(X)])
  }
  Y=Y[-1,]
  nS=nrow(scenAvail)
  if(cat=="meteo"){
    if(scenAvail$var[1]!="tasAdjust"){
      typeChangeVar="rel"
    }else{
      typeChangeVar="abs"
    }
  }
  if(cat=="hydro"){
    typeChangeVar="rel"
  }
  clim_resp=prepare_clim_resp(Y=Y,X=X,Xfut=Xfut,typeChangeVariable = typeChangeVar,spar = rep(SPAR,nS),type = spline_type)
  if(pred=="time"){
    if(type=="raw_spline"|type=="raw"){
      raw=data.frame(t(Y[,X>=Xfut[1]]))
      spline=data.frame(t(clim_resp$phi))
    }
    if(type=="diff_spline"|type=="diff"){
      raw=data.frame(t(clim_resp$YStar[,X>=Xfut[1]]))
      spline=data.frame(t(clim_resp$phiStar))
    }
    raw[is.na(t(Y[,X>=Xfut[1]]))]=NA
    if(cat=="meteo"){
      colnames(raw)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp])
    }
    if(cat=="hydro"){
      colnames(raw)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp],"_",scenAvail$hm[scen_rcp])
    }
    raw$xfut=X[X>=Xfut[1]]
    raw=pivot_longer(data=raw,cols=!xfut,names_to = "model",values_to = "val")
    spline[is.na(t(Y[,X>=Xfut[1]]))]=NA
    if(cat=="meteo"){
      colnames(spline)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp])
    }
    if(cat=="hydro"){
      colnames(spline)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp],"_",scenAvail$hm[scen_rcp])
    }
    spline$xfut=X[X>=Xfut[1]]
    spline=pivot_longer(data=spline,cols=!xfut,names_to = "model",values_to = "val")
  }
  if(pred=="temp"){
    if(type=="raw_spline"|type=="raw"){
      raw=data.frame(t(Y))
      spline=data.frame(t(clim_resp$phi))
    }
    if(type=="diff_spline"|type=="diff"){
      raw=data.frame(t(clim_resp$YStar))
      spline=data.frame(t(clim_resp$phiStar))
    }
    if(cat=="meteo"){
      colnames(raw)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp])
    }
    if(cat=="hydro"){
      colnames(raw)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp],"_",scenAvail$hm[scen_rcp])
    }
    raw_x=data.frame(t(X))
    raw=pivot_longer(data=raw,cols=everything(),names_to = "model",values_to = "val")
    raw$xfut=pivot_longer(data=raw_x,cols=everything(),names_to = "model",values_to = "val")$val
    if(cat=="meteo"){
      colnames(spline)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp])
    }
    if(cat=="hydro"){
      colnames(spline)=paste0(scenAvail$rcp[scen_rcp],"_",scenAvail$gcm[scen_rcp],"_",scenAvail$rcm[scen_rcp],"_",scenAvail$bc[scen_rcp],"_",scenAvail$hm[scen_rcp])
    }
    spline$xfut=Xfut
    spline=pivot_longer(data=spline,cols=!xfut,names_to = "model",values_to = "val")
  }
  
  raw$type="raw"
  spline$type="spline"
  data=rbind(raw,spline)
  warn=clim_resp$warning_store
  rm(chains_rcp,ClimateProjections,clim_resp,raw,spline,Y)
  gc()
  data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
  data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
  data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[3]))
  data$bc=unlist(lapply(strsplit(data$model,"_"),function(x) x[4]))
  if(cat=="hydro"){
    data$hm=unlist(lapply(strsplit(data$model,"_"),function(x) x[5]))
  }
  
  if(cat=="meteo"){
    if(scenAvail$var[1]!="tasAdjust"&(type=="raw_spline"|type=="raw")){
      ylabel="Réponse climatique"
      unit=" (mm)"
    }
    if(scenAvail$var[1]=="tasAdjust"&(type=="raw_spline"|type=="raw")){
      ylabel="Réponse climatique"
      unit=" (°C)"
    }
    if(scenAvail$var[1]!="tasAdjust"&(type=="diff_spline"|type=="diff")){
      ylabel="Réponse en\nchangement climatique"
      unit=" (%)"
    }
    if(scenAvail$var[1]=="tasAdjust"&(type=="diff_spline"|type=="diff")){
      ylabel="Réponse en\nchangement climatique"
      unit=" (°C)"
    }
  }
  if(cat=="hydro"){
    if(type=="raw_spline"|type=="raw"){
      ylabel="Réponse climatique"
      unit=" (m3/s)"
    }
    if(type=="diff_spline"|type=="diff"){
      ylabel="Réponse en\nchangement climatique"
      unit=" (%)"
    }
  }
  
  rcm_colors=tol(length(unique(scenAvail$rcm)))
  names(rcm_colors)=unique(scenAvail$rcm)
  
  if(cat=="meteo"){
    if(type=="diff_spline"|type=="raw_spline"){
      plt=ggplot(data)+#Warnings okay
        geom_line(aes(x=xfut,y=val,size=type,color=rcm))+
        scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur",ylabel))
    }
    if(type=="diff"|type=="raw"){
      plt=ggplot(data[data$type=="raw",])+#Warnings okay
        geom_line(aes(x=xfut,y=val,size=type,color=rcm))+
        scale_size_manual("",values=c(0.7),label=c("Indicateur"))
    }
    plt=plt+
      scale_color_manual("RCM",values=rcm_colors[unique(data$rcm)])+
      theme_bw(base_size = 18)+
      theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      scale_x_continuous("")+
      scale_y_continuous(paste0(ylabel,unit),limits = c(min(data$val,na.rm=T),max(data$val,na.rm=T)),n.breaks=4,expand = c(0,0))+
      guides(color = guide_legend(override.aes = list(size = 1.7)))+
      facet_grid(gcm~bc)+
      theme(panel.spacing.x = unit(0.5, "lines"))+
      theme(strip.text.y = element_text(size = 9))
    if(print_warning & warn!="NA"){
      plt=plt+
        ggtitle(warn)+
        theme(plot.title = element_text( face="bold",  size=8,hjust=0.5))
    }
    if(place=="cities"){
      if(SPAR==1){
        save.plot(plt,Filename = paste0(scenAvail$var[1],"_",scenAvail$indic[1],"_",type,"_chronique_",pred,"_",city_name,"_",rcp,"_spar1.0"),Folder = paste0(path_fig,v,"/",scenAvail$indic[1],"/"),Format = "jpeg")
      }else{
        save.plot(plt,Filename = paste0(scenAvail$var[1],"_",scenAvail$indic[1],"_",type,"_chronique_",pred,"_",city_name,"_",rcp,"_spar",SPAR),Folder = paste0(path_fig,v,"/",scenAvail$indic[1],"/"),Format = "jpeg")
      }
    }else{
      if(SPAR==1){
        save.plot(plt,Filename = paste0(scenAvail$var[1],"_",scenAvail$indic[1],"_",type,"_chronique_",pred,"_",city_name,"_",rcp,"_spar1.0"),Folder = paste0(path_fig,v,"/",scenAvail$indic[1],"/",place,"/"),Format = "jpeg")
      }else{
        save.plot(plt,Filename = paste0(scenAvail$var[1],"_",scenAvail$indic[1],"_",type,"_chronique_",pred,"_",city_name,"_",rcp,"_spar",SPAR),Folder = paste0(path_fig,v,"/",scenAvail$indic[1],"/",place,"/"),Format = "jpeg")
      }
    }
  }
  
  if(cat=="hydro"){
    for(h in unique(data$hm)){
      if(type=="diff_spline"|type=="raw_spline"){
        plt=ggplot(data[data$hm==h,])+#Warnings okay
          geom_line(aes(x=xfut,y=val,size=type,color=rcm))+
          scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur",ylabel))
      }
      if(type=="diff"|type=="raw"){
        plt=ggplot(data[data$type=="raw"&data$hm==h,])+#Warnings okay
          geom_line(aes(x=xfut,y=val,size=type,color=rcm))+
          scale_size_manual("",values=c(0.7),label=c("Indicateur"))
      }
      plt=plt+
        scale_color_manual("RCM",values=rcm_colors[unique(data$rcm)])+
        theme_bw(base_size = 18)+
        theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
        theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
        scale_x_continuous("")+
        guides(color = guide_legend(override.aes = list(size = 1.7)))+
        facet_grid(gcm~bc)+
        theme(panel.spacing.x = unit(0.5, "lines"))+
        theme(strip.text.y = element_text(size = 9))
      if(cut_ymax==T){
        plt=plt+
          scale_y_continuous(paste0(ylabel,unit),limits = c(min(data$val,na.rm=T),quantile(data$val,probs=0.99,na.rm=T)),n.breaks=4,expand = c(0,0))
      }else{
        plt=plt+
          scale_y_continuous(paste0(ylabel,unit),limits = c(min(data$val,na.rm=T),max(data$val,na.rm=T)),n.breaks=4,expand = c(0,0))
      }
      if(print_warning & warn!="NA"){
        plt=plt+
          ggtitle(warn)+
          theme(plot.title = element_text( face="bold",  size=8,hjust=0.5))
      }
      if(SPAR==1){
        save.plot(plt,Filename = paste0(scenAvail$indic[1],"_",type,"_chronique_",pred,"_",city_name,"_",rcp,"_",h,"_spar1.0"),Folder = paste0(path_fig,scenAvail$indic[1],"/"),Format = "jpeg")
      }else{
        save.plot(plt,Filename = paste0(scenAvail$indic[1],"_",type,"_chronique_",pred,"_",city_name,"_",rcp,"_",h,"_spar",SPAR),Folder = paste0(path_fig,scenAvail$indic[1],"/"),Format = "jpeg")
      }
    }
  }
  rm(plt,data)
  gc()
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

plotQUALYPSOeffect_ggplot=function(lst.QUALYPSOOUT,idx,nameEff,includeMean=FALSE,includeRCP=NULL,plain_nameEff,pred,pred_name,ind_name,ind_name_full,bv_name,bv_full_name,pred_unit,folder_out,xlim,var="toto"){
  
  Xfut = lst.QUALYPSOOUT[[1]]$Xfut
  iEff = which(lst.QUALYPSOOUT[[1]]$namesEff == nameEff)
  nEff=lst.QUALYPSOOUT[[1]]$listScenarioInput$nTypeEff[iEff]
  if (length(iEff) == 0){    stop("wrong value for nameEff")} 
  if (includeMean) {
    EffHat = do.call(rbind,lapply(lst.QUALYPSOOUT,function(x) x$MAINEFFECT[[nameEff]]$MEAN[idx,]+x$GRANDMEAN$MEAN[idx]))
    ylims=c(min(unlist(lapply(lst.QUALYPSOOUT,function(x) lapply(x$MAINEFFECT,function(y) y$MEAN[idx,]+x$GRANDMEAN$MEAN[idx])))),max(unlist(lapply(lst.QUALYPSOOUT,function(x) lapply(x$MAINEFFECT,function(y) y$MEAN[idx,]+x$GRANDMEAN$MEAN[idx])))))
  }  else {
    EffHat = do.call(rbind,lapply(lst.QUALYPSOOUT,function(x) x$MAINEFFECT[[nameEff]]$MEAN[idx,]))
    ylims=c(min(unlist(lapply(lst.QUALYPSOOUT,function(x) lapply(x$MAINEFFECT,function(y) y$MEAN[idx,])))),max(unlist(lapply(lst.QUALYPSOOUT,function(x) lapply(x$MAINEFFECT,function(y) y$MEAN[idx,])))))
  }
  if(!is.null(includeRCP)){
    ircp=which(lst.QUALYPSOOUT[[1]]$namesEff == "rcp")
    i_thisrcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ircp]]==includeRCP)
    EffHat = apply(EffHat,MARGIN=2,function(x) x+unlist(lapply(lst.QUALYPSOOUT,function(y) y$MAINEFFECT$rcp$MEAN[idx,i_thisrcp]+y$GRANDMEAN$MEAN[idx])))
    
    ref_mean=unlist(lapply(lst.QUALYPSOOUT,function(y) y$MAINEFFECT$rcp$MEAN[idx,i_thisrcp]+y$GRANDMEAN$MEAN[idx]))
    ylims[1]=ylims[1]+min(ref_mean)
    ylims[2]=ylims[2]+max(ref_mean)
  }
  EffhanEff = dim(EffHat)[2]
  meanPred =EffHat
  if(var!="tasAdjust"){
    med=data.frame(meanPred)*100
  }else{
    med=data.frame(meanPred)
  }
  
  colnames(med)=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]]
  med$pred=Xfut
  med=pivot_longer(data=med,cols=!pred,names_to = "eff",values_to = "med")
  data=med
  
  color_select=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]]
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
  
  if(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)[iEff]=="rcp"){
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


plotQUALYPSO_summary_change=function(lst.QUALYPSOOUT,idx,pred,pred_name,ind_name,ind_name_full,bv_name,bv_full_name,pred_unit,folder_out,xlim,var="toto",indic="titi",idx_pix="tata",idx_row="tata",idx_col="tata",path_temp=NULL){
  
  if(var!="tasAdjust"){
    Ystar=(lst.QUALYPSOOUT[[1]]$Y[idx,,]-lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phi[idx,,1])/lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phi[idx,,1]*100
  }else{
    Ystar=lst.QUALYPSOOUT[[1]]$Y[idx,,]-lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phi[idx,,1]
  }
  scenAvail=lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail
  Xfut = lst.QUALYPSOOUT[[1]]$Xfut
  probCI = lst.QUALYPSOOUT[[1]]$listOption$probCI
  
  if(pred=="time"){
    iEff = which(lst.QUALYPSOOUT[[1]]$namesEff == "rcp")
    EffHat=do.call(rbind,lapply(lst.QUALYPSOOUT,function(x) x$MAINEFFECT[["rcp"]]$MEAN[idx,]+x$GRANDMEAN$MEAN[idx]))
    nEff = dim(EffHat)[2]
  }
  if(pred=="temp"){
    EffHat=do.call(rbind,lapply(lst.QUALYPSOOUT,function(x) x$GRANDMEAN$MEAN[idx]))
    nEff = dim(EffHat)[2]
  }
  meanPred = EffHat
  if(var!="tasAdjust"){
    med=data.frame(meanPred)*100
  }else{
    med=data.frame(meanPred)
  }
  if(pred=="time"){
    colnames(med)=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]]
  }
  if(pred=="temp"){
    colnames(med)="rcp85"
  }
  med$pred=Xfut
  med=pivot_longer(data=med,cols=!pred,names_to = "eff",values_to = "med")
  
  Binf=NULL
  Bsup=NULL
  for (r in 1:nEff){
    if(pred=="time"){
      idx_rcp=which(scenAvail$rcp==lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]][r])
      # phiStar
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[idx,idx_rcp,]
      idx_phistar_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenComp==lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]][r]&!lst.QUALYPSOOUT[[1]]$listScenarioInput$isMissing)
    }
    if(pred=="temp"){
      # phiStar
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[idx,,]
      idx_phistar_rcp=which(!lst.QUALYPSOOUT[[1]]$listScenarioInput$isMissing)
    }
    #Reconstructed chains
    chains=reconstruct_chains(lst.QUALYPSOOUT,idx_pred = idx)
    #Replace for this rcp the reconstructed values by the true values
    chains[idx_phistar_rcp,]=phiStar
    if(pred=="time"){
      chains=chains[seq(r,nrow(chains),3),]#only this rcp
      # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
      # obtained from QUALYPSO
      sd.qua = sqrt(unlist(lapply(lst.QUALYPSOOUT,function(x) x$TOTALVAR[idx]-x$INTERNALVAR[idx]-x$EFFECTVAR[idx,iEff])))
    }
    if(pred=="temp"){
      # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
      # obtained from QUALYPSO
      sd.qua = sqrt(unlist(lapply(lst.QUALYPSOOUT,function(x) x$TOTALVAR[idx]-x$INTERNALVAR[idx])))
    }
    sd.emp = apply(chains,2,sd)
    sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
    sd.corr = sd.qua/sd.emp
    phiStar.corr = phiStar*t(replicate(dim(phiStar)[1],sd.corr))
    # compute the lower bound if the distribution is gaussian
    binf = apply(phiStar.corr,2,quantile,probs = (1-probCI)/2)
    bsup = apply(phiStar.corr,2,quantile,probs = 0.5+probCI/2)
    if(var!="tasAdjust"){
      if(any(binf<(-1))){
        warning("Lower bound forced to -100%")
        binf[binf<(-1)]=(-1)
      }
      Binf=cbind(Binf,binf*100)
      Bsup=cbind(Bsup,bsup*100)
    }else{
      Binf=cbind(Binf,binf)
      Bsup=cbind(Bsup,bsup)
    }
  }
  
  
  Binf=data.frame(Binf)
  Bsup=data.frame(Bsup)
  if(pred=="time"){
    colnames(Binf)=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]]
    colnames(Bsup)=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]]
  }
  if(pred=="temp"){
    colnames(Binf)="rcp85"
    colnames(Bsup)="rcp85"
  }
  Binf$pred=Xfut
  Bsup$pred=Xfut
  Binf=pivot_longer(data=Binf,cols=!pred,names_to = "eff",values_to = "binf")
  Bsup=pivot_longer(data=Bsup,cols=!pred,names_to = "eff",values_to = "bsup")
  data=merge(med,Binf,by=c("pred","eff"))
  data=merge(data,Bsup,by=c("pred","eff"))
  
  if(pred=="time"){
    if(var!="tasAdjust"){
      chains=data.frame(as.vector(lst.QUALYPSOOUT[[1]]$Xmat),as.vector(Ystar),rep(scenAvail$rcp,times=dim(lst.QUALYPSOOUT[[1]]$Xmat)[2]),rep(paste0(scenAvail$rcp,scenAvail$gcm,scenAvail$rcm),times=dim(lst.QUALYPSOOUT[[1]]$Xmat)[2]))
    }else{
      chains=data.frame(as.vector(lst.QUALYPSOOUT[[1]]$Xmat),as.vector(Ystar),rep(scenAvail$rcp,times=dim(lst.QUALYPSOOUT[[1]]$Xmat)[2]),rep(paste0(scenAvail$rcp,scenAvail$gcm,scenAvail$rcm),times=dim(lst.QUALYPSOOUT[[1]]$Xmat)[2]))
    }
  }
  if(pred=="temp"){
    if(var!="tasAdjust"){
      chains=data.frame(as.vector(lst.QUALYPSOOUT[[1]]$Xmat),as.vector(Ystar),"rcp85",rep(paste0("rcp85",scenAvail$gcm,scenAvail$rcm),times=dim(lst.QUALYPSOOUT[[1]]$Xmat)[2]))
    }else{
      chains=data.frame(as.vector(lst.QUALYPSOOUT[[1]]$Xmat),as.vector(Ystar),"rcp85",rep(paste0("rcp85",scenAvail$gcm,scenAvail$rcm),times=dim(lst.QUALYPSOOUT[[1]]$Xmat)[2]))
    }
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
        Obs=res[idx_pix,]
      }
      if(var=="evspsblpotAdjust"){
        full_years=nc$dim$time$vals
        full_years=year(as.Date(full_years,origin="1975-01-01"))
        res=ncvar_get(nc,varid="etp")
        Obs=res[idx_row,idx_col,]
      }
      rm(nc)
      gc()
    }else{
      Obs=read_fst(paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-hydro/Diagnostic/diag/",i,".fst"))
      Obs=Obs[Obs$Code==bv_name,]
      colnames(Obs)=c("hm","code","year","val")
      Obs$year=year(Obs$year)
      Obs=Obs[Obs$hm=="GRSD",]
      full_years=Obs$year
      idx_na=which(!is.na(Obs$val))
    }
    sp=lst.QUALYPSOOUT[[1]]$listOption$spar
    if(var!="tasAdjust"){
      if(var=="Q"){
        # hms=unique(Obs$hm)
        # obs_hm=Obs[Obs$hm==hms[1],]
        # tmp=data.frame(obs_hm$year,(obs_hm$val-obs_hm$val[obs_hm$year==1990])/obs_hm$val[obs_hm$year==1990]*100,hms[1])
        # colnames(tmp)=c("pred","val","hm")
        # for(j in 2:length(hms)){
        #   obs_hm=Obs[Obs$hm==hms[j],]
        #   tmp2=data.frame(obs_hm$year,(obs_hm$val-obs_hm$val[obs_hm$year==1990])/obs_hm$val[obs_hm$year==1990]*100,hms[j])
        #   colnames(tmp2)=c("pred","val","hm")
        #   tmp=rbind(tmp,tmp2)
        # }
        # Obs=tmp
        tmp=smooth.spline(full_years[idx_na],Obs$val[idx_na],spar = sp)
        obs1990=predict(tmp, 1990)$y
        Obs=data.frame(full_years,(Obs$val-obs1990)/obs1990*100)
        colnames(Obs)=c("pred","val")
      }else{
        tmp=smooth.spline(full_years,Obs,spar = sp)$y
        Obs=data.frame(full_years,(Obs-tmp[full_years==1990])/tmp[full_years==1990]*100)
        colnames(Obs)=c("pred","val")
      }
    }else{
      tmp=smooth.spline(full_years,Obs,spar = sp)$y
      Obs=data.frame(full_years,Obs-tmp[full_years==1990])
      colnames(Obs)=c("pred","val")
    }
  }
  if(pred=="temp"){
    chains=chains[chains$pred>=xlim[1] & chains$pred<=xlim[2],]
    xlim2=c(0,xlim[2])
    
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
        Obs=res[idx_pix,]
      }
      if(var=="evspsblpotAdjust"){
        full_years=nc$dim$time$vals
        full_years=year(as.Date(full_years,origin="1975-01-01"))
        res=ncvar_get(nc,varid="etp")
        Obs=res[idx_row,idx_col,]
      }
      rm(nc)
      gc()
    }else{
      Obs=read_fst(paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-hydro/Diagnostic/diag/",i,".fst"))
      Obs=Obs[Obs$Code==bv_name,]
      colnames(Obs)=c("hm","code","year","val")
      Obs$year=year(Obs$year)
      Obs=Obs[Obs$hm=="GRSD",]#same for all models
      full_years=Obs$year
    }
    # Calculate 1860-1990 warming from HADCRUT5 with spline
    nc=load_nc(paste0(path_temp,"HadCRUT.5.0.1.0.analysis.summary_series.global.annual.nc"))
    res2=ncvar_get(nc,varid="tas_mean")
    full_years2=nc$dim$time$vals
    full_years2=year(as.Date(full_years2,origin="1850-01-01"))
    nc_close(nc)#for some reason stays opened otherwise
    rm(nc)
    gc()
    tas_obs=smooth.spline(x=full_years2,y=res2,spar=1)$y
    tas_obs=tas_obs-tas_obs[full_years2==1875]
    
    sp=lst.QUALYPSOOUT[[1]]$listOption$spar
    if(var!="tasAdjust"){
      if(var=="Q"){
        # hms=unique(Obs$hm)
        # obs_hm=Obs[Obs$hm==hms[1],]
        # tmp=data.frame(tas_obs[which(full_years2 %in% obs_hm$year)],(obs_hm$val-obs_hm$val[obs_hm$year==1990])/obs_hm$val[obs_hm$year==1990]*100,hms[1])
        # colnames(tmp)=c("pred","val","hm")
        # for(j in 2:length(hms)){
        #   obs_hm=Obs[Obs$hm==hms[j],]
        #   tmp2=data.frame(tas_obs[which(full_years2 %in% obs_hm$year)],(obs_hm$val-obs_hm$val[obs_hm$year==1990])/obs_hm$val[obs_hm$year==1990]*100,hms[j])
        #   colnames(tmp2)=c("pred","val","hm")
        #   tmp=rbind(tmp,tmp2)
        # }
        # Obs=tmp
        
        
        idx_na=which(!is.na(Obs$val))
        tas_obs=tas_obs[which(full_years2 %in% full_years)]
        tmp=smooth.spline(tas_obs[idx_na],Obs$val[idx_na],spar = sp)
        obs1990=predict(tmp, tas_obs[full_years==1990])$y
        Obs=data.frame(tas_obs,(Obs$val-obs1990)/obs1990*100)
        colnames(Obs)=c("pred","val")
      }else{
        tas_obs=tas_obs[which(full_years2 %in% full_years)]
        tmp=smooth.spline(tas_obs,Obs,spar = sp)$y
        Obs=data.frame(tas_obs,(Obs-tmp[full_years==1990])/tmp[full_years==1990]*100)
        colnames(Obs)=c("pred","val")
      }
    }else{
      tas_obs=tas_obs[which(full_years2 %in% full_years)]
      tmp=smooth.spline(tas_obs,Obs,spar = sp)$y
      Obs=data.frame(tas_obs,Obs-tmp[full_years==1990])
      colnames(Obs)=c("pred","val")
    }
    
  }
  
  if(pred=="time"){
    color_select=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]]
    if(substr(color_select[1],5,5)!="."){
      for (cs in 1:length(color_select)){
        color_select[cs]=paste(c(substr(color_select[cs], 1, 4), substr(color_select[cs], 5,nchar(color_select[cs]))), collapse=".")#rcp26 to rcp2.6
      }
    }
  }
  if(pred=="temp"){
    color_select="rcp8.5"
  }

  chain_band=aggregate(chains$val,by=list(chains$pred,chains$eff),max)
  colnames(chain_band)=c('pred',"eff","max")
  chain_band$min=aggregate(chains$val,by=list(chains$pred,chains$eff),min)[,3]
  if(pred=="time"){
    a_label=data.frame(lab="a",eff="rcp26")
    idx_label=seq(1,3)
  }
  if(pred=="temp"){
    a_label=data.frame(lab="a",eff="rcp85")
    idx_label=3
  }
  
  if(pred=="time"){
    plt1=ggplot(data)+
      geom_ribbon(data=chain_band,aes(x=pred,ymin=min,ymax=max,fill=eff),alpha=0.7)+#raw chain band
      scale_fill_discrete("Dispersion des expériences\nclimatiques",type= as.vector(col_3rcp_shade[color_select]),labels=NULL)+
      guides(fill=guide_legend(order=2,nrow=1, byrow=TRUE,title.theme=element_text(size = 13)))+
      new_scale_fill()+
      geom_ribbon(aes(x=pred,ymin=binf,ymax=bsup,fill=eff),alpha=0.4)+#uncertainty band
      scale_fill_discrete("Incertitude liée aux modèles\n(intervalle 5-95%)",type= as.vector(col_3rcp[color_select]),labels=NULL)+
      guides(fill=guide_legend(order=2,nrow=1, byrow=TRUE,title.theme=element_text(size = 13)))
  }else{
    plt1=ggplot(data)+
      geom_ribbon(aes(x=pred,ymin=binf,ymax=bsup,fill=eff),alpha=0.4)+#uncertainty band
      scale_fill_discrete("Incertitude liée aux modèles\n(intervalle 5-95%)",type= as.vector(col_3rcp[color_select]),labels=NULL)+
      guides(fill=guide_legend(order=2,nrow=1, byrow=TRUE,title.theme=element_text(size = 13)))
  }
  plt1=plt1+
    geom_line(aes(x=pred,y=med,group=eff,color=eff),size=1.5,lty="21")+#RCP mean
    scale_x_continuous("",limits=xlim2,expand=c(0,0))+
    scale_color_discrete("Moyenne d'ensemble",type= as.vector(col_3rcp[color_select]),labels=labels_rcp[idx_label])+
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
  # if(var!="Q"){
    plt1=plt1+
      geom_line(data=Obs,aes(x=pred,y=val,size="aa"),alpha=0.7,color="gray50")+
      scale_size_manual("",values = c("aa"=1),labels=c("Observations"))
  # }else{
  #   plt1=plt1+
  #     geom_line(data=Obs,aes(x=pred,y=val,size="aa",group=hm),alpha=0.7,color="gray50")+
  #     scale_size_manual("",values = c("aa"=1),labels=c("Observations"))
  # }
  
  if(pred=="temp"){
    plt1=plt1+
      scale_x_continuous("Niveau de réchauffement planétaire (°C)",limits=xlim2,expand=c(0,0))
  }

  
  
  
  
  
  perc_pos=vector(mode="list",length=nEff)
  for (r in 1:nEff){
    if(pred=="time"){
      idx_rcp=which(scenAvail$rcp==lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]][r])
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[idx,idx_rcp,]
    }
    if(pred=="temp"){
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[idx,,]
    }
    phiStar=phiStar[,Xfut>xlim[1]]
    n_chain=dim(phiStar)[1]
    perc_pos[[r]]=apply(phiStar,MARGIN=2,function(x) sum(x>=0)/n_chain*100)
  }
  data=data.frame(Xfut[Xfut>xlim[1]],do.call("cbind",perc_pos))
  if(pred=="time"){
    colnames(data)=c("pred",lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]])
  }
  if(pred=="temp"){
    colnames(data)=c("pred","rcp85")
  }
  data=pivot_longer(data,cols=!pred,names_to = "rcp",values_to = "val")
  if(pred=="time"){
    rcp.labs <- c("RCP 2.6", "RCP 4.5", "RCP 8.5")
  }
  if(pred=="temp"){
    rcp.labs <- c("RCP 8.5")
  }
  
  plt2=ggplot(data)+
    geom_point(aes(x=pred,y=factor(rcp,levels=c("rcp26","rcp45","rcp85")),fill=val),color="transparent",size=5,shape=21)+
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
  nEff = lst.QUALYPSOOUT[[1]]$listScenarioInput$nEff
  vecEff = 1:nEff
  VARDECOMP = do.call(rbind,lapply(lst.QUALYPSOOUT,function(x) x$DECOMPVAR[idx,]))
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
  namesEff = lst.QUALYPSOOUT[[1]]$names
  names_var=c(namesEff,"res","int")
  colnames(data)=c("Xfut",names_var)
  data=pivot_longer(data=data,cols=-c(Xfut),names_to = "var",values_to = "val")
  data$val=data$val*100 #percentage
  
  vec_color=col_7var[names(col_7var) %in% names_var]
  data$var=factor(data$var,levels=rev(names(vec_color)))
  labels_var=rev(legend_7var[names(col_7var) %in% names_var])
  
  plt3=ggplot(data)+
    geom_ribbon(aes(x=Xfut,ymin=0,ymax=val,fill=var))+
    # geom_line(data=data[data$var!="int",],aes(x=Xfut,y=val,group=var),color="black",linetype="dashed",size=0.5)+
    scale_fill_discrete("",type = vec_color,labels=labels_var)+
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
    save.plot(plt,Filename = paste0("summary_change_",ind_name,"_",pred,"_",bv_full_name),Folder = folder_out,Format = "jpeg")
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


plotQUALYPSO_boxplot_horiz_rcp=function(lst.QUALYPSOOUT,idx,pred,pred_name,ind_name,ind_name_full,bv_name,bv_full_name,pred_unit,folder_out,var="toto",indic="titi",horiz=c(2030,2050,2085),title=T){
  
  scenAvail=lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail
  Xfut = lst.QUALYPSOOUT[[1]]$Xfut
  if (predict=="time"){
    iEff = which(lst.QUALYPSOOUT[[1]]$namesEff == "rcp")
    nrcp=length(unique(scenAvail$rcp))
  }
  if (predict=="temp"){
    nrcp=1
  }
  h=which(Xfut %in% horiz)
  
  
  phiStar.corr=list()
  for (r in 1:nrcp){
    if (predict=="time"){
      idx_rcp=which(scenAvail$rcp==lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]][r])
      # phiStar
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[idx,idx_rcp,h]
    }
    if (predict=="temp"){
      # phiStar
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[idx,,h]
    }
    #Reconstructed chains
    chains=reconstruct_chains(lst.QUALYPSOOUT,idx_pred = idx)[h]
    #Replace for this rcp the reconstructed values by the true values
    if (predict=="time"){
      idx_phistar_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenComp==lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]][r]&!lst.QUALYPSOOUT[[1]]$listScenarioInput$isMissing)
    }
    if (predict=="temp"){
      idx_phistar_rcp=which(!lst.QUALYPSOOUT[[1]]$listScenarioInput$isMissing)
    }
    chains[idx_phistar_rcp,]=phiStar
    if(predict=="time"){
      chains=chains[seq(r,nrow(chains),3),]#only this rcp
    }
    # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
    # obtained from QUALYPSO
    sd.emp = apply(chains,2,sd)
    sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
    sd.qua=sd.emp
    i0=1
    for (i in h){
      if(predict=="time"){
        sd.qua[i0] = sqrt(unlist(lapply(lst.QUALYPSOOUT[i],function(x) x$TOTALVAR[idx]-x$INTERNALVAR[idx]-x$EFFECTVAR[idx,iEff])))
      }
      if(predict=="temp"){
        sd.qua[i0] = sqrt(unlist(lapply(lst.QUALYPSOOUT[i],function(x) x$TOTALVAR[idx]-x$INTERNALVAR[idx])))
      }
      i0=i0+1
    }
    sd.corr = sd.qua/sd.emp
    phiStar.corr[[r]] = data.frame(phiStar*t(replicate(dim(phiStar)[1],sd.corr)))
    colnames(phiStar.corr[[r]])=paste0("h_",horiz)
    if(predict=="time"){
      phiStar.corr[[r]]$rcp=unique(scenAvail$rcp)[r]
    }
    if(predict=="temp"){
      phiStar.corr[[r]]$rcp="rp8.5"
    }
    
  }
  
  phiStar.corr=bind_rows(phiStar.corr)
  if(var!="tasAdjust"){
    if(any(phiStar.corr[,c(1:(ncol(phiStar.corr)-1))]<(-1))){
      warning("Lower bound forced to -100%")
      phiStar.corr[phiStar.corr[,c(1:(ncol(phiStar.corr)-1))]<-1,c(1:(ncol(phiStar.corr)-1))]=(-1)
    }
    phiStar.corr[,c(1:(ncol(phiStar.corr)-1))]=phiStar.corr[,c(1:(ncol(phiStar.corr)-1))]*100
  }
  phiStar.corr=pivot_longer(phiStar.corr,cols=-c(rcp),names_to = "horiz",values_to = "val")
  
  if(pred=="time"){
    color_select=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[iEff]]
    if(substr(color_select[1],5,5)!="."){
      for (cs in 1:length(color_select)){
        color_select[cs]=paste(c(substr(color_select[cs], 1, 4), substr(color_select[cs], 5,nchar(color_select[cs]))), collapse=".")#rcp26 to rcp2.6
      }
    }
    label_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")
  }
  if(pred=="temp"){
    color_select="rcp8.5"
    label_rcp=c("RCP 8.5")
  }
  
  custom_boxplot=function(x){
    return(data.frame(ymin=min(x), ymax=max(x), upper=quantile(x,probs=0.95), lower=quantile(x,probs=0.05), middle=mean(x)))
  }
  
  plt1=ggplot(phiStar.corr)+
    stat_summary(fun.data = custom_boxplot,geom = "boxplot",aes(x=horiz,y=val,fill=rcp),lwd=2,position="dodge")+
    geom_point(aes(x=horiz,y=val,color=rcp),alpha=0)+#just for legend
    scale_fill_discrete("",type=as.vector(col_3rcp_shade[color_select]))+
    scale_color_discrete("",type=as.vector(col_3rcp_shade[color_select]),labels=label_rcp)+
    scale_x_discrete("",labels = horiz)+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    theme(legend.text = element_text(size=12,face="bold"))+
    theme(strip.background = element_blank(),strip.text.x = element_blank(),legend.key.height =unit(5, "lines"))+
    guides(fill="none",color=guide_legend(override.aes = list(alpha=1,shape=15,size=15)))
  if(var!="tasAdjust"){
    plt1=plt1+
      scale_y_continuous(paste0("Changement relatif (%)"),expand=c(0,0))
  }else{
    plt1=plt1+
      scale_y_continuous(paste0("Changement (°C)"),expand=c(0,0))
  }
  if(pred=="temp"){
    plt1=plt1+
      scale_x_discrete("Niveau de réchauffement (°C)",labels = horiz)
  }
  
  custom_boxplot2=function(x){
    return(data.frame(ymin=min(x), ymax=max(x), upper=quantile(x,probs=0.75), lower=quantile(x,probs=0.25), middle=mean(x)))
  }
  plt2=ggplot(data.frame(x=rep(1,100),y=c(1:100)))+
    stat_summary(fun.data = custom_boxplot2,geom = "boxplot",aes(x=x,y=y),lwd=2,width=0.02)+
    geom_text(aes(x=1.05,y=50,label="Moyenne\nd'ensemble"),fontface = "bold",size=5)+
    geom_text(aes(x=1.05,y=75,label="Incertitude\nmodèle (95%)"),fontface = "bold",size=5)+
    geom_text(aes(x=1.05,y=25,label="Incertitude\nmodèle (5%)"),fontface = "bold",size=5)+
    geom_text(aes(x=1.05,y=100,label="Projection\nmaximale"),fontface = "bold",size=5)+
    geom_text(aes(x=1.05,y=0,label="Projection\nminimale"),fontface = "bold",size=5)+
    scale_x_continuous("",limits = c(0.98,1.08),expand=c(0,0))+
    theme_void()+
    theme(panel.background = element_rect(colour="black",fill="transparent"))
  
  plt=ggarrange(plt1,plt2,widths=c(0.8,0.2),nrow=1,ncol=2,align="h")+
    theme(plot.margin = unit(c(0,0.5,0,0), "cm"))
  if(title==T){
    plt=annotate_figure(plt, top = text_grob(paste0("Distribution de l'ensemble balancé pour le prédicteur ",pred_name,"\net l'indicateur ",ind_name_full,"\n(",bv_full_name,", référence 1990)"), face = "bold", size = 20,hjust=0.5))+
      theme(panel.background = element_rect(fill="white"))
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

background_for_maps=function(path_river,path_fr){
  river=read_shp(path_river)
  fr=read_shp(path_fr)
  river_L2=read_shp(path_river,wgs84_to_l2 = T)
  fr_L2=read_shp(path_fr,wgs84_to_l2 = T)
  
  data(wrld_simpl)
  options(warn=-1)
  wrld <- fortify(wrld_simpl)
  options(warn=0)
  assign("river",river,envir = globalenv())
  assign("river_L2",river_L2,envir = globalenv())
  assign("fr",fr,envir = globalenv())
  assign("fr_L2",fr_L2,envir = globalenv())
}


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

base_map_grid=function(data,val_name,pattern_name=NULL,facet_vert_name=NULL,facet_horizontal_name=NULL,threshold="<80%",exclude_horizontal=c("5%","95%"),afterscale=F){
  n=nrow(data)
  n_rep=n/length(mask_fr)
  data$idx=data$idx*rep(mask_fr,n_rep)
  if(!afterscale){
    data=data[data$idx!=0,]
  }
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
      geom_polygon_pattern(data=mask_polygon,aes(x = long, y = lat,group=group,pattern_density=factor(layer,levels=c(0,1))),pattern_alpha=0.5,pattern_color="black",pattern_fill="white",pattern_size=0.6,color=NA,fill=NA,pattern="circle")+
      scale_pattern_density_manual(values = c("0"=0.4,"1"=0),drop=F)
      guides(fill=guide_colorbar(barwidth = 2, barheight = 15,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))+
      geom_polygon(data=fr_L2,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=0.1)
  }else{
    if(afterscale){
      plt=ggplot(data=data,aes(x=x,y=y,color=get(val_name)))+
        geom_tile(aes(fill=after_scale(colour)),key_glyph = draw_key_rect)
    }else{
      plt=ggplot(data=data)+
        geom_tile(aes(x=x,y=y,fill=get(val_name)))+
        geom_path(data=river_L2,aes(x=long,y=lat,group=group),colour="gray80",size=0.1,alpha=0.5)+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 15,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))+
        geom_polygon(data=fr_L2,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=0.1)
    }
  }
  plt=plt+
    coord_equal()+
    scale_x_continuous("")+
    scale_y_continuous("")+
    theme_bw(base_size = 10)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.2))+
    theme(axis.ticks =element_blank(),axis.text = element_blank() )+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank())+
    theme(strip.text = element_text(size = 12, face = "bold"))
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
  Xfut=lst.QUALYPSOOUT[[1]]$Xfut
  idx_Xfut=which(Xfut==horiz)
  probCI = lst.QUALYPSOOUT[[1]]$listOption$probCI
  quant=c("5%","mean","95%")
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=data.frame(x=as.vector(ref$x),y=as.vector(ref$y))
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

  for(r in rcp_names){
    ieff_this_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]==r)
    chg_mean=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT$rcp$MEAN[,ieff_this_rcp]+lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN[ieff_this_rcp]
    
    idx_this_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail$rcp==r)
    # phiStar
    phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[,idx_this_rcp,idx_Xfut]
    chains=reconstruct_chains(lst.QUALYPSOOUT,idx_space = idx_Xfut)
    #Replace for this rcp the reconstructed values by the true values
    idx_phistar_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenComp==lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]][ieff_this_rcp]&!lst.QUALYPSOOUT[[1]]$listScenarioInput$isMissing)
    chains[idx_phistar_rcp,]=t(phiStar)
    chains=chains[seq(ieff_this_rcp,dim(chains)[1],3),]#only this rcp
    # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
    # obtained from QUALYPSO
    sd.qua = sqrt(lst.QUALYPSOOUT[[idx_Xfut]]$TOTALVAR-lst.QUALYPSOOUT[[idx_Xfut]]$INTERNALVAR-lst.QUALYPSOOUT[[idx_Xfut]]$EFFECTVAR[,ieff_rcp])
    sd.emp = apply(chains,2,sd)
    sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
    sd.corr = sd.qua/sd.emp
    phiStar.corr = phiStar*replicate(dim(phiStar)[2],sd.corr)
    chg_q5 = apply(phiStar.corr,1,quantile,probs = (1-probCI)/2)
    chg_q95 = apply(phiStar.corr,1,quantile,probs = 0.5+probCI/2)
    

    resp=lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[,which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail$rcp==r),idx_Xfut]*100
    if(var!="tasAdjust"){
      if(any(chg_q5<(-1))){
        warning("Lower bound forced to -100%")
        chg_q5[chg_q5<(-1)]=-1
      }
      exut$val[exut$rcp==r & exut$quant==quant[2]]=chg_mean*100
      exut$val[exut$rcp==r & exut$quant==quant[1]]=chg_q5*100
      exut$val[exut$rcp==r & exut$quant==quant[3]]=chg_q95*100
    }else{
      exut$val[exut$rcp==r & exut$quant==quant[2]]=chg_mean
      exut$val[exut$rcp==r & exut$quant==quant[1]]=chg_q5
      exut$val[exut$rcp==r & exut$quant==quant[3]]=chg_q95
    }
    exut$sign[exut$rcp==r ]=apply(resp,1,function(x) sum(x>0))/ncol(resp)*100
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
  quant.labs <- c("Projections\nbasses", "Projections\nintermédiaires", "Projections\nhautes")
  names(quant.labs) <- quant
  
  
  #Setting limits for color scale
  if(var!="tasAdjust"){
    q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
    q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg,na.rm=T)
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
      facet_grid(rcp ~ quant,labeller = labeller(rcp=rcp.labs, quant = quant.labs))+
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
        ggtitle(paste0("Changement relatif du ",ind_name_full," et\nson incertitude pour différents RCPs\net le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        scale_pattern_density_manual("Accord sur le signe du\nchangement (Projections intermédiaires)",values = c("0"=0.4,"1"=0),drop=F,labels=c("<80%",">80%"))+
        theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20))
        if(var=="evspsblpotAdjust"){
          plt=plt+
            binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(rev(precip_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
        }
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

map_3quant_1rcp_3horiz=function(lst.QUALYPSOOUT,horiz,rcp_name, rcp_plainname,pred,pred_name,pred_unit,ind_name,ind_name_full,folder_out,freq_col=0.99,pix=F,var="toto",nbcores=6,path_temp=NULL,cat="meteo"){
  
  if(pred=="time"){
    ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
    ieff_this_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]==rcp_name)
  }
  probCI=lst.QUALYPSOOUT[[1]]$listOption$probCI
  quant=c("5%","mean","95%")
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=data.frame(x=as.vector(ref$x),y=as.vector(ref$y))
  }
  
  exut$idx=seq(1:nrow(exut))
  
  tmp=exut
  if(pred=="time"){
    for (i in 1:8){
      exut=rbind(exut,tmp)
    }
    exut$horiz=rep(horiz,each=nrow(exut)/3)
    exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
  }
  if(pred=="temp"){
    for (i in 1:11){
      exut=rbind(exut,tmp)
    }
    exut$horiz=rep(horiz,each=nrow(exut)/4)
    exut$quant=rep(rep(quant,each=nrow(exut)/12),times=4)
  }
  exut$val=0
  exut$sign=0
  
  exut$sign_agree="<80%"
  
  for(h in horiz){
    idx_Xfut=which(lst.QUALYPSOOUT[[1]]$Xfut==h)
    
    if(pred=="time"){
      chg_mean=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT$rcp$MEAN[,ieff_this_rcp]+lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN
      idx_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail$rcp==rcp_name)
      # phiStar
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[,idx_rcp,idx_Xfut]
      idx_phistar_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenComp==lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]][ieff_this_rcp]&!lst.QUALYPSOOUT[[1]]$listScenarioInput$isMissing)
    }
    if(pred=="temp"){
      chg_mean=lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN
      # phiStar
      phiStar = lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[,,idx_Xfut]
      idx_phistar_rcp=which(!lst.QUALYPSOOUT[[1]]$listScenarioInput$isMissing)
    }
    chains=reconstruct_chains(lst.QUALYPSOOUT,idx_space = idx_Xfut)
    #Replace for this rcp the reconstructed values by the true values
    chains[idx_phistar_rcp,]=t(phiStar)
    if(pred=="time"){
      chains=chains[seq(ieff_this_rcp,nrow(chains),3),]#only this rcp
    }
    # compute a multiplicative factor SDtot/SD(phi*) to match the standard deviation for each RCP
    # obtained from QUALYPSO
    if (pred=="time"){
      sd.qua = sqrt(lst.QUALYPSOOUT[[idx_Xfut]]$TOTALVAR-lst.QUALYPSOOUT[[idx_Xfut]]$INTERNALVAR-lst.QUALYPSOOUT[[idx_Xfut]]$EFFECTVAR[,ieff_rcp])
    }
    if(pred=="temp"){
      sd.qua = sqrt(lst.QUALYPSOOUT[[idx_Xfut]]$TOTALVAR-lst.QUALYPSOOUT[[idx_Xfut]]$INTERNALVAR)
    }
    sd.emp = apply(chains,2,sd)
    sd.emp[sd.emp==0] = 1 # avoid NaN for the reference year
    sd.corr = sd.qua/sd.emp
    phiStar.corr = phiStar*replicate(dim(phiStar)[2],sd.corr)
    chg_q5 = apply(phiStar.corr,1,quantile,probs = (1-probCI)/2)
    chg_q95 = apply(phiStar.corr,1,quantile,probs = 0.5+probCI/2)
    
    if(pred=="time"){
      resp=lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[,which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail$rcp==rcp_name),idx_Xfut]*100
    }
    if(pred=="temp"){
      resp=lst.QUALYPSOOUT[[1]]$CLIMATEESPONSE$phiStar[,,idx_Xfut]*100
    }
    if(var!="tasAdjust"){
      if(any(chg_q5<(-1))){
        warning("Lower bound forced to -100%")
        chg_q5[chg_q5<(-1)]=-1
      }
      exut$val[exut$horiz==h & exut$quant==quant[2]]=chg_mean*100
      exut$val[exut$horiz==h & exut$quant==quant[1]]=chg_q5*100
      exut$val[exut$horiz==h & exut$quant==quant[3]]=chg_q95*100
    }else{
      exut$val[exut$horiz==h & exut$quant==quant[2]]=chg_mean
      exut$val[exut$horiz==h & exut$quant==quant[1]]=chg_q5
      exut$val[exut$horiz==h & exut$quant==quant[3]]=chg_q95
    }
    exut$sign[exut$horiz==h ]=apply(resp,1,function(x) sum(x>0))/ncol(resp)*100
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
  quant.labs <- c("Projections\nbasses", "Projections\nintermédiaires", "Projections\nhautes")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  if(var!="tasAdjust"){
    q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
    q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg,na.rm=T)
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
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement\nrelatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif du ",ind_name_full," et\nson incertitude pour différents horizons et\nle prédicteur ",pred_name,"(",rcp_plainname," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        scale_pattern_density_manual("Accord sur le signe du\nchangement (Projections intermédiaires)",values = c("0"=0.4,"1"=0),drop=F,labels=c("<80%",">80%"))+
        theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20))
      if(var=="evspsblpotAdjust"){
        plt=plt+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(rev(precip_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
      }
    }else{
      plt=base_map_grid(data = exut,val_name = "val")
      plt=plt+
        facet_grid(horiz ~ quant,labeller = labeller(horiz = horiz.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement du ",ind_name_full," et\nson incertitude pour différents horizons et le\nprédicteur ",pred_name,"(",rcp_plainname," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
    }
  }
  if(pred=="temp"){
    emerg=plot_emergence(path_temp = path_temp,temp_ref = horiz,simu_lst =lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail,cat=cat)
    lay <- rbind(c(1,NA,NA,NA),
                 c(1,2,NA,NA),
                 c(1,2,3,NA),
                 c(1,2,NA,NA))
    plt=grid.arrange(plt, emerg[[1]],emerg[[2]], layout_matrix = lay,widths=c(2,0.7,0.6,0.05),heights=c(0.1,0.225,0.45,0.225))
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

## Will not work for predictor temperature (cannot do average +- 30 years)

map_3quant_3rcp_1horiz_basic=function(lst.QUALYPSOOUT,horiz,pred_name,pred,pred_unit,ind_name,ind_name_full,folder_out,freq_col=0.99,pix=F,var="toto",ref0=1990){

  ieff_rcp=which(colnames(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail)=="rcp")
  rcp_names=lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ieff_rcp]]
  quant=c("5%","mean","95%")
  idx_Xfut=which(lst.QUALYPSOOUT[[1]]$Xmat[1,]==horiz)
  idx_ref0=which(lst.QUALYPSOOUT[[1]]$Xmat[1,]==ref0)
  
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=data.frame(x=as.vector(ref$x),y=as.vector(ref$y))
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
  
  for(r in rcp_names){
    
    idx_rcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$scenAvail$rcp==r)
    ref0=apply(lst.QUALYPSOOUT[[1]]$Y[,idx_rcp,(idx_ref0-15):min(c(idx_ref0+15,dim(lst.QUALYPSOOUT[[1]]$Y)[3]))],MARGIN=1,mean,na.rm=T)
    
    if(var!="tasAdjust"){
      rel_chg=(apply(lst.QUALYPSOOUT[[1]]$Y[,idx_rcp,(idx_Xfut-15):min(c(idx_ref0+15,dim(lst.QUALYPSOOUT[[1]]$Y)[3]))],MARGIN=c(1,2),mean,na.rm=T)-ref0)/ref0*100
    }else{
      rel_chg=apply(lst.QUALYPSOOUT[[1]]$Y[,idx_rcp,(idx_Xfut-15):min(c(idx_ref0+15,dim(lst.QUALYPSOOUT[[1]]$Y)[3]))],MARGIN=c(1,2),mean,na.rm=T)-ref0
    }
    exut$val[exut$rcp==r & exut$quant==quant[2]]=apply(rel_chg,1,mean)
    exut$val[exut$rcp==r & exut$quant==quant[1]]=apply(rel_chg,1,quantile,probs=0.05)
    exut$val[exut$rcp==r & exut$quant==quant[3]]=apply(rel_chg,1,quantile,probs=0.95)
    
    exut$sign[exut$rcp==r]=apply(rel_chg,1,function(x) sum(x>0)/length(x)*100)
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
  quant.labs <- c("Projections\nbasses", "Projections\nintermédiaires", "Projections\nhautes")
  names(quant.labs) <- quant
  
  #Setting limits for color scale
  if(var!="tasAdjust"){
    q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
    q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg,na.rm=T)
    lim_col=round(lim_col/25)*25#arrondi au 25 le plus proche
  }else{
    # q99=quantile(exut$val,probs=freq_col)
    # q01=quantile(exut$val,probs=(1-freq_col))
    # lim_col=as.numeric(c(q01,q99))
    # lim_col=round(lim_col)#arrondi au 1 le plus proche
    lim_col=c(1,5) #forced to allow comparison with QUALYPSO
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
      facet_grid(rcp ~ quant,labeller = labeller(rcp=rcp.labs, quant = quant.labs))+
      ggtitle(paste0("Changement relatif du ",ind_name_full," et son incertitude pour\ndifférents RCPs et le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990),\n ensemble non balancé"))+
      theme(panel.border = element_rect(colour = "black",fill=NA))
    plt$layers[[3]]$aes_params$size= 3
    plt$layers[[4]]$aes_params$size= 3
  }else{
    if(var!="tasAdjust"){
      plt=base_map_grid(data = exut,val_name = "val",pattern_name = "sign_agree",facet_vert_name ="rcp",threshold="<80%",facet_horizontal_name="quant",exclude_horizontal=c("5%","95%"))
      plt=plt+
        facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif du ",ind_name_full," et\nson incertitude pour différents RCPs\net le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990),\n ensemble non balancé"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))+
        scale_pattern_density_manual("Accord sur le signe du\nchangement (Projections intermédiaires)",values = c("0"=0.4,"1"=0),drop=F,labels=c("<80%",">80%"))+
        theme(legend.key =element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
        guides(fill=guide_colorbar(barwidth = 2, barheight = 20))
      if(var=="evspsblpotAdjust"){
        plt=plt+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(rev(precip_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
      }
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
    exut=data.frame(x=as.vector(ref$x),y=as.vector(ref$y))
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
  

  idx_Xfut=which(lst.QUALYPSOOUT[[1]]$Xfut==horiz)
  if(var!="tasAdjust"){
    if(includeMean){
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT[[name_eff]]$MEAN*100 + lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN*100 #*100 for percentages
    }else{
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT[[name_eff]]$MEAN*100 #*100 for percentages
      if(!is.null(includeRCP)){
        ircp=which(lst.QUALYPSOOUT[[1]]$namesEff == "rcp")
        i_thisrcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ircp]]==includeRCP)
        chg = chg+lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT$rcp$MEAN[,i_thisrcp]*100 + lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN*100
      }
    }
  }else{
    if(includeMean){
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT[[name_eff]]$MEAN + lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN #*100 for percentages
    }else{
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT[[name_eff]]$MEAN #*100 for percentages
      if(!is.null(includeRCP)){
        ircp=which(lst.QUALYPSOOUT[[1]]$namesEff == "rcp")
        i_thisrcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ircp]]==includeRCP)
        chg = chg+lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT$rcp$MEAN[,i_thisrcp] + lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN
      }
    }
  }
  
  
  for(j in 1:ncol(chg)){
    exut$val[exut$effs==j]=chg[,j]
  }

  
  colnames(exut)=c("x","y","idx","effs","val")
  
  #Setting limits for color scale
  if(includeMean){
    ##all pixels, all effects, one time step
    tmp=unlist(lapply(lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT,function(x) x$MEAN+lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN))
  }else{
    tmp=unlist(lapply(lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT,function(x) x$MEAN))
    if(!is.null(includeRCP)){
      tmp=unlist(lapply(lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT,function(x) x$MEAN+lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN+lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT$rcp$MEAN[,i_thisrcp]))
    }
  }
  if(var!="tasAdjust"){
    tmp=tmp*100
  }
  if(var!="tasAdjust"|(is.null(includeRCP)&includeMean==F)){
    q99pos=quantile(tmp[tmp>=0],probs=freq_col)
    q99neg=abs(quantile(tmp[tmp<=0],probs=(1-freq_col)))
    lim_col=max(q99pos,q99neg,na.rm=T)
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
    br=seq(lim_col[1],lim_col[2],0.25)
  }
  
  if(!pix){
    plt=base_map_outlets(data = exut,val_name = "val")+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
  }else{
    plt=base_map_grid(data = exut,val_name = "val")+
      guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
  }
  
  if(includeMean){
    if(var!="tasAdjust"){
      plt=plt+
        facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))
      if(var=="evspsblpotAdjust"){
        plt=plt+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(rev(precip_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
      }
    }else{
      plt=plt+
        facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (°C))",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
        theme(panel.border = element_rect(colour = "black",fill=NA))
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
      if(var!="tasAdjust"){
        plt=plt+
          facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Effet principal (%)",ggplot2:::binned_pal(scales::manual_pal(temp_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),oob=squish,show.limits = T)+#that way because stepsn deforms colors
          ggtitle(paste0("Effet principaux des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
          theme(panel.border = element_rect(colour = "black",fill=NA))
      }else{
        plt=plt+
          facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Effet principal (%)",ggplot2:::binned_pal(scales::manual_pal(rev(temp_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels= c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),oob=squish,show.limits = T)+#that way because stepsn deforms colors
          ggtitle(paste0("Effet principaux des ",name_eff_plain,"s pour le ",ind_name_full,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
          theme(panel.border = element_rect(colour = "black",fill=NA))
      }
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
          theme(panel.border = element_rect(colour = "black",fill=NA))
        if(var=="evspsblpotAdjust"){
          plt=plt+
            binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(rev(precip_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
        }
      }else{
        plt=plt+
          facet_wrap(~effs,ncol=3,labeller = labeller(effs=effs.labs))+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement (%)",ggplot2:::binned_pal(scales::manual_pal(brewer.ylorrd(length(br)-1))),guide="coloursteps",limits=lim_col,breaks=br,oob=squish,show.limits = T,labels=c(paste0("< ",lim_col[1]),br[-c(1,length(br))],paste0("> ",lim_col[2])))+#that way because stepsn deforms colors
          ggtitle(paste0("Changement des ",name_eff_plain,"s pour le ",ind_name_full," le ",includeRCP,"\net le prédicteur ",pred_name," (",horiz," ",pred_unit," VS 1990)"))+
          theme(panel.border = element_rect(colour = "black",fill=NA))
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
    exut=data.frame(x=as.vector(ref$x),y=as.vector(ref$y))
  }
  exut$idx=seq(1:nrow(exut))
  exut$val=0
  
  idx_Xfut=which(lst.QUALYPSOOUT[[1]]$Xfut==horiz)
  
  if(vartype=="mean"){
    if(var!="tasAdjust"){
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN*100
    }else{
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN
    }
  }
  if(vartype=="varint"){
    if(var!="tasAdjust"){
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$INTERNALVAR*100^2#because variance is square of standard deviation unit
    }else{
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$INTERNALVAR
    }
    chg=sqrt(chg)
  }
  if(vartype=="varres"){
    if(var!="tasAdjust"){
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$RESIDUALVAR$MEAN*100^2
    }else{
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$RESIDUALVAR$MEAN
    }
    chg=sqrt(chg)
  }
  if(vartype=="vartot"){
    Veff = lst.QUALYPSOOUT[[idx_Xfut]]$EFFECTVAR
    if(var!="tasAdjust"){
      chg = apply(cbind(Veff, lst.QUALYPSOOUT[[idx_Xfut]]$RESIDUALVAR$MEAN,lst.QUALYPSOOUT[[idx_Xfut]]$INTERNALVAR),1,sum)*100^2
    }else{
      chg = apply(cbind(Veff, lst.QUALYPSOOUT[[idx_Xfut]]$RESIDUALVAR$MEAN,lst.QUALYPSOOUT[[idx_Xfut]]$INTERNALVAR),1,sum)
    }
    chg=sqrt(chg)
  }
  if(vartype=="incert"){# sans IV
    Veff = lst.QUALYPSOOUT[[idx_Xfut]]$EFFECTVAR
    if(var!="tasAdjust"){
      chg = apply(cbind(Veff, lst.QUALYPSOOUT[[idx_Xfut]]$RESIDUALVAR$MEAN),1,sum)*100^2
    }else{
      chg = apply(cbind(Veff, lst.QUALYPSOOUT[[idx_Xfut]]$RESIDUALVAR$MEAN),1,sum)
    }
    chg=sqrt(chg)
  }
  if(vartype=="rcp8.5"|vartype=="rcp85"){
    ircp=which(lst.QUALYPSOOUT[[1]]$namesEff == "rcp")
    i_thisrcp=which(lst.QUALYPSOOUT[[1]]$listScenarioInput$listEff[[ircp]]==vartype)
    if(var!="tasAdjust"){
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT$rcp$MEAN[,i_thisrcp]*100+lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN*100
    }else{
      chg=lst.QUALYPSOOUT[[idx_Xfut]]$MAINEFFECT$rcp$MEAN[,i_thisrcp]+lst.QUALYPSOOUT[[idx_Xfut]]$GRANDMEAN$MEAN
    }
  }
  exut$val=chg
  
  colnames(exut)=c("x","y","idx","val")
  
  ## We show uncertainties not variances
  
  if(!pix){
    plt=base_map_outlets(data = exut,val_name = "val")+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
  }else{
    plt=base_map_grid(data = exut,val_name = "val")+
      guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
  }
  if(vartype=="mean"){
    if(var!="tasAdjust"){
      q99pos=quantile(exut$val[exut$val>=0],probs=freq_col)
      q99neg=abs(quantile(exut$val[exut$val<=0],probs=(1-freq_col)))
      lim_col=max(q99pos,q99neg,na.rm=T)
      lim_col=round(lim_col/5)*5#arrondi au 5 le plus proche
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif moyen du ",ind_name_full,"\npour le prédicteur ",pred_name,"\n(",horiz," ",pred_unit," VS 1990)"))#+
      if(var=="evspsblpotAdjust"){
        plt=plt+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(rev(precip_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
      }
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
      lim_col=c(round(q01/0.25)*0.25,round(q99/0.25)*0.25)
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
      lim_col=c(round(q01/0.25)*0.25,round(q99/0.25)*0.25)
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
      lim_col=max(q99pos,q99neg,na.rm=T,na.rm=T)
      lim_col=round(lim_col/5)*5#arrondi au 5 le plus proche
      plt=plt+
        binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(precip_10)),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),oob=squish,show.limits = T,labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)))+#that way because stepsn deforms colors
        ggtitle(paste0("Changement relatif moyen du ",ind_name_full,"\npour le prédicteur ",pred_name," et le RCP 8.5\n(",horiz," ",pred_unit," VS 1990)"))
      if(var=="evspsblpotAdjust"){
        plt=plt+
          binned_scale(aesthetics = "fill",scale_name = "toto",name="Changement relatif (%)",ggplot2:::binned_pal(scales::manual_pal(rev(precip_10))),guide="coloursteps",limits=c(-lim_col,lim_col),breaks=seq(-lim_col,lim_col,length.out=11),labels=c(paste0("< -",lim_col),seq(-lim_col+lim_col/5,lim_col-lim_col/5,lim_col/5),paste0("> ",lim_col)),show.limits = T,oob=squish)#that way because stepsn deforms colors
      }
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

map_var_part=function(lst.QUALYPSOOUT,horiz,pred,pred_name,pred_unit,ind_name,ind_name_full,folder_out,pix=F,var="toto",title=T){
  
  if(pix){
    exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
    exut=exut[as.logical(refs$mask),]
  }else{
    exut=data.frame(x=as.vector(ref$x),y=as.vector(ref$y))
  }
  colnames(exut)=c("x","y")
  exut$idx=seq(1:nrow(exut))
  exut$gcm=exut$rcm=exut$rv=0
  if(pred=="time"){
    exut$rcp=0
  }
  if(!is.null(lst.QUALYPSOOUT[[1]]$CONTRIB_EACH_EFFECT$hm)){
    exut$hm=0
  }
  if(!is.null(lst.QUALYPSOOUT[[1]]$CONTRIB_EACH_EFFECT$bc)){
    exut$bc=0
  }
  

  idx_Xfut=which(lst.QUALYPSOOUT[[1]]$Xfut==horiz)
  if(pred=="time"){
    exut$rcp=lst.QUALYPSOOUT[[idx_Xfut]]$DECOMPVAR[,"rcp"]*100
  }
  exut$gcm=lst.QUALYPSOOUT[[idx_Xfut]]$DECOMPVAR[,"gcm"]*100
  exut$rcm=lst.QUALYPSOOUT[[idx_Xfut]]$DECOMPVAR[,"rcm"]*100
  if(!is.null(lst.QUALYPSOOUT[[1]]$CONTRIB_EACH_EFFECT$bc)){
    exut$bc=lst.QUALYPSOOUT[[idx_Xfut]]$DECOMPVAR[,"bc"]*100
  }
  if(!is.null(lst.QUALYPSOOUT[[1]]$CONTRIB_EACH_EFFECT$hm)){
    exut$hm=lst.QUALYPSOOUT[[idx_Xfut]]$DECOMPVAR[,"hm"]*100
  }
  exut$rv=lst.QUALYPSOOUT[[idx_Xfut]]$DECOMPVAR[,"ResidualVar"]*100
  exut$iv=lst.QUALYPSOOUT[[idx_Xfut]]$DECOMPVAR[,"InternalVar"]*100
  
  exut=pivot_longer(exut,cols=-c(x,y,idx),names_to="source",values_to = "val")
  exut=exut[order(exut$source),]
  labs_part=list("rv"="Variabilité Résiduelle","rcp"="RCP","gcm"="GCM","rcm"="RCM","bc"="Correction de biais","hm"="Modèle hydrologique")
  labs_part_labeller <- function(variable,value){
    return(labs_part[value])
  }
  
  lim_col1=as.numeric(round(quantile((exut[exut$source!="iv",]$val),probs=0.99),-1))
  lim_col2=as.numeric(round(quantile((exut[exut$source=="iv",]$val),probs=c(0.01,0.99)),-1))
  
  if(!pix){
    plt1=base_map_outlets(data = exut[exut$source!="iv",],val_name = "val")+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
  }else{
    plt1=base_map_grid(data = exut[exut$source!="iv",],val_name = "val")+
      guides(fill=guide_colorbar(barwidth = 2, barheight = 10,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
  }
  plt1=plt1+
    binned_scale(aesthetics = "fill",scale_name = "toto",name="Partition de variance (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=c(0,lim_col1),breaks=seq(0,lim_col1,length.out=6),show.limits = T,labels= c(0,round(seq(0+(lim_col1-0)/6,0+(lim_col1-0)/6*4,length.out=4),1),paste0("> ",lim_col1)),oob=squish)+#that way because stepsn deforms colors
    facet_wrap(vars(factor(source,levels=c("rv","rcp","gcm","rcm","bc","hm"))),labeller=labs_part_labeller )
  if(!pix){
    plt1$layers[[3]]$aes_params$size=3
  }
  
  exut$cat="Variabilité interne"
  if(!pix){
    plt2=base_map_outlets(data = exut[exut$source=="iv",],val_name = "val")+
      guides(fill = guide_bins(override.aes=list(size=7),axis = FALSE,show.limits = T,reverse=TRUE,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
  }else{
    plt2=base_map_grid(data = exut[exut$source=="iv",],val_name = "val")+
      guides(fill=guide_colorbar(barwidth = 2, barheight = 10,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
  }
  plt2=plt2+
    binned_scale(aesthetics = "fill",scale_name = "toto",name="Partition de variance (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelred_5)),guide="coloursteps",limits=lim_col2,breaks=seq(lim_col2[1],lim_col2[2],length.out=6),show.limits = T,labels= c(paste0("< ",lim_col2[1]),round(seq(lim_col2[1]+(lim_col2[2]-lim_col2[1])/5,lim_col2[2]-(lim_col2[2]-lim_col2[1])/5,length.out=4),1),paste0("> ",lim_col2[2])),oob=squish)+#that way because stepsn deforms colors
    facet_grid(. ~ cat)
  if(!pix){
    plt2$layers[[3]]$aes_params$size=3
  }
  
  
  # plt=ggarrange(plt1,ggarrange(ggplot()+theme_void(),plt2,ggplot()+theme_void(),heights=c(0.25,0.5,0.25),nrow=3),widths=c(0.6,0.4),ncol=2)
  # plt=ggarrange(plt1,plt2,ncol=2,widths=c(0.68,0.32))
  plt=ggarrange(plt1,plt2,nrow=2,heights=c(0.6,0.4))
  if(title==T){
    plt=annotate_figure(plt, top = text_grob(paste0("Partition de variance du ",ind_name_full,"\npour le prédicteur ",pred_name," (",horiz,pred_unit,")"), face = "bold", size = 18,hjust=0.5))+
      theme(panel.background = element_rect(fill="white"))
  }
  
  
  if (is.na(folder_out)){
    return(plt)
  }else{
    save.plot(plt,Filename = paste0("map_var_part_",ind_name,"_",pred,"_",horiz),Folder = folder_out,Format = "jpeg")
  }
  
}


##################################################################
## Emergence dates for reference in predictor temperature maps

plot_emergence=function(path_temp,ref_year=1990,simu_lst,temp_ref=c(1.5,2,3,4),cat){
  simu_lst$rcp="rcp85"
  data=prep_global_tas(path_temp,simu_lst=simu_lst,cat=cat)[["mat_Globaltas"]]
  idx=vector(mode = "list")
  for(temp in temp_ref){
    idx[[as.character(temp)]]=apply(data,MARGIN=2,function(x) min(which(x>=temp)))
  }
  
  idx=data.frame(do.call(rbind, idx))
  idx[idx==Inf]=NA
  years=prep_global_tas(path_temp,simu_lst=simu_lst,cat=cat)[["gcm_years"]]
  idx=data.frame(apply(idx,MARGIN=2,function(x) years[x]))
  idx$temp=temp_ref
  colnames(idx)[-ncol(idx)]=colnames(data)
  data=pivot_longer(data=idx,cols=!temp,names_to = "chain",values_to = "val")
  data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))
  data=data[data$rcp=="rcp85",]
  
  data_min=aggregate(data$val,by=list(data$temp), min,na.rm=T)
  colnames(data_min)=c("temp","val")
  data_max=aggregate(data$val,by=list(data$temp), max)
  colnames(data_max)=c("temp","val")
  data_max$val[is.na(data_max$val)]=">2100"#One GCM does not cross 4°C
  data_max$pos=aggregate(data$val,by=list(data$temp),max, na.rm=T)[,2]
  
  custom_boxplot=function(x){
    return(data.frame(ymin=min(x), ymax=max(x), upper=quantile(x,probs=0.75), lower=quantile(x,probs=0.25), middle=quantile(x,probs=0.5)))
  }
  
  plt1=ggplot(data)+
    stat_summary(fun.data = custom_boxplot,geom = "boxplot",aes(x=1,y=val),lwd=1.2,width=0.02)+
    # geom_point(aes(x=1,y=val),size=3,alpha=0.7)+
    xlab("")+
    ylab("")+
    theme_bw(base_size = 18)+
    theme(axis.ticks =element_blank(),axis.text =element_blank(),panel.grid = element_blank() )+
    theme(plot.title = element_text( face="bold",  size=16,hjust=0.5))+
    facet_wrap(vars(temp),nrow=4)+
    theme(strip.background = element_blank(),strip.text.x = element_blank())+
    scale_x_continuous("",limits = c(0.985,1.015),expand=c(0,0))+
    scale_y_reverse(limits = c(2110,2003),expand=c(0,0))+
    ggtitle("Date d'émergence")+
    geom_text(data=data_min,aes(x=1,y=val-7,label=val),fontface = "bold",size=4.5,color="red2")+
    geom_text(data=data_max,aes(x=1,y=pos+7,label=val),fontface = "bold",size=4.5,color="red2")
  
  plt2=ggplot(data.frame(x=rep(1,100),y=c(1:100)))+
    stat_summary(fun.data = custom_boxplot,geom = "boxplot",aes(x=x,y=y),lwd=2,width=0.02)+
    geom_text(aes(x=1.05,y=50,label="Quantile 50%"),fontface = "bold",size=4.5)+
    geom_text(aes(x=1.05,y=75,label="Quantile 75%"),fontface = "bold",size=4.5)+
    geom_text(aes(x=1.05,y=25,label="Quantile 25%"),fontface = "bold",size=4.5)+
    geom_text(aes(x=1.05,y=100,label="Maximum"),fontface = "bold",size=4.5)+
    geom_text(aes(x=1,y=105,label="Max"),fontface = "bold",size=4.5,color="red2")+
    geom_text(aes(x=1.05,y=0,label="Minimum"),fontface = "bold",size=4.5)+
    geom_text(aes(x=1,y=-5,label="Min"),fontface = "bold",size=4.5,color="red2")+
    scale_x_continuous("",limits = c(0.98,1.08),expand=c(0,0))+
    theme_void()+
    theme(panel.background = element_rect(colour="black"))+
    theme(plot.title = element_text( face="bold",  size=14,hjust=0.5))+
    scale_y_reverse()


  return(list(plt1,plt2))
}

