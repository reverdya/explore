# Alix Reverdy
# Explore 2
# 04/04/2022
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

#####
##IPCC colors

yellow_blue_5=c(rgb(255,255,204,maxColorValue=255),rgb(161,218,180,maxColorValue=255),rgb(65,182,196,maxColorValue=255),rgb(44,127,184,maxColorValue=255),rgb(37,52,148,maxColorValue=255))
precip_5=c(rgb(166,97,26,maxColorValue=255),rgb(223,194,125,maxColorValue=255),rgb(245,245,245,maxColorValue=255),rgb(128,205,193,maxColorValue=255),rgb(1,133,113,maxColorValue=255))
temp_5=c(rgb(202,0,32,maxColorValue=255),rgb(244,165,130,maxColorValue=255),rgb(247,247,247,maxColorValue=255),rgb(146,197,222,maxColorValue=255),rgb(5,113,176,maxColorValue=255))
col_3rcp=c("#0000FF","#79BCFF","#FF0000")

###################################
## Format Global temperature for use in Qualypso, to be used inside code run_QUalypso
## Difference to 1850-1900 average and rcp/gcm matching
## path_data the root of the path
##simu_lst the list of simulations
#first_full year and last_full_year the first an last years with data for all simu all year round

format_global_tas=function(path_data,first_full_year,last_full_year,simu_lst){
  
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
  mat_Globaltas=t(do.call(cbind,mat_Globaltas))
  return(mat_Globaltas)
}


