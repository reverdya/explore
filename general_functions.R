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


