# Alix Reverdy
# Explore 2
# transform a series of polygons (departments and hydrological regions) to a netcdf (safran grid)

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(ncdf4)
library(abind)

########
#SOURCE#
########

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/raw/meteo/safran/"


###########
#FUNCTIONS#
###########


vector_to_grid=function(x_old,y_old,old,idx){
  grid=expand.grid(c(unique(x_old),136000),unique(y_old))#136000 is missing otherwise
  colnames(grid)=c("x","y")
  tmp=data.frame(x=x_old,y=y_old,val=old[,idx])
  tmp=merge(grid,tmp,by=c("x","y"),all=T)
  tmp=tmp[order(tmp$x),]
  tmp=tmp[order(tmp$y),]
  return(matrix(tmp$val,nrow=143,ncol=134))
}

######
#MAIN#
######

dir.create(paste0(path_data,"regridded/"))
lst_f=list.files(path_data,pattern=glob2rx("safran*"),full.names = T)[c(2,4,5)]
varids=c("prtotAdjust","prsnAdjust","tasAdjust")
varids_old=c("Rain","Snow","Tair")


for(f in length(lst_f)){

  
  nc_old=nc_open(lst_f[f])
  old=ncvar_get(nc_old,varids_old[f])
  x_old=ncvar_get(nc_old,"LambXg")
  y_old=ncvar_get(nc_old,"LambYg")
  time=nc_old$dim$Time
  nc_close(nc_old)
  new=vector(mode="list",length=dim(old)[2])
  for(i in 1:length(new)){
    new[[i]]=vector_to_grid(x_old=x_old,y_old=y_old,old=old,idx=i)
    if(i%%100==0){print(i)}
  }
  new=abind(new,along = 3)
  rm(old)
  gc()

  
  nc_new=nc_open(paste0(path_data,"safran_new_ETP.nc"))
  x=nc_new$dim$X
  y=nc_new$dim$Y
  nc_close(nc_new)
  new_var <- ncvar_def( varids[f], "units", list(x,y,time) )
  nc=nc_create(paste0(path_data,"regridded/",gsub(".nc","_grid.nc",basename(lst_f[f]))),new_var)
  ncvar_put(nc,varid = varids[f],vals = new)
  nc_close(nc)
  
}


