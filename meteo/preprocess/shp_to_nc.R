# Alix Reverdy
# Explore 2
# transform a series of polygons (Hydrological basins) to a netcdf (safran grid)

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

path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/"


###########
#FUNCTIONS#
###########



######
#MAIN#
######

fr_8km=brick(paste0(path_sig,"raw/SAFRAN_mask_France.nc")) #brick is a function to read raster
crs(fr_8km)=crs("+init=epsg:27572")

###################
##Basins
basHy=readOGR(paste0(path_sig,"processed/BassinHydro.shp"), use_iconv = TRUE, encoding = "UTF-8")
basHy=spTransform(basHy,crs(fr_8km))
fr_8km_bas=vector(mode="list")
for(i in 1:length(basHy$gid)){
  fr_8km_bas[[i]]=mask(fr_8km,basHy[i,])#mask by cell centroid (allows to not have region overlap)
}

for(i in 1:length(basHy$gid)){
  plot(fr_8km_bas[[i]],alpha=0.5,col=viridis(96)[i])
  par(new=T)
}
lines(basHy)
fr_8km_bas=lapply(fr_8km_bas,function(x) t(drop(as.array(x)))[,ncol(t(drop(as.array(x)))):1])
fr_8km_bas=abind(fr_8km_bas,along = 3)
fr_8km_bas[!is.na(fr_8km_bas)]=1


########################################################################
# if ran several times need to manually delete the new netcdf
file.copy(paste0(path_sig,"raw/SAFRAN_mask_France.nc"),paste0(path_sig,"processed/SAFRAN_mask_basHy.nc"),overwrite=T)
nc=nc_open(paste0(path_sig,"processed/SAFRAN_mask_basHy.nc"),write = T)
bas=ncdim_def( "bas", "", 1:dim(fr_8km_bas)[3])
x=nc$dim$x
y=nc$dim$y
mask_bas=ncvar_def( 'Basins_hydro', '', list(x,y,bas), NA )
nc=ncvar_add(nc,mask_bas)
ncvar_put(nc,varid='Basins_hydro',vals = fr_8km_bas)
nc_close(nc)

## Make reference files
ref_bas=data.frame(id=basHy$gid,name=basHy$LbRegionHy)
write.csv(ref_bas,file = paste0(path_sig,"processed/SAFRAN_ref_basHy.csv"))
