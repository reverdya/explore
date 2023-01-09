# Alix Reverdy
# Explore 2
# transform a series of polygons (departments and hydrological regions) to a netcdf (safran grid)

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

deptmt=readOGR(paste0(path_sig,"raw/IGN/GEOFLA_2-2_DEPARTEMENT_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2021-02-00129/GEOFLA_2-2_SHP_LAMB93_FR-ED161/DEPARTEMENT/DEPARTEMENT.shp"), use_iconv = TRUE, encoding = "UTF-8")
deptmt=spTransform(deptmt,crs(fr_8km))
fr_8km_dep=vector(mode="list")
for(i in 1:length(deptmt$CODE_DEPT)){
  fr_8km_dep[[i]]=mask(fr_8km,deptmt[i,])#mask by cell centroid (allows to not have region overlap)
}

for(i in 1:length(deptmt$CODE_DEPT)){
  plot(fr_8km_dep[[i]],alpha=0.5,col=viridis(96)[i])
  par(new=T)
}
lines(deptmt)

fr_8km_dep=lapply(fr_8km_dep,function(x) t(drop(as.array(x)))[,ncol(t(drop(as.array(x)))):1])
fr_8km_dep=abind(fr_8km_dep,along = 3)
fr_8km_dep[!is.na(fr_8km_dep)]=1

reg_hyd=readOGR(paste0(path_sig,"raw/IGN/RegionHydro_FXX-shp/RegionHydro_FXX.shp"), use_iconv = TRUE, encoding = "UTF-8")
reg_hyd=spTransform(reg_hyd,crs(fr_8km))
fr_8km_reg_hyd=vector(mode="list")
for(i in 1:length(reg_hyd$LbRegionHy)){
  fr_8km_reg_hyd[[i]]=mask(fr_8km,reg_hyd[i,])#mask by cell centroid (allows to not have region overlap)
}

for(i in 1:length(reg_hyd$LbRegionHy)){
  plot(fr_8km_reg_hyd[[i]],alpha=0.5,col=viridis(24)[i])
  par(new=T)
}
lines(reg_hyd)
##offset when zooming is probaly due to "reprojection à la volée"

fr_8km_reg_hyd=lapply(fr_8km_reg_hyd,function(x) t(drop(as.array(x)))[,ncol(t(drop(as.array(x)))):1])#reorder because axis not same order and increasing order in ncvar_get and netcdf
fr_8km_reg_hyd=abind(fr_8km_reg_hyd,along = 3)
fr_8km_reg_hyd[!is.na(fr_8km_reg_hyd)]=1

# if ran several times need to manually delete the new netcdf
file.copy(paste0(path_sig,"raw/SAFRAN_mask_France.nc"),paste0(path_sig,"processed/SAFRAN_mask_reg-hydro_deptmt.nc"),overwrite=T)
nc=nc_open(paste0(path_sig,"processed/SAFRAN_mask_reg-hydro_deptmt.nc"),write = T)
reg=ncdim_def( "reg", "", 1:dim(fr_8km_reg_hyd)[3])
dep=ncdim_def( "dep", "", 1:dim(fr_8km_dep)[3])
x=nc$dim$x
y=nc$dim$y
mask_reg <- ncvar_def( 'Hydro_regions', '', list(x,y,reg), NA )
mask_dep=ncvar_def( 'Departments', '', list(x,y,dep), NA )
nc=ncvar_add(nc,mask_reg)
nc=ncvar_add(nc,mask_dep)
ncvar_put(nc,varid='Hydro_regions',vals = fr_8km_reg_hyd)
ncvar_put(nc,varid='Departments',vals = fr_8km_dep)
nc_close(nc)

## Make reference files
ref_dep=data.frame(code=deptmt$CODE_DEPT,name=deptmt$NOM_DEPT)
write.csv(ref_dep,file = paste0(path_sig,"processed/SAFRAN_ref_deptmt.csv"))
ref_reg_hyd=data.frame(name=reg_hyd$LbRegionHy)
write.csv(ref_reg_hyd,file = paste0(path_sig,"processed/SAFRAN_ref_reg_hyd.csv"))

