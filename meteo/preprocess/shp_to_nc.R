# Alix Reverdy
# Explore 2
# transform a series of polygons (departments, BV and hydrological sectors) to a netcdf (safran grid)

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
##Departements
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

################
##Secteur hydro
sect_hyd=readOGR(paste0(path_sig,"processed/SecteurHydro_FXX_Explore2_20230222_buffer250m.shp"), use_iconv = TRUE, encoding = "UTF-8")
sect_hyd=spTransform(sect_hyd,crs(fr_8km))
fr_8km_sect_hyd=vector(mode="list")
for(i in 1:length(sect_hyd$LbRegionHy)){
  fr_8km_sect_hyd[[i]]=mask(fr_8km,sect_hyd[i,])#mask by cell centroid (allows to not have region overlap)
}

for(i in 1:length(sect_hyd$LbRegionHy)){
  plot(fr_8km_sect_hyd[[i]],alpha=0.5,col=viridis(172)[i])
  par(new=T)
}
lines(sect_hyd) #offset when zooming is probaly due to "reprojection à la volée"
#Combine
fr_8km_sect_hyd=lapply(fr_8km_sect_hyd,function(x) t(drop(as.array(x)))[,ncol(t(drop(as.array(x)))):1])#reorder because axis not same order and increasing order in ncvar_get and netcdf
fr_8km_sect_hyd=abind(fr_8km_sect_hyd,along = 3)
fr_8km_sect_hyd[!is.na(fr_8km_sect_hyd)]=1

################
##611 BV
BV=readOGR(paste0(path_sig,"raw/Explore2/shp_BV_evaluation_Explore2_26042023/shp_files_bv_eval_Explore2_26042023.shp"), use_iconv = TRUE, encoding = "UTF-8")
BV=spTransform(BV,crs(fr_8km))
fr_8km_bv=vector(mode="list")
for(i in 1:length(bv$Code8)){
  fr_8km_bv[[i]]=mask(fr_8km,BV[i,])#mask by cell centroid (allows to not have region overlap)
}

for(i in 1:length(BV$Code8)){
  plot(fr_8km_bv[[i]],alpha=0.5,col=viridis(611)[i])
  par(new=T)
}
lines(BV) #offset when zooming is probaly due to "reprojection à la volée"
#Combine
fr_8km_bv=lapply(fr_8km_bv,function(x) t(drop(as.array(x)))[,ncol(t(drop(as.array(x)))):1])#reorder because axis not same order and increasing order in ncvar_get and netcdf
fr_8km_bv=abind(fr_8km_bv,along = 3)
fr_8km_bv[!is.na(fr_8km_bv)]=1


########################################################################
# if ran several times need to manually delete the new netcdf
file.copy(paste0(path_sig,"raw/SAFRAN_mask_France.nc"),paste0(path_sig,"processed/SAFRAN_mask_sect-hydro_bv_deptmt.nc"),overwrite=T)
nc=nc_open(paste0(path_sig,"processed/SAFRAN_mask_sect-hydro_bv_deptmt.nc"),write = T)
sect=ncdim_def( "sect", "", 1:dim(fr_8km_sect_hyd)[3])
dep=ncdim_def( "dep", "", 1:dim(fr_8km_dep)[3])
bv=ncdim_def( "bv", "", 1:dim(fr_8km_bv)[3])
x=nc$dim$x
y=nc$dim$y
mask_sect <- ncvar_def( 'Hydro_sectors', '', list(x,y,sect), NA )
mask_dep=ncvar_def( 'Departments', '', list(x,y,dep), NA )
mask_bv=ncvar_def( 'Basins', '', list(x,y,bv), NA )
nc=ncvar_add(nc,mask_sect)
nc=ncvar_add(nc,mask_dep)
nc=ncvar_add(nc,mask_bv)
ncvar_put(nc,varid='Hydro_sectors',vals = fr_8km_sect_hyd)
ncvar_put(nc,varid='Departments',vals = fr_8km_dep)
ncvar_put(nc,varid='Basins',vals = fr_8km_bv)
nc_close(nc)

## Make reference files
ref_dep=data.frame(code=deptmt$CODE_DEPT,name=deptmt$NOM_DEPT)
write.csv(ref_dep,file = paste0(path_sig,"processed/SAFRAN_ref_deptmt.csv"))
ref_sect_hyd=data.frame(name=sect_hyd$LbSecteurH)
write.csv(ref_sect_hyd,file = paste0(path_sig,"processed/SAFRAN_ref_sect_hyd.csv"))
ref_bv=data.frame(code=BV$Code10,name=BV$NOM)
write.csv(ref_bv,file = paste0(path_sig,"processed/SAFRAN_ref_bv.csv"))

