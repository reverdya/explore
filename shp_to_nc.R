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

deptmt=readOGR(paste0(path_sig,"raw/IGN/GEOFLA_2-2_DEPARTEMENT_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2021-02-00129/GEOFLA_2-2_SHP_LAMB93_FR-ED161/DEPARTEMENT/LIMITE_DEPARTEMENT.shp"))
crs(deptmt)=crs("+init=epsg:2154")
deptmt=spTransform(deptmt,crs(fr_8km))
reg_hyd=readOGR(paste0(path_sig,"raw/IGN/RegionHydro_FXX-shp/RegionHydro_FXX.shp"))
crs(reg_hyd)=crs("+init=epsg:2154")
reg_hyd=spTransform(reg_hyd,crs(fr_8km))

fr_8km_reg_hyd=mask(fr_8km,reg_hyd)#mask is too strict and there is no submask

##netcdf mask with one layer per department and department numbers with reference folder
## problem with department being too long to reproject

