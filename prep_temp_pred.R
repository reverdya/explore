# Alix Reverdy
# Explore 2
# Prepare matrix of France temperatures for tempretaur predictor

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

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/indic/tasAdjust/"
pth_mask="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/SAFRAN_mask_France.nc"
centr_ref_year=1990# central year of 1975-2005 reference period

indic="yearmean"

###########
#FUNCTIONS#
###########



######
#MAIN#
######


nc=load_nc(pth_mask)
mask=ncvar_get(nc,varid="mask")
nc_close(nc)#for some reason stays opened otherwise
rm(nc)
gc()
mask[mask==0]=NA


lst_path=Sys.glob(paths=paste0(path_data,"*",indic,"*.nc"))

all_chain=vector(length=length(lst_path),mode="list")
for(i in 1:length(lst_path)){
  nc=load_nc(lst_path[i])
  res=ncvar_get(nc,varid="tasAdjust")
  full_years=nc$dim$time$vals
  if(grepl("ADAMONT",lst_path[i])){
    full_years=year(as.Date(full_years,origin="1950-01-01"))
  }
  if(grepl("CDFt",lst_path[i])){
    full_years=year(as.Date(full_years,origin="1850-01-01"))
  }
  nc_close(nc)#for some reason stays opened otherwise
  rm(nc)
  gc()
  

  ## now need to apply spline 1971-2099 with df=4 and adapt to our method
  
}

