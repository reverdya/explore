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
path_data2="C:/Users/reverdya/Documents/Docs/2_data/"
pth_mask="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/SAFRAN_mask_France.nc"
pth_save="C:/Users/reverdya/Documents/Docs/2_data/processed/"
centr_ref_year=1990# central year of 1975-2005 reference period

indic="yearmean"

###########
#FUNCTIONS#
###########



######
#MAIN#
######


## Temlpérature globale corrigée

load(file=paste0(path_data2,"raw/Global_temp/T_GLO_RibesSA2021.Rdata"))
T_glo = X_Consc[,"be","all","histssp585","cons"]
years_glo=as.numeric(names(T_glo))
T_glo=T_glo-mean(T_glo[years_glo>=1850&years_glo<=1900])
T_glo_spline=smooth.spline(x=years_glo,y = T_glo,spar=0.8)$y
plot(years_glo,T_glo)
lines(years_glo,T_glo_spline,lwd=2)

## Temlpérature FR corrigée

load(file=paste0(path_data2,"raw/Global_temp/T_FR_RibesESD2022.Rdata"))
T_FR = CX_fulls[,"be","loc-ANN","all","histssp585","cons"]
years_FR=as.numeric(names(T_FR))
T_FR_spline=smooth.spline(x=years_FR,y = T_FR,spar=0.8)$y
plot(years_FR,T_FR)
lines(years_FR,T_FR_spline,lwd=2)
T_FR_spline1990=T_FR_spline-T_FR_spline[years_FR==1990] #changement rapport à 1990

## Relation FR et globale

plot(T_FR_spline1990,T_glo_spline)
T_lm=lm(T_glo_spline~T_FR_spline1990)
abline(T_lm,lwd=2)
print(summary(T_lm))
T_coef=c(T_lm$coefficients[[2]],T_lm$coefficients[[1]])
save(T_coef,file=paste0(pth_save,"T_coef_spline1990toGlob.Rdata"))



## Telmpératures FR par chaîne (données Explore2)

nc=load_nc(pth_mask)
mask=ncvar_get(nc,varid="masque")
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
  res=res*rep(mask,length(full_years))
  res=apply(res,MARGIN=3,mean,na.rm=T)
  res=res[full_years<=2099&full_years>=1971]
  full_years=full_years[full_years<=2099&full_years>=1971]
  res_spline=smooth.spline(x=full_years,y = res,df = 4)$y
  res_spline1990=res_spline-res_spline[full_years==1990]
  all_chain[[i]]=data.frame(year=full_years,temp_raw=res,temp_spline=res_spline,temp_spline1990=res_spline1990)
  names(all_chain)[i]=paste0(strsplit(lst_path[i],"_")[[1]][4],"_",strsplit(lst_path[i],"_")[[1]][5],"_",strsplit(lst_path[i],"_")[[1]][6],"_",strsplit(lst_path[i],"_")[[1]][7])
}

pred_temp=all_chain
save(pred_temp,file=paste0(pth_save,"pred_temp.Rdata"))



