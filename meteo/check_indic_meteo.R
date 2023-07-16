# Alix Reverdy
# Explore 2
# check meteorological indicators: fit of splines

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(plot.matrix)

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse-indic/"
pth_mask="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/SAFRAN_mask_France.nc"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/"
path_temp="C:/Users/reverdya/Documents/Docs/2_data/processed/"
load(file=paste0(path_temp,"T_coef_spline1990toGlob.Rdata"))

Var=vector(mode="list")
# Var[["tasAdjust"]]=c(rep("monmean",12),rep("seasmean",4),"yearmean")
# Var[["prtotAdjust"]]=c(rep("monsum",12),rep("seassum",4),"yearsum")
# Var[["evspsblpotAdjust"]]=c(rep("monsum",12),rep("seassum",4),"yearsum")
Var[["tasAdjust"]]=c(rep("seasmean",2))
Var[["prtotAdjust"]]=c(rep("seassum",2))
Var[["evspsblpotAdjust"]]=c(rep("seassum",2))
Var[["prsnAdjust"]]=c("NDJFMAsum")
# period=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12","_DJF","_MAM","_JJA","_SON","")
period=c("_DJF","_JJA","")
rcp=c("historical","rcp26","rcp45","rcp85")
bc=c("ADAMONT","CDFt")

units=c("°C","mm")

centr_ref_year=1990# central year of 1975-2005 reference period

basHy=read.csv(paste0(path_sig,"processed/SAFRAN_ref_basHy.csv"))


###########
#FUNCTIONS#
###########



######
#MAIN#
######


####################################################
## Make table of available simulations

simu_lst=vector(mode = "list",length=7)
names(simu_lst)=c("var","indic","period","rcp","gcm","rcm","bc")
for (v in names(Var)){
  for(r in rcp[-1]){# -1 for not historical
    for(b in bc){
      s=1
      if(v=="prsnAdjust"){
        s=length(period)
      }
      for(v2 in Var[[v]]){
        lst_f=list.files(paste0(path_data,"indic/",v,"/"),pattern=glob2rx(paste0(v,"*",r,"*",b,"*",v2,"*",period[s],"*"))) #list all files of projections in folder
        gcm=sapply(strsplit(lst_f,"_"),"[",4)
        rcm=sapply(strsplit(lst_f,"_"),"[",5)
        n=length(gcm)
        simu_lst$var=c(simu_lst$var,rep(v,n))
        simu_lst$period=c(simu_lst$period,rep(gsub('[_]', '', period[s]),n))
        if(period[s]==""){
          simu_lst$indic=c(simu_lst$indic,rep(paste0(v2),n))
        }else{
          simu_lst$indic=c(simu_lst$indic,rep(paste0(v2,period[s]),n))
        }
        simu_lst$rcp=c(simu_lst$rcp,rep(r,n))
        simu_lst$gcm=c(simu_lst$gcm,gcm)
        simu_lst$rcm=c(simu_lst$rcm,rcm)
        simu_lst$bc=c(simu_lst$bc,rep(b,n))
        s=s+1
      }
    }
  }
}
simu_lst=data.frame(sapply(simu_lst,unlist))
simu_lst=simu_lst[!(simu_lst$gcm=="IPSL-CM5A-MR"&simu_lst$rcm=="WRF381P"),]
simu_lst=simu_lst[!(simu_lst$gcm=="CNRM-CM5"&simu_lst$rcm=="RACMO22E"),]
# simu_lst[simu_lst$rcm=="REMO2009",]$rcm="REMO"# the 2 versions of REMO have been signaled as identical
# simu_lst[simu_lst$rcm=="REMO2015",]$rcm="REMO"
save(simu_lst,file=paste0(path_data,"simu_lst.Rdata"))

###################################################################################
## Prepare mask of 0 and 1 in shape of SAFRAN zone, lon, lat matrixes
## Also mask with altitude above 1000m for snowfall

nc=load_nc(pth_mask)
mask=ncvar_get(nc,varid="masque")
nc_close(nc)
rm(nc)
gc()

##For snowfall
pth_tmp=paste0(path_data,"indic/masks/mask_alti1000.nc")
nc=load_nc(pth_tmp)
mask_prsn=ncvar_get(nc,varid="height")
mask_prsn[!is.na(mask_prsn)]=1
mask_prsn[is.na(mask_prsn)]=0

lon=ncvar_get(nc,varid="lon")
lat=ncvar_get(nc,varid="lat")
x_l2=nc$dim$x$vals
y_l2=nc$dim$y$vals
X_l2=Y_l2=lon
for(i in 1:nrow(lon)){
  X_l2[i,]=x_l2[i]
}
for(i in 1:ncol(lon)){
  Y_l2[,i]=y_l2[i]
}

nc_close(nc)
rm(nc)
gc()

refs=list(mask,mask_prsn,lon,lat,X_l2,Y_l2)
names(refs)=c("mask","mask_prsn","lon","lat","x_l2","y_l2")
save(refs,file=paste0(path_data,"refs.Rdata"))
# plot(rotate(refs$mask))
# plot(rotate(refs$mask_prsn))

#######################################################################################
## Extract indexes of reference "cities"

ref_cities=read.xlsx(paste0(path_sig,"raw/French_cities/French_cities_coord.xlsx"))
ref_cities$col=ref_cities$row=ref_cities$xcoord
for (i in 1:nrow(ref_cities)){
  dist=sqrt((lon-ref_cities$xcoord[i])^2+(lat-ref_cities$ycoord[i])^2)
  min_dist=as.vector(which(dist==min(dist),arr.ind = T))
  ref_cities$row[i]=min_dist[1]
  ref_cities$col[i]=min_dist[2]
}

## by nature this plot will be rotated
# plot(refs$mask)
# points(ref_cities$col,nrow(refs$mask)-ref_cities$row,pch=19)

ref_cities=ref_cities[c(1,3,7),]

#########################################################################################
## Reference cities above 1000 m

ref_snow=data.frame(name=c("La Grave","Font-Romeu","Mont-Dore"),xcoord=c(6.30620,2.04383,2.80826),ycoord=c(45.04667,42.50592,45.57661))
ref_snow$col=ref_snow$row=ref_snow$xcoord
for (i in 1:nrow(ref_snow)){
  dist=sqrt((lon-ref_snow$xcoord[i])^2+(lat-ref_snow$ycoord[i])^2)
  min_dist=as.vector(which(dist==min(dist),arr.ind = T))
  ref_snow$row[i]=min_dist[1]
  ref_snow$col[i]=min_dist[2]
}

## by nature this plot will be rotated
# plot(refs$mask)
# points(ref_snow$col,nrow(refs$mask)-ref_snow$row,pch=19)

###############################################################################################
## Plot raw indicator , and its spline for all models and selection of places by RCP for time
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
# type : raw_spline, raw, diff, diff_spline

##Merge data frame warnings are okay


memory_saving_function=function(cpt){
  tic()
  dir.create(paste0(path_fig,lst_indic$v[cpt],"/",lst_indic$indic[cpt]))

  scenAvail=simu_lst[simu_lst$var==lst_indic$v[cpt] & simu_lst$indic==lst_indic$indic[cpt],]
  global_tas=prep_global_tas(path_temp,simu_lst=scenAvail)
  assign("global_tas",global_tas,envir = globalenv())
  all_chains=extract_chains(scenAvail=scenAvail,ref_cities=lst_indic$ref_c[[cpt]])
  for(c in 1:nrow(lst_indic$ref_c[[cpt]])){
    for(R in c("rcp26","rcp45","rcp85")){
      for(S in c(0.8,0.9,1,1.1,1.2)){
        plot_spline(all_chains=all_chains,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name =lst_indic$ref_c[[cpt]]$name[c],idx=c)
      }
    }
    for(R in c("rcp85")){
      for(S in c(1.2,1.3,1.4,1.5,1.6)){
        plot_spline(all_chains=all_chains,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = lst_indic$ref_c[[cpt]]$name[c],globaltas = global_tas,idx=c)
      }
    }
  }
  rm(all_chains,global_tas)
  gc()
  print(lst_indic$indic[cpt])
  toc()
}

lst_indic=list(indic=c(),v=c(),ref_c=list())
count=1
for(v in unique(simu_lst$var)){
  dir.create(paste0(path_fig,v,"/"))
  if(v=="prsnAdjust"){
    lst_indic$ref_c[[count]]=ref_snow
    count=count+1
    lst_indic$indic=c(lst_indic$indic,unique(simu_lst[simu_lst$var==v,]$indic))
    lst_indic$v=c(lst_indic$v,v)
  }else{
    # lgth=length(unique(simu_lst[simu_lst$var==v,]$indic)[c(1,4,7,10,17)])# to reduce number of indic plotted
    lgth=length(unique(simu_lst[simu_lst$var==v,]$indic))
    for(j in 1:lgth){
      lst_indic$ref_c[[count]]=ref_cities
      count=count+1
    }
    # lst_indic$indic=c(lst_indic$indic,unique(simu_lst[simu_lst$var==v,]$indic)[c(1,4,7,10,17)])
    lst_indic$indic=c(lst_indic$indic,unique(simu_lst[simu_lst$var==v,]$indic))# to reduce number of indic plotted
    lst_indic$v=c(lst_indic$v,rep(v,lgth))
  }
}


#Strangely sometimes the functions that does the restart does not work if the code is ran all at once and need to be launched individually...
restart_loop(fct=memory_saving_function,last=length(lst_indic$indic),step=1)
  
  
################################################
## Idem for basins hydro

basHy=basHy[c(1,3,4),]

memory_saving_function=function(cpt){
  tic()
  dir.create(paste0(path_fig,lst_indic$v[cpt],"/",lst_indic$indic[cpt]))
  dir.create(paste0(path_fig,lst_indic$v[cpt],"/",lst_indic$indic[cpt],"/bas/"))

  scenAvail=simu_lst[simu_lst$var==lst_indic$v[cpt] & simu_lst$indic==lst_indic$indic[cpt],]
  global_tas=prep_global_tas(path_temp,simu_lst=scenAvail)
  assign("global_tas",global_tas,envir = globalenv())
  all_chains_bas=extract_chains(scenAvail,ref_cities = basHy,type = "bas")
  for(c in 1:nrow(basHy)){
    for(R in c("rcp26","rcp85")){
      for(S in c(1,1.1)){
        plot_spline(all_chains=all_chains_bas,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = basHy$name[c],cat="meteo",idx=c,place="bas")
      }
    }
    for(R in c("rcp85")){
      for(S in c(1.2,1.3,1.4,1.5,1.6)){
        plot_spline(all_chains=all_chains_bas,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = basHy$name[c],globaltas = global_tas,cat="meteo",idx=c,place="bas")
      }
    }
  }
  rm(all_chains_bas,global_tas)
  gc()
  print(lst_indic$indic[cpt])
  toc()
}


lst_indic=list(indic=c(),v=c())
for(v in unique(simu_lst$var)[unique(simu_lst$var)!="prsnAdjust"]){
  dir.create(paste0(path_fig,v,"/"))
  # lgth=length(unique(simu_lst[simu_lst$var==v,]$indic)[c(1,4,7,10,17)])
  # lst_indic$indic=c(lst_indic$indic,unique(simu_lst[simu_lst$var==v,]$indic)[c(1,4,7,10,17)])
  lgth=length(unique(simu_lst[simu_lst$var==v,]$indic))
  lst_indic$indic=c(lst_indic$indic,unique(simu_lst[simu_lst$var==v,]$indic))
  lst_indic$v=c(lst_indic$v,rep(v,lgth))
}



#Strangely sometimes the functions that does the restart does not work if the code is ran all at once and need to be launched individually...
restart_loop(fct=memory_saving_function,last=length(lst_indic$indic),step=1)
