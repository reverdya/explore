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
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"

Var=vector(mode="list")
Var[["tasAdjust"]]=c("seasmean","seasmean","seasmean","seasmean","yearmean")
# Var[["tasAdjust"]]=c(rep("monmean",12),"seasmean","seasmean","seasmean","seasmean","yearmean")
# Var[["prtotAdjust"]]=c(rep("monsum",12),"seassum","seassum","seassum","seassum","yearsum",rep("monocc",12),"seasocc","seasocc","seasocc","seasocc","yearocc")
Var[["prtotAdjust"]]=c("seassum","seassum","seassum","seassum","yearsum")
# Var[["evspsblpotAdjust"]]=c(rep("monsum",12),"seassum","seassum","seassum","seassum","yearsum")
Var[["evspsblpotAdjust"]]=c("seassum","seassum","seassum","seassum","yearsum")
# Var[["prsnAdjust"]]=c(rep("monsum",12),"seassum","seassum","seassum","seassum","yearsum",rep("monocc",12),"seasocc","seasocc","seasocc","seasocc","yearocc")
# Var[["prsnAdjust"]]=c(rep("monsum",12),"seassum","seassum","seassum","seassum","yearsum")
Var[["prsnAdjust"]]=c("NDJFMAsum")
# period=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12","_DJF","_MAM","_JJA","_SON","")
period=c("_DJF","_MAM","_JJA","_SON","")
rcp=c("historical","rcp26","rcp45","rcp85")
bc=c("ADAMONT","CDFt")

units=c("°C","mm")

centr_ref_year=1990# central year of 1975-2005 reference period



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
        s=5
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
simu_lst=data.frame(simu_lst)
# simu_lst[simu_lst$rcm=="REMO2009",]$rcm="REMO"# the 2 versions of REMO have been signaled as identical
# simu_lst[simu_lst$rcm=="REMO2015",]$rcm="REMO"
save(simu_lst,file=paste0(path_data,"simu_lst.Rdata"))

###################################################################################
## Prepare mask of 0 and 1 in shape of SAFRAN zone, lon, lat matrixes
## Also mask with altitude above 1000m for snowfall

pth_tmp=list.files(paste0(path_data,"indic/",simu_lst$var[1],"/"),full.names=T)[18]#18 to get cdft and lambert coordinates
nc=load_nc(pth_tmp)
res=ncvar_get(nc,varid=simu_lst$var[1])
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
mask=res[,,1]
mask[!is.na(mask)]=1
mask[is.na(mask)]=0

##For snowfall
pth_tmp=paste0(path_data,"/indic/masks/mask_alti1000.nc")
nc=load_nc(pth_tmp)
res=ncvar_get(nc,varid="height")
nc_close(nc)
rm(nc)
gc()
mask_prsn=res
mask_prsn[!is.na(mask_prsn)]=1
mask_prsn[is.na(mask_prsn)]=0

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

#########################################################################################
## Reference departments and zones

ref_dep=read.csv(paste0(path_sig,"processed/SAFRAn_ref_deptmt.csv"))
colnames(ref_dep)=c("id","code","name")
idx_ref_dep=c(12,34,64,65,75)
ref_reg=read.csv(paste0(path_sig,"processed/SAFRAn_ref_reg_hyd.csv"))
colnames(ref_reg)=c("id","name")
idx_ref_reg=c(1,4,139)

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
## Sometimes problem with too many connections opened requires running step by step (v by v)


for(v in unique(simu_lst$var)){
  dir.create(paste0(path_fig,v,"/"))
  if(v=="prsnAdjust"){
    ref_c=ref_snow
  }else{
    ref_c=ref_cities
  }
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
    closeAllConnections()
    gc()
    dir.create(paste0(path_fig,v,"/",i))
    
    scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
    global_tas=prep_global_tas(path_temp,ref_year=centr_ref_year,simu_lst=scenAvail)
    all_chains=extract_chains(scenAvail=scenAvail,ref_cities=ref_c)
    for(c in 1:nrow(ref_c)){
      for(R in c("rcp26","rcp45","rcp85")){
        # plot_spline(all_chains=all_chains,type="diff",pred="time",scenAvail = scenAvail,SPAR=1,rcp=R,city_name = ref_c$name[c],idx=c)
        # plot_spline(all_chains=all_chains,type="raw",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],idx=c)
        for(S in c(0.8,0.9,1,1.1,1.2)){
          plot_spline(all_chains=all_chains,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],idx=c)
          # plot_spline(all_chains=all_chains,type="diff_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],idx=c)
        }
      }
      for(R in c("rcp85")){
        # plot_spline(all_chains=all_chains,type="raw",pred="temp",scenAvail = scenAvail,SPAR=1,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
        # plot_spline(all_chains=all_chains,type="diff",pred="temp",scenAvail = scenAvail,SPAR=1,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
        for(S in c(1.2,1.3,1.4,1.5,1.6)){
          plot_spline(all_chains=all_chains,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
          # plot_spline(all_chains=all_chains,type="diff_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
        }
      }
    }
  }
}
  
## Idem for regions hydro and departments


for(v in unique(simu_lst$var)[unique(simu_lst$var)!="prsnAdjust"]){
  dir.create(paste0(path_fig,v,"/"))
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
    closeAllConnections()
    gc()
    dir.create(paste0(path_fig,v,"/",i))
    dir.create(paste0(path_fig,v,"/",i,"/reg/"))
    dir.create(paste0(path_fig,v,"/",i,"/dep/"))
    
    scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
    global_tas=prep_global_tas(path_temp,ref_year=centr_ref_year,simu_lst=scenAvail)
    all_chains_reg=extract_chains(scenAvail,ref_cities = ref_reg[idx_ref_reg,],type = "reg")
    all_chains_dep=extract_chains(scenAvail,ref_cities = ref_dep[idx_ref_dep,],type = "dep")
    for(c in 1:nrow(ref_reg[idx_ref_reg,])){
      for(R in c("rcp26","rcp45","rcp85")){
        for(S in c(0.8,0.9,1,1.1,1.2)){
          plot_spline(all_chains=all_chains_reg,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_reg$name[idx_ref_reg[c]]),categ="reg",idx=c)
        }
      }
      for(R in c("rcp85")){
        for(S in c(1.2,1.3,1.4,1.5,1.6)){
          plot_spline(all_chains=all_chains_reg,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_reg$name[idx_ref_reg[c]]),globaltas = global_tas,categ="reg",idx=c)
        }
      }
    }
    for(c in 1:nrow(ref_dep[idx_ref_dep,])){
      for(R in c("rcp26","rcp45","rcp85")){
        for(S in c(0.8,0.9,1,1.1,1.2)){
          plot_spline(all_chains=all_chains_dep,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_dep$name[idx_ref_dep[c]]),categ="dep",idx=c)
        }
      }
      for(R in c("rcp85")){
        for(S in c(1.2,1.3,1.4,1.5,1.6)){
          plot_spline(all_chains=all_chains_dep,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_dep$name[idx_ref_dep[c]]),globaltas = global_tas,categ="dep",idx=c)
        }
      }
    }
  }
}

##########################################################################################
## Check season prsnadjust for % of null chains over 20,40 and 60% of 2070-2100 and map
# 
# v="prsnAdjust"
# count_0=list()
# dir.create(paste0(path_fig,"count_0/"))
# 
# for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(1,2,3,4)]){
#   closeAllConnections()
#   gc()
#   scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i & simu_lst$rcp=="rcp85",]
#   all_chains=vector(length=nrow(scenAvail),mode="list")
#   for(c in 1:nrow(scenAvail)){# for each chain
#     
#     pth_tmp=list.files(paste0(path_data,"indic/",v,"/"),full.names=T,pattern=glob2rx(paste0(v,"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c],"_")[[1]][1],"*",scenAvail$period[c],"*")))
#     nc=load_nc(pth_tmp)
#     res=ncvar_get(nc,varid=v)
#     full_years=nc$dim$time$vals
#     if(scenAvail$bc[c]=="ADAMONT"){
#       full_years=year(as.Date(full_years,origin="1950-01-01"))
#     }
#     if(scenAvail$bc[c]=="CDFt"){
#       full_years=year(as.Date(full_years,origin="1850-01-01"))
#     }
#     nc_close(nc)#for some reason stays opened otherwise
#     rm(nc)
#     gc()
#     
#     vec_mask=as.logical(refs$mask)
#     dim(res)=c(dim(res)[1]*dim(res)[2],dim(res)[3])# collapses one dimension
#     res=res[vec_mask,]
#     res=cbind(full_years,t(res))
#     all_chains[[c]]=res[which(full_years>=2070),c(-1)]
#   }
#   
#   all_chains=lapply(all_chains,function(x) apply(x,MARGIN=2,function(y) sum(y==0)/length(y)*100))
#   all_chains_df=as.data.frame(do.call(rbind,all_chains))
#   thresh=c(20,40,60)
#   tmp_df=data.frame(matrix(nrow=ncol(all_chains_df),ncol = length(thresh)))
#   for(j in 1:length(thresh)){
#     tmp_df[,j]=apply(all_chains_df,MARGIN = 2,function(x) sum(x>=thresh[j])/length(x)*100)
#   }
#   colnames(tmp_df)=paste0("thresh_",thresh)
#   tmp_df$season=i
#   count_0[[i]]=tmp_df
# }
# 
# count_0=as.data.frame(do.call(rbind,count_0))
# count_0=pivot_longer(count_0,cols=!season,names_to = "thresh",values_to = "freq")
# count_0=count_0[order(count_0$thresh),]
# 
# 
# exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
# exut=exut[as.logical(refs$mask),]
# exut$idx=seq(1:nrow(exut))
# tmp=exut
# for (j in 1:((3*4)-1)){
#   exut=rbind(exut,tmp)
# }
# exut$thresh=count_0$thresh
# exut$season=factor(count_0$season,levels=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16)])
# exut$val=count_0$freq
# 
# thresh.labs <- c("20% de 2070-2100","40% de 2070-2100","60% de 2070-2100")
# names(thresh.labs) <- paste0("thresh_",thresh)
# season.labs <- c("DJF","MAM","JJA","SON")
# names(season.labs) <- unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16)]
# 
# plt=base_map_grid(data = exut,val_name = "val")
# plt=plt+
#   facet_grid(thresh ~ season,labeller = labeller(thresh = thresh.labs, season = season.labs))+
#   binned_scale(aesthetics = "fill",scale_name = "toto",name="Part des\nchaînes (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=c(0,100),breaks=seq(0,100,20),show.limits = T,oob=squish)+#that way because stepsn deforms colors
#   ggtitle(paste0("Part des chaînes avec au moins 20%, 40% ou 60% de cumuls de\nprécipitation neigeuse saisonniers nuls entre 2070 et 2100 (RCP 8.5)"))+
#   theme(panel.border = element_rect(colour = "black",fill=NA))+
#   theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
#   guides(fill=guide_colorbar(barwidth = 2, barheight = 20))
# save.plot(plt,Filename = paste0("map_prsn_count0"),Folder = paste0(path_fig,"count_0/"),Format = "jpeg")
