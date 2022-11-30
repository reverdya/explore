# Alix Reverdy
# Explore 2
# Compare ADAMONT and R2D2-2L

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(scattermore)#geom_scattermore

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse-indic/compare_BC/"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/French_cities/"
path_raw="C:/Users/reverdya/Documents/Docs/2_data/raw/meteo_Explore2/"

centr_ref_year=1990# central year of 1975-2005 reference period
first_ref_year=1975
last_ref_year=2005

load(paste0(path_data,"simu_lst.Rdata"))

###########
#FUNCTIONS#
###########



######
#MAIN#
######


###################################################################################
## Prepare mask of 0 and 1 in shape of SAFRAN zone, lon, lat matrixes and time vector

pth_tmp=list.files(paste0(path_data,"indic/",simu_lst$var[1],"/"),full.names=T)[18]#18 to get r2d2 and lambert coordinates
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
rm(nc)
gc()
mask=res[,,1]
mask[!is.na(mask)]=1
mask[is.na(mask)]=0
refs=list(mask,lon,lat,X_l2,Y_l2)
names(refs)=c("mask","lon","lat","x_l2","y_l2")
save(refs,file=paste0(path_data,"refs.Rdata"))
plot(rotate(refs$mask))

#######################################################################################
## Extract indexes of reference "cities"

ref_cities=read.xlsx(paste0(path_sig,"French_cities_coord.xlsx"))
ref_cities$col=ref_cities$row=ref_cities$xcoord
for (i in 1:nrow(ref_cities)){
  dist=sqrt((lon-ref_cities$xcoord[i])^2+(lat-ref_cities$ycoord[i])^2)
  min_dist=as.vector(which(dist==min(dist),arr.ind = T))
  ref_cities$row[i]=min_dist[1]
  ref_cities$col[i]=min_dist[2]
}

plot(refs$mask)
points(ref_cities$col,nrow(refs$mask)-ref_cities$row,pch=19)


###############################################################################################
## Plot raw indicator for seasonal time series, BC together


##Merge data frame warnings are okay

## Problem with too many connections opened requires running step by step (v by v)

for(v in unique(simu_lst$var)[1:2]){
  dir.create(paste0(path_fig,v,"/"))
  if (v!="tasAdjust"){
    # stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16,30,31,32,33)]#outdated because not doing ocurences anymore
    stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16)]
  }else{
    stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16)]
  }
  
  
  for (i in stats){
    closeAllConnections()
    gc()
    scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
    all_chains=vector(length=nrow(scenAvail),mode="list")
    for(c in 1:nrow(scenAvail)){# for each chain
      
      pth_tmp=list.files(paste0(path_data,"indic/",v,"/"),full.names=T,pattern=glob2rx(paste0(v,"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c],"_")[[1]][1],"*",scenAvail$period[c],"*")))
      nc=load_nc(pth_tmp)
      res=ncvar_get(nc,varid=v)
      full_years=nc$dim$time$vals
      if(scenAvail$bc[c]=="ADAMONT"){
        full_years=year(as.Date(full_years,origin="1950-01-01"))
      }
      if(scenAvail$bc[c]=="R2D2"){
        full_years=year(as.Date(full_years,origin="1850-01-01"))
      }
      nc_close(nc)#for some reason stays opened otherwise
      rm(nc)
      gc()
      res2=data.frame(matrix(nrow=dim(res)[3],ncol=nrow(ref_cities)+1))
      res2[,1]=full_years
      for (j in 1 :nrow(ref_cities)){
        res2[,j+1]=res[ref_cities$row[j],ref_cities$col[j],]
      }
      colnames(res2)[1]="year"
      all_chains[[c]]=res2
      rm(res)
      rm(res2)
      gc()
    }
    
    for(cities in 2:(nrow(ref_cities)+1)){
      
      ClimateProjections=lapply(all_chains, function(x) x[,c(1,cities)])
      Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections))
      Y=Y[,Y[1,]<=2100]
      X=Y[1,]
      Y=Y[-1,]
      nS=nrow(scenAvail)
      raw=data.frame(t(Y))
      colnames(raw)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
      raw[is.na(t(Y))]=NA
      raw$year=X
      raw=pivot_longer(data=raw,cols=!year,names_to = "model",values_to = "val")
      raw$type="raw"
      
      data=raw
      data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
      data$mod=paste0(unlist(lapply(strsplit(data$model,"_"),function(x) x[2])),"/",unlist(lapply(strsplit(data$model,"_"),function(x) x[3])))
      data$bc=unlist(lapply(strsplit(data$model,"_"),function(x) x[4]))
      
      
      if(v!="tasAdjust"){
        if(i %in% c("seasocc_DJF","seasocc_MAM","seasocc_JJA","seasocc_SON")){
          ylabel="Nb occurence"
          unit=""
        }else{
          ylabel="Prtot"
          unit=" (mm)"
        }
      }else{
        ylabel="Tas"
        unit=" (K)"
      }
      
      for (r in unique(data$rcp)){
        plt=ggplot(data[data$rcp==r,])+#Warnings okay
          geom_line(aes(x=year,y=val,color=bc),size=0.75)+
          scale_color_manual("BC",values=c("red","blue"))+
          theme_bw(base_size = 18)+
          theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
          theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
          scale_x_continuous("")+
          scale_y_continuous(paste0(ylabel,unit),expand=c(0,0))+
          guides(color = guide_legend(override.aes = list(size = 1.7)))+
          facet_wrap(vars(mod),nrow=7)+
          theme(panel.spacing.x = unit(0.1, "lines"))+
          theme(strip.text.y = element_text(size = 9))
        save.plot(plt,Filename = paste0(v,"_",i,"_chronique",ref_cities$name[cities-1],"_",r),Folder = paste0(path_fig,v,"/"),Format = "jpeg")
      }
    }
  }
}


##############################################################################
## Plot indicator for all pixels, all_chains and all times, scatter plot BC by season

cpt=1
for(v in unique(simu_lst$var)[1:2]){
  if (v!="tasAdjust"){
    stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16,30,31,32,33)]
    # stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,30)]
  }else{
    stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16)]
    # stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13)]
    bins_size=c(0.1,0.1)
    br=c(100,1000,5000,50000)
  }
  for (i in stats){
    if (v!="tasAdjust"){
      if(cpt <= 4){
      # if(cpt <= 1){
        bins_size=c(10,10)
        br=c(100,1000,5000,50000)
      }else{
        bins_size=c(1,1)
        br=c(100,1000,5000,50000)
      }
      cpt=cpt+1
    }
    closeAllConnections()
    gc()
    scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
    all_chains=vector(length=nrow(scenAvail),mode="list")
    for(c in 1:nrow(scenAvail)){# for each chain
      pth_tmp=list.files(paste0(path_data,"indic/",v,"/"),full.names=T,pattern=glob2rx(paste0(v,"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c],"_")[[1]][1],"*",scenAvail$period[c],"*")))
      nc=load_nc(pth_tmp)
      all_chains[[c]]=ncvar_get(nc,varid=v)
      nc_close(nc)#for some reason stays opened otherwise
      rm(nc)
      gc()
    }
    # r="rcp85"
    for (r in unique(scenAvail$rcp)){
      df_bc=data.frame(ADAMONT=unlist(all_chains[scenAvail$bc=="ADAMONT" & scenAvail$rcp==r]),R2D2=unlist(all_chains[scenAvail$bc=="R2D2" & scenAvail$rcp==r]))
      df_bc=df_bc[!is.na(df_bc$R2D2),]
      
      # plt=ggplot(df_bc)+
      #   geom_scattermore(aes(x=R2D2,y=ADAMONT),pch='.',alpha=0.1)+
      #   geom_abline(slope=1, intercept=0,color="red")+
      #   theme_bw(base_size = 18)+
      #   theme( axis.line = element_line(colour = "black"),panel.border = element_blank())
      
      trans_4rt <- trans_new(name = "4th root",
                              transform = function(x) x ^ (1/4),
                              inverse = function(x) x ^ 4)
      
      plt=ggplot(df_bc)+
        geom_bin2d(aes(x=R2D2,y=ADAMONT),binwidth=bins_size,na.rm=T)+
        scale_fill_gradientn("Count",colours = viridis(25), trans=trans_4rt,breaks=br)+
        coord_equal()+
        geom_abline(slope=1, intercept=0,color="red",size=1)+
        theme_bw(base_size = 18)+
        theme( axis.line = element_line(colour = "black"),panel.border = element_blank())
      if(v!="tasAdjust"){
        plt=plt+
          geom_abline(slope=1.1, intercept=0,color="red",linetype="dashed",size=1)+
          geom_abline(slope=0.9, intercept=0,color="red",linetype="dashed",size=1)
      }else{
        plt=plt+
          geom_abline(slope=1, intercept=0.5,color="red",linetype="dashed",size=1)+
          geom_abline(slope=1, intercept=-0.5,color="red",linetype="dashed",size=1)
      }
      save.plot(plt,Filename = paste0(v,"_",i,"_scatter_",r),Folder = paste0(path_fig,v,"/"),Format = "jpeg")
      
      rm(df_bc,plt)
      gc()
    }
  }
}



##############################################
## Plot raw variable for several years and a chain 

##Prtot

nc=load_nc(paste0(path_raw,"prtotAdjust_FR_rcp85_HadGEM2-ES_RegCM4-6_ADAMONT_day_2006-2099.nc"))
adamont=ncvar_get(nc,varid="prtotAdjust")
full_dates=nc$dim$time$vals
full_dates=as.Date(full_dates,origin="1950-01-01")
nc_close(nc)#for some reason stays opened otherwise
rm(nc)
gc()
adamont=adamont[,,full_dates<dmy("01-01-2011")&full_dates>=dmy("01-01-2000")]

nc=load_nc(paste0(path_raw,"prtotAdjust_FR_rcp85_HadGEM2-ES_RegCM4-6_R2D2_day_2006-2099.nc"))
r2d2=ncvar_get(nc,varid="prtotAdjust")
full_dates=nc$dim$time$vals
full_dates=as.Date(full_dates,origin="1850-01-01")
nc_close(nc)#for some reason stays opened otherwise
rm(nc)
gc()
r2d2=r2d2[,,full_dates<dmy("01-01-2011")&full_dates>=dmy("01-01-2000")]

for(c in 1:nrow(ref_cities)){
  data=data.frame(full_dates[full_dates<dmy("01-01-2011")&full_dates>=dmy("01-01-2000")],adamont[ref_cities$row[c],ref_cities$col[c],]*86400,-r2d2[ref_cities$row[c],ref_cities$col[c],]*86400)
  colnames(data)=c("date","adamont","r2d2")
  data$year=year(data$date)
  yearsum_adamont=round(aggregate(data$adamont,by=list(data$year),sum))
  yearsum_r2d2=round(aggregate(data$r2d2*-1,by=list(data$year),sum))
  yearsum=merge(yearsum_adamont,yearsum_r2d2,by="Group.1")
  colnames(yearsum)=c("year","adamont","r2d2")
  yearsum$x=300
  yearsum$y=50
  data$date=yday(data$date)
  plt=ggplot(data)+
    geom_segment(aes(x=date,xend=date,y=0,yend=adamont,col="ADAMONT"))+
    geom_segment(aes(x=date,xend=date,y=0,yend=r2d2,col="R2D2"))+
    scale_color_manual("",values = c("ADAMONT"="red","R2D2"="blue"))+
    geom_text(data=yearsum,aes(x=x,y=y,label=adamont),col="red")+
    geom_text(data=yearsum,aes(x=x,y=-y,label=r2d2),col="blue")+
    # facet_wrap(vars(year),nrow=3,scales = "free_x")+
    facet_wrap(vars(year),nrow=3)+
    ylab("")+
    xlab("")+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())
  save.plot(plt,Filename = paste0("prtotAdjust_rcp85_HadGEM2-ES_RegCM4-6_2006-2010_",ref_cities$name[c]),Folder = paste0(path_fig,"prtotAdjust/"),Format = "jpeg")
}


##tasAdjust

nc=load_nc(paste0(path_raw,"tasAdjust_FR_rcp85_HadGEM2-ES_RegCM4-6_ADAMONT_day_2006-2099.nc"))
adamont=ncvar_get(nc,varid="tasAdjust")
full_dates=nc$dim$time$vals
full_dates=as.Date(full_dates,origin="1950-01-01")
nc_close(nc)#for some reason stays opened otherwise
rm(nc)
gc()
adamont=adamont[,,full_dates<dmy("01-01-2011")&full_dates>=dmy("01-01-2000")]

nc=load_nc(paste0(path_raw,"tasAdjust_FR_rcp85_HadGEM2-ES_RegCM4-6_R2D2_day_2006-2099.nc"))
r2d2=ncvar_get(nc,varid="tasAdjust")
full_dates=nc$dim$time$vals
full_dates=as.Date(full_dates,origin="1850-01-01")
nc_close(nc)#for some reason stays opened otherwise
rm(nc)
gc()
r2d2=r2d2[,,full_dates<dmy("01-01-2011")&full_dates>=dmy("01-01-2000")]

for(c in 1:nrow(ref_cities)){
  data=data.frame(full_dates[full_dates<dmy("01-01-2011")&full_dates>=dmy("01-01-2000")],adamont[ref_cities$row[c],ref_cities$col[c],],r2d2[ref_cities$row[c],ref_cities$col[c],])
  colnames(data)=c("date","adamont","r2d2")
  data$year=year(data$date)
  yearmean_adamont=round(aggregate(data$adamont,by=list(data$year),mean),1)
  yearmean_r2d2=round(aggregate(data$r2d2,by=list(data$year),mean),1)
  yearmean=merge(yearmean_adamont,yearmean_r2d2,by="Group.1")
  colnames(yearmean)=c("year","adamont","r2d2")
  yearmean$x=300
  yearmean$y=round(max(data$adamont))-2
  data$date=yday(data$date)
  plt=ggplot(data)+
    geom_line(aes(x=date,y=adamont,col="ADAMONT"))+
    geom_line(aes(x=date,y=r2d2,col="R2D2"))+
    scale_color_manual("",values = c("ADAMONT"="red","R2D2"="blue"))+
    geom_text(data=yearmean,aes(x=x,y=y,label=adamont),col="red")+
    geom_text(data=yearmean,aes(x=x,y=y-5,label=r2d2),col="blue")+
    # facet_wrap(vars(year),nrow=3,scales = "free_x")+
    facet_wrap(vars(year),nrow=3)+
    ylab("")+
    xlab("")+
    theme_bw(base_size = 18)+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())
  save.plot(plt,Filename = paste0("tasAdjust_rcp85_HadGEM2-ES_RegCM4-6_2006-2010_",ref_cities$name[c]),Folder = paste0(path_fig,"tasAdjust/"),Format = "jpeg")
}

