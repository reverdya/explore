# Alix Reverdy
# Explore 2
# Compare ADAMONT and R2D2-2L

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
    stats=unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16,30,31,32,33)]
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
        ylabel="Prtot"
        unit=" (mm)"
      }else{
        ylabel="Tas"
        unit=" (°C)"
      }
      
      r="rcp85"
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
      save.plot(plt,Filename = paste0(v,"_",i,"_chronique_rel_",ref_cities$name[cities-1],"_",r),Folder = paste0(path_fig,v,"/"),Format = "jpeg")
    }
  }
}



##############################################
## Plot raw variable for a year and a chain 

