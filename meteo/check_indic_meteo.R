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
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/French_cities/"

Var=vector(mode="list")
Var[["tasAdjust"]]=c(rep("monmean",12),"seasmean","seasmean","seasmean","seasmean","yearmean")
Var[["prtotAdjust"]]=c(rep("monsum",12),"seassum","seassum","seassum","seassum","yearsum",rep("monocc",12),"seasocc","seasocc","seasocc","seasocc","yearocc")
Var[["prsnAdjust"]]=c(rep("monsum",12),"seassum","seassum","seassum","seassum","yearsum",rep("monocc",12),"seasocc","seasocc","seasocc","seasocc","yearocc")
Var[["evspsblpotAdjust"]]=c(rep("monsum",12),"seassum","seassum","seassum","seassum","yearsum")
period=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12","_DJF","_MAM","_JJA","_SON","","_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12","_DJF","_MAM","_JJA","_SON","")
rcp=c("historical","rcp26","rcp45","rcp85")
bc=c("ADAMONT","R2D2")

units=c("°C","mm")

centr_ref_year=1990# central year of 1975-2005 reference period
first_ref_year=1975
last_ref_year=2005



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
simu_lst[simu_lst$rcm=="REMO2009",]$rcm="REMO"# the 2 versions of REMO have been signaled as identical
simu_lst[simu_lst$rcm=="REMO2015",]$rcm="REMO"
save(simu_lst,file=paste0(path_data,"simu_lst.Rdata"))


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
## Plot raw indicator , and its spline and residual anomalies for all models and selection of watersheds by RCP for time
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
## Climate response not climate change response


##Merge data frame warnings are okay

## Problem with too many connections opened requires running step by step (v by v)

for(v in unique(simu_lst$var)){
  dir.create(paste0(path_fig,v,"/"))
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
    closeAllConnections()
    gc()
    clim_resp=vector(length=nrow(simu_lst),mode="list")
    clim_resp_spline=vector(length=nrow(simu_lst),mode="list")
    dir.create(paste0(path_fig,v,"/",i))
    dir.create(paste0(path_fig,v,"/",i,"/plot_chains/"))
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
      for (SPAR in c(0.8,0.9,1.0,1.1,1.2)){
        ClimateProjections=lapply(all_chains, function(x) x[,c(1,cities)])
        Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections))
        Y=Y[,Y[1,]<=2100]
        X=Y[1,]
        Y=Y[-1,]
        nS=nrow(scenAvail)
        if(scenAvail$indic[1]=="VNC10"){#for use of logarithm
          clim_resp=prepare_clim_resp(Y=Y,X=X,Xref=1990,Xfut=X,typeChangeVariable = "rel",spar = rep(SPAR,nrow(scenAvail)),type = "log_spline")
        }else{
          if(scenAvail$var[1]=="tasAdjust"){
            clim_resp=prepare_clim_resp(Y=Y,X=X,Xref=1990,Xfut=X,typeChangeVariable = "abs",spar = rep(SPAR,nrow(scenAvail)),type = "spline")
          }else{
            clim_resp=prepare_clim_resp(Y=Y,X=X,Xref=1990,Xfut=X,typeChangeVariable = "rel",spar = rep(SPAR,nrow(scenAvail)),type = "spline")
          }
        }
        raw=data.frame(t(clim_resp$phiStar+clim_resp$etaStar))
        colnames(raw)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
        raw[is.na(t(Y))]=NA
        raw$year=X
        raw=pivot_longer(data=raw,cols=!year,names_to = "model",values_to = "val")
        raw$type="raw"
        spline=data.frame(t(clim_resp$phiStar))
        colnames(spline)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
        spline[is.na(t(Y))]=NA
        spline$year=X
        spline=pivot_longer(data=spline,cols=!year,names_to = "model",values_to = "val")
        spline$type="spline"
        
        data=rbind(raw,spline)
        data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
        data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
        data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[3]))
        data$bc=unlist(lapply(strsplit(data$model,"_"),function(x) x[4]))
        
        
        if(v!="tasAdjust"){
          data$val=data$val*100
          ylabel="Réponse au\nchangement climatique"
          unit=" (%)"
        }else{
          ylabel="Réponse climatique"
          unit=" (°C)"
        }
        
        for (r in unique(data$rcp)){
          plt=ggplot(data[data$rcp==r,])+#Warnings okay
            geom_line(aes(x=year,y=val,size=type,color=rcm))+
            scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur",ylabel))+
            scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
            #scale_linetype("")+
            theme_bw(base_size = 18)+
            theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
            theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
            scale_x_continuous("")+
            scale_y_continuous(paste0(ylabel,unit),limits = c(min(data[data$rcp==r,]$val,na.rm=T),max(data[data$rcp==r,]$val,na.rm=T)),n.breaks=4,expand = c(0,0))+
            guides(color = guide_legend(override.aes = list(size = 1.7)))+
            facet_grid(gcm~bc)+
            theme(panel.spacing.x = unit(0.5, "lines"))+
            theme(strip.text.y = element_text(size = 9))
          if(SPAR==1){
            save.plot(plt,Filename = paste0(v,"_",i,"_chronique_rel_",ref_cities$name[cities-1],"_",r,"_spar1.0"),Folder = paste0(path_fig,v,"/",i,"/plot_chains/"),Format = "jpeg")
          }else{
            save.plot(plt,Filename = paste0(v,"_",i,"_chronique_rel_",ref_cities$name[cities-1],"_",r,"_spar",SPAR),Folder = paste0(path_fig,v,"/",i,"/plot_chains/"),Format = "jpeg")
          }
        }
      }
    }
  }
}



