# Alix Reverdy
# Explore 2
# Make figures from Qualypso runs

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

path_data="C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse_indic/"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/French_cities/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"

load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"refs.Rdata"))

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions

nbcores=detectCores()-2

ref_year=1990

######
#MAIN#
######


############################################
## Find pixel with greatest total variance

v="prtotAdjust"
i="yearsum"

load(file=paste0(path_data,"Qualypso/outdated/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_allyears.RData"))
xfut=which(lst.QUALYPSOOUT_time[[1]]$Xfut==2085)
all_pix=lst.QUALYPSOOUT_time[[xfut]]$TOTALVAR

Nhalf=which(as.vector(refs$lat)[as.logical(refs$mask)]>47.5) #North half of France
all_pixN=all_pix[Nhalf]
idx_N=which.max(all_pixN)
idx=Nhalf[idx_N]

## Check position
print(as.vector(refs$lat)[as.logical(refs$mask)][idx])
print(as.vector(refs$lon)[as.logical(refs$mask)][idx])

##############################################
## Plot raw data and spline

xcoord=as.vector(refs$lon)[as.logical(refs$mask)][idx]
ycoord=as.vector(refs$lat)[as.logical(refs$mask)][idx]
dist=sqrt((refs$lon-xcoord)^2+(refs$lat-ycoord)^2)
min_dist=as.vector(which(dist==min(dist),arr.ind = T))
idx_row=min_dist[1]
idx_col=min_dist[2]

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
  res2=data.frame(matrix(nrow=dim(res)[3],ncol=2))
  res2[,1]=full_years
  res2[,2]=res[idx_row,idx_col,]
  colnames(res2)[1]="year"
  all_chains[[c]]=res2
  rm(res)
  rm(res2)
  gc()
}


SPAR=1.1
Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections))
Y=Y[,Y[1,]<=2100]
X=Y[1,]
Y=Y[-1,]
nS=nrow(scenAvail)
Xfut=seq(centr_ref_year,X[length(X)])
clim_resp=prepare_clim_resp(Y=Y,X=X,Xfut=Xfut,typeChangeVariable = "abs",spar = rep(SPAR,nrow(scenAvail)),type = "spline")
raw=data.frame(t(Y[,X>=Xfut[1]]))
colnames(raw)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
raw[is.na(t(Y[,X>=Xfut[1]]))]=NA
raw$year=X[X>=Xfut[1]]
raw=pivot_longer(data=raw,cols=!year,names_to = "model",values_to = "val")
raw$type="raw"
spline=data.frame(t(clim_resp$phi))
colnames(spline)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
spline[is.na(t(Y[,X>=Xfut[1]]))]=NA
spline$year=X[X>=Xfut[1]]
spline=pivot_longer(data=spline,cols=!year,names_to = "model",values_to = "val")
spline$type="spline"

data=rbind(raw,spline)
data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[3]))
data$bc=unlist(lapply(strsplit(data$model,"_"),function(x) x[4]))

ylabel="Réponse climatique"
unit="(mm)"

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
  save.plot(plt,Filename = paste0(v,"_",i,"_chronique_pixel_problem_",r,"_spar",SPAR),Folder = paste0(path_fig,v,"/",i,"/plot_chains/"),Format = "jpeg")
}
