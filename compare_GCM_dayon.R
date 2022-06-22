# Alix Reverdy
# Explore 2
# Compare GCM from Gildas Dayon and from Explore 2

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(abind)

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/raw/GCM"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/compare_gcm/"

###########
#FUNCTIONS#
###########



######
#MAIN#
######

lst_f=list.files(path_data,pattern=glob2rx(paste0("*","_deltaT.nc")),full.names = T) 

lst_gcm=vector(mode = "list",length=length(lst_f))
for (i in 1:length(lst_f)){
  nc=load_nc(lst_f[i])
  data=ncvar_get(nc,varid="tas")
  lon=nc$dim$lon$vals
  lat=nc$dim$lat$vals
  rm(nc)
  gc()# memory management as some files can get big
  gcm_name=strsplit(x = lst_f[i],split = "_")[[1]][4]
  lst_gcm[[i]]=data
  names(lst_gcm)[i]=gcm_name
}

idx_dayon=c(1,2,3,4,6,9,10,12)
idx_expl=c(5,7,8,9,11,12)

df_gcm <- abind(lst_gcm, along=3)
mean_dayon=apply(df_gcm[,,idx_dayon], c(1,2), mean)
mean_expl=apply(df_gcm[,,idx_expl], c(1,2), mean)
diff=mean_expl-mean_dayon

diff=stack(data.frame(diff))
colnames(diff)=c("tas","lat")
diff$lat=rep(lat,each=length(lon))
diff$lon=rep(lon,times=length(lat))

plt=ggplot(diff)+
  geom_tile(aes(x=lon,y=lat,fill=tas))+
  scale_fill_stepsn("DeltaT (K)",colors=temp_11[-6],limits=c(-0.5,0.5),breaks=seq(-0.5,0.5,0.1))+
  geom_polygon(data=wrld,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=0.1)+
  coord_equal(ratio=111/78,xlim = c(-23, 23),ylim = c(33, 63),expand=F)+## ratio of 1°lat by 1°long at 45°N
  scale_x_continuous("Longitude (deg)")+
  scale_y_continuous("Latitude (deg)")+
  ggtitle(paste0("Temperature difference between mean changes of\nExplore2 and Dayon GCMs ( 2065-2095 VS 2005-2035 RCP8.5)"))+
  theme_bw(base_size = 10)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  guides(fill = guide_colourbar(barheight = 12))
dir.create(paste0(path_fig))
save.plot(plt,Filename = paste0("diff_gcm_expl-dayon"),Folder = path_fig,Format = "jpeg")

