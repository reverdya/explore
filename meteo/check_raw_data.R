# Alix Reverdy
# Explore 2
# check raw data for NA, data too big or small and missing dates

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
load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"refs.Rdata"))
path_data="E:/Explore2_data/meteo/"
folder_out="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse-indic/compare_BC/"

nb_NA_ref=9270
thresh_max=c(58+273.15,5*550,5*550,5*550)
thresh_min=c(-60+273.15,0,0,0)
ref_duration=as.numeric(read.csv(paste0(path_data,"duration.csv"),sep=",",header=F))

###########
#FUNCTIONS#
###########


######
#MAIN#
######

cpt=1
for (v in unique(simu_lst$var)){
  folder=paste0(path_data,"indic/",v,"/check_data/")
  
  files=list.files(folder,pattern=glob2rx(paste0("*NA*")),full.names = T)
  too_much_NA=vector(length=length(files))
  for(f in 1:length(files)){
    df=read.csv(files[f],sep ="",header = F,skip = 5) #"" is any numvber of whitespace delimiter
    df=df[-nrow(df),]
    too_much_NA[f]=any(df$V7!=nb_NA_ref)
  }
  print(paste0("Variable ",v," has too much NA: ",any(too_much_NA)))
  
  files=list.files(folder,pattern=glob2rx(paste0("*check*")),full.names = T)
  vec=read.csv(files,header=F)
  if(v=="tasAdjust"){vec=vec[-c((nrow(vec)-2):nrow(vec)),]}
  vec=data.frame(vec)
  if(nrow(vec)!=6*120){stop(paste0("One chain was not processed in cdo for :",v))}
  min_idx=seq(from=2,by=6,length.out=120)
  max_idx=seq(from=4,by=6,length.out=120)
  ntime_idx=seq(from=6,by=6,length.out=120)
  df=data.frame(min=as.numeric(vec[min_idx,]),max=as.numeric(vec[max_idx,]),ntime=as.integer(vec[ntime_idx,]))
  if(v!="tasAdjust"){df[,c("min","max")]=df[,c("min","max")]*24*3600}
  
  print(paste0("Variable ",v," has ",length(which(df$max>thresh_max[cpt]))," chains with values above ",thresh_max[cpt]))
  print(paste0("Variable ",v," has ",length(which(df$min<thresh_min[cpt]))," chains with values below ",thresh_min[cpt]))
  print(paste0("Variable ",v," has missing dates: ",any(!df$ntime %in% ref_duration)))
  cpt=cpt+1
}


#########################################
## Export for LSCE and MF

## NA ADAMONT

cpt=4
v= unique(simu_lst$var)[4]
folder=paste0(path_data,"indic/",v,"/check_data/")
files=list.files(folder,pattern=glob2rx(paste0("*NA*")),full.names = T)
too_much_NA=vector(length=length(files))
for(f in 1:length(files)){
  df=read.csv(files[f],sep ="",header = F,skip = 4) #"" is any numvber of whitespace delimiter
  df=df[-nrow(df),]
  too_much_NA[f]=any(as.numeric(df$V7)!=nb_NA_ref)
}
tmp=files[which(too_much_NA)]
vec_chain=sub("_day.*", "",sub(".*FR_", "", tmp))

NA_ADAMONT=list()
for(f in 1:length(tmp)){
  df=read.csv(tmp[f],sep ="",header = F,skip = 4) #"" is any numvber of whitespace delimiter
  df=df[-nrow(df),]
  NA_ADAMONT[[vec_chain[f]]]=df[which(as.numeric(df$V7)!=nb_NA_ref),"V3"]
}

seq_length_max<- seq(length.out=max(sapply(NA_ADAMONT, length)))
NA_ADAMONT <- sapply(NA_ADAMONT, "[", i = seq_length_max)

write.csv(NA_ADAMONT,paste0(folder,"NA_ADAMONT.csv"),row.names = F)

## Threshold R2D2 tasmin
cpt=1
v= unique(simu_lst$var)[1]
folder=paste0(path_data,"indic/",v,"/check_data/")
files=list.files(folder,pattern=glob2rx(paste0("*NA*")),full.names = T)
tas_min_thresh=vector(length=length(files))
for(f in 1:length(files)){
  df=read.csv(files[f],sep ="",header = F,skip = 5) #"" is any numvber of whitespace delimiter
  df=df[-nrow(df),]
  tas_min_thresh[f]=any(as.numeric(df$V9)<(273.15-50))
}
tmp=files[which(tas_min_thresh)]
vec_chain=sub("_day.*", "",sub(".*FR_", "", tmp))

tas_min_R2D2=list()
for(f in 1:length(tmp)){
  df=read.csv(tmp[f],sep ="",header = F,skip = 5) #"" is any numvber of whitespace delimiter
  df=df[-nrow(df),]
  tas_min_R2D2[[vec_chain[f]]]=df[which(df$V9<(273.15-50)),"V3"]
}

seq_length_max<- seq(length.out=max(sapply(tas_min_R2D2, length)))
tas_min_R2D2<- sapply(tas_min_R2D2, "[", i = seq_length_max)

write.csv(tas_min_R2D2,paste0(folder,"tas_min-50_R2D2.csv"),row.names = F)

## Threshold R2D2 tasmax
cpt=1
v= unique(simu_lst$var)[1]
folder=paste0(path_data,"indic/",v,"/check_data/")
files=list.files(folder,pattern=glob2rx(paste0("*NA*")),full.names = T)
tas_max_thresh=vector(length=length(files))
for(f in 1:length(files)){
  df=read.csv(files[f],sep ="",header = F,skip = 5) #"" is any numvber of whitespace delimiter
  df=df[-nrow(df),]
  tas_max_thresh[f]=any(as.numeric(df$V11)>(273.15+58))
}
tmp=files[which(tas_max_thresh)]
vec_chain=sub("_day.*", "",sub(".*FR_", "", tmp))

tas_max_R2D2=list()
for(f in 1:length(tmp)){
  df=read.csv(tmp[f],sep ="",header = F,skip = 5) #"" is any numvber of whitespace delimiter
  df=df[-nrow(df),]
  tas_max_R2D2[[vec_chain[f]]]=df[which(df$V11>(273.15+58)),"V3"]
}

seq_length_max<- seq(length.out=max(sapply(tas_max_R2D2, length)))
tas_max_R2D2<- sapply(tas_max_R2D2, "[", i = seq_length_max)

write.csv(tas_max_R2D2,paste0(folder,"tas_max+58_R2D2.csv"),row.names = F)

## Threshold R2D2 prtotmax
cpt=2
v= unique(simu_lst$var)[2]
folder=paste0(path_data,"indic/",v,"/check_data/")
files=list.files(folder,pattern=glob2rx(paste0("*NA*")),full.names = T)
prcp_max_thresh=vector(length=length(files))
for(f in 1:length(files)){
  df=read.csv(files[f],sep ="",header = F,skip = 5) #"" is any numvber of whitespace delimiter
  df=df[-nrow(df),]
  prcp_max_thresh[f]=any(as.numeric(df$V11)>(2750/86400))
}
tmp=files[which(prcp_max_thresh)]
vec_chain=sub("_day.*", "",sub(".*FR_", "", tmp))

prcp_max_R2D2=list()
for(f in 1:length(tmp)){
  df=read.csv(tmp[f],sep ="",header = F,skip = 5) #"" is any number of whitespace delimiter
  df=df[-nrow(df),]
  prcp_max_R2D2[[vec_chain[f]]]=df[which(df$V11>(2750/86400)),"V3"]
}

seq_length_max<- seq(length.out=max(sapply(prcp_max_R2D2, length)))
prcp_max_R2D2<- sapply(prcp_max_R2D2, "[", i = seq_length_max)

write.csv(prcp_max_R2D2,paste0(folder,"prcp_max2750_R2D2.csv"),row.names = F)

#####################################################################################
## Maps of number of years with too high precip or tas by season and chain


path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/"

for(v in unique(simu_lst$var)[1:2]){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(13,14,15,16)]){
    closeAllConnections()
    gc()
    scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i&simu_lst$bc=="R2D2",]
    thresh_max=vector(length=nrow(scenAvail),mode="list")
    thresh_min=vector(length=nrow(scenAvail),mode="list")
    for(c in 1:nrow(scenAvail)){# for each chain
      pth_tmp=list.files(paste0(path_data,"indic/",v,"/"),full.names=T,pattern=glob2rx(paste0(v,"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c],"_")[[1]][1],"*",scenAvail$period[c],"*")))
      nc=load_nc(pth_tmp)
      res=ncvar_get(nc,varid=v)
      nc_close(nc)
      rm(nc)
      gc()
      
      vec_mask=as.logical(refs$mask)
      dim(res)=c(dim(res)[1]*dim(res)[2],dim(res)[3])# collapses one dimension
      res=res[vec_mask,]
      if(v=="tasAdjust"){
        thresh_max[[c]]=apply(res,MARGIN=1,function(x) length(which(x>(273.15+50))))
        thresh_min[[c]]=apply(res,MARGIN=1,function(x) length(which(x<(273.15-30))))
      }
      if(v=="prtotAdjust"){
        thresh_max[[c]]=apply(res,MARGIN=1,function(x) length(which(x>(3000))))
      }
    }
    
    data=data.frame(do.call(cbind,thresh_max))
    colnames(data)=paste0(scenAvail$rcp,"/",scenAvail$gcm,"/",scenAvail$rcm)
    notonly_0=as.numeric(which(apply(data,MARGIN=2,sum)!=0))
    if(length(notonly_0)!=0){
      data=data[,notonly_0]
      data=pivot_longer(data,names_to = "chain",values_to = "val",cols=everything())
      data=data[order(data$chain),]
      
      exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
      exut=exut[as.logical(refs$mask),]
      exut$idx=seq(1:nrow(exut))
      tmp=exut
      for (j in 1:(length(notonly_0)-1)){
        exut=rbind(exut,tmp)
      }
      exut$chain=data$chain
      exut$val=data$val
      exut$val[exut$val==0]=NA
      
      plt=base_map_grid(data = exut,val_name = "val",afterscale = T)
      if(v=="tasAdjust"){
        plt=plt+
          facet_wrap(~chain,ncol=5)+
          theme(panel.border = element_rect(colour = "black",fill=NA))+
          theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
          scale_colour_gradientn("Nb",colors=parula(100),breaks = sort(unique(exut$val)),guide = guide_legend(),na.value = "grey90")+
          ggtitle(paste0("Number of yearly values above 50°C\nfor each cell and chain (",v,"_",i,")"))
      }
      if(v=="prtotAdjust"){
        plt=plt+
          facet_wrap(~chain,ncol=5)+
          theme(panel.border = element_rect(colour = "black",fill=NA))+
          theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
          scale_colour_gradientn("Nb",colors=kovesi.rainbow_bgyr_35_85_c72(100),breaks = sort(unique(exut$val)),guide = guide_legend(),na.value = "grey90")+
          ggtitle(paste0("Number of yearly values above 3000mm\nfor each cell and chain (",v,"_",i,")"))
      }
      save.plot(plt,Filename = paste0("threshmax_",v,"_",i),Folder = folder_out,Format = "jpeg")
    }
    
    if(v=="tasAdjust"){
      data=data.frame(do.call(cbind,thresh_min))
      colnames(data)=paste0(scenAvail$rcp,"/",scenAvail$gcm,"/",scenAvail$rcm)
      notonly_0=as.numeric(which(apply(data,MARGIN=2,sum)!=0))
      if(length(notonly_0)!=0){
        data=data[,notonly_0]
        data=pivot_longer(data,names_to = "chain",values_to = "val",cols=everything())
        data=data[order(data$chain),]
        
        exut=data.frame(x=as.vector(refs$x_l2),y=as.vector(refs$y_l2))
        exut=exut[as.logical(refs$mask),]
        exut$idx=seq(1:nrow(exut))
        tmp=exut
        for (j in 1:(length(notonly_0)-1)){
          exut=rbind(exut,tmp)
        }
        exut$chain=data$chain
        exut$val=data$val
        exut$val[exut$val==0]=NA
        
        plt=base_map_grid(data = exut,val_name = "val",afterscale = T)
        plt=plt+
          facet_wrap(~chain,ncol=5)+
          theme(panel.border = element_rect(colour = "black",fill=NA))+
          theme(legend.key = element_rect(color="black"),legend.title = element_text(face = "bold",size = 14),legend.text = element_text(face = "bold",size = 11))+
          scale_colour_gradientn("Nb",colors=parula(100),breaks = sort(unique(exut$val)),guide = guide_legend(),na.value = "grey90")+
          ggtitle(paste0("Number of yearly values below -30°C\nfor each cell and chain (",v,"_",i,")"))
        save.plot(plt,Filename = paste0("threshmin_",v,"_",i),Folder = folder_out,Format = "jpeg")
      }
    }
  }
}
