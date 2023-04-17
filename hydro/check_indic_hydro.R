# Alix Reverdy
# Explore 2
# check hydrological indicators: fit of splines

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

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-hydro/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/hydro/analyse-indic/"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"

indic=c("QA","QA_DJF","QA_MAM","QA_JJA","QA_SON","QA_janv","QA_févr","QA_mars","QA_avr","QA_mai","QA_juin","QA_juill","QA_août","QA_sept","QA_oct","QA_nov","QA_déc","QA05","QA10","QA50","QA90","QA95","QJXA","QMNA","VCN3","VCN10","VCX3")
rcp=c("historical","rcp26","rcp45","rcp85")
bc=c("ADAMONT","CDFt")
hm=c("CTRIP","EROS","GRSD","J2000","MORDOR-TS","MORDOR-SD","SMASH")
hm_domain=c("FR","Lo-Br","FR","Lo-Rh","FR","Lo","FR")
hm_NBbc=c(1,2,2,2,2,2,2)

centr_ref_year=1990# central year of 1975-2005 reference period

bv_sample=c("K010002010","K055001010","K118001010","K206401002","L541181001")

###########
#FUNCTIONS#
###########


######
#MAIN#
######


####################################################
## Make table of available simulations

simu_lst=vector(mode = "list",length=8)
names(simu_lst)=c("indic","rcp","gcm","rcm","bc","hm","hm_domain","hm_NBbc")
for (i in indic){
  for(h in 1:length(hm)){
    lst_f=list.dirs(paste0(path_data,"indic/",hm[h],"/"),recursive=F,full.names = F) #list all files of projections in folder
    simu_lst$gcm=c(simu_lst$gcm,sapply(strsplit(lst_f,"_"),"[",1))
    simu_lst$rcp=c(simu_lst$rcp,sapply(strsplit(lst_f,"_"),"[",2))
    simu_lst$rcm=c(simu_lst$rcm,sapply(strsplit(lst_f,"_"),"[",3))
    simu_lst$bc=c(simu_lst$bc,sapply(strsplit(lst_f,"_"),"[",4))
    n=length(sapply(strsplit(lst_f,"_"),"[",1))
    simu_lst$indic=c(simu_lst$indic,rep(i,n))
    simu_lst$hm=c(simu_lst$hm,rep(hm[h],n))
    simu_lst$hm_domain=c(simu_lst$hm_domain,rep(hm_domain[h],n))
    simu_lst$hm_NBbc=c(simu_lst$hm_NBbc,rep(hm_NBbc[h],n))
  }
}
simu_lst=data.frame(simu_lst)
simu_lst=simu_lst[!(simu_lst$gcm=="IPSL-CM5A-MR"&simu_lst$rcm=="WRF381P"),]
# simu_lst[simu_lst$rcm=="REMO2009",]$rcm="REMO"# the 2 versions of REMO have been signaled as identical
# simu_lst[simu_lst$rcm=="REMO2015",]$rcm="REMO"
save(simu_lst,file=paste0(path_data,"simu_lst.Rdata"))

#######################################################################################
## Codes of basins and selection

hm=unique(simu_lst$hm)
codes=vector(length=length(hm),mode="list")
for(c in 1:length(hm)){# for each chain
  if(hm[c]!="SMASH"){#because some station not simulated for SMASH
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*CNRM-CM5*historical*ALADIN*ADAMONT*",hm[c],"*")))
  }else{
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*CNRM-CM5*historical*ALADIN*CDFt*",hm[c],"*")))
  }
  pth_tmp=list.files(dir_tmp,full.names=T,recursive = T,include.dirs = F,pattern=glob2rx(paste0("*QA.f*")))
  res=read_fst(pth_tmp)
  colnames(res)=c("gcm","rcp","rcm","bc","hm","code","year","indic")
  codes[[c]]=unique(res$code)
}
codes=unlist(codes)
codes=data.frame(table(codes))
colnames(codes)=c("code","n")
codes$code=as.character(codes$code)
print(any(nchar(codes$code)==0))#missing code?

coord=vector(length=length(hm),mode="list")
for(c in 1:length(hm)){# for each chain
  dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*",hm[c],"*")))
  pth_tmp=list.files(dir_tmp,full.names=T,recursive = T,include.dirs = F,pattern=glob2rx(paste0("*meta*")))
  res=read_fst(pth_tmp)
  colnames(res)=c("code","name","regionHydro","source","ref","x_l93","y_l93","area","gestion","alti")
  coord[[c]]=res
}
coord=do.call("rbind",coord)
coord=coord[!duplicated(coord$code),]

ref=merge(x=codes,y=coord,by="code",all.x=T)
print(any(is.na(ref$ref)))#check if all simulated points have coordinates
bv_selec_idx=which(ref$code %in% bv_sample)
print(ref[bv_selec_idx,])
save(ref,file=paste0(path_data,"ref.Rdata"))

###############################################################################################
## Plot raw indicator , and its spline for all models and selection of watersheds by RCP for time
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
# type : raw_spline, raw, diff, diff_spline

##Merge data frame warnings are okay
## Sometimes problem with too many connections opened requires running step by step (i by i)


for(i in unique(simu_lst$indic)){
  dir.create(paste0(path_fig,i,"/"))
  closeAllConnections()
  gc()
  
  #############ref_c=ref
  scenAvail=simu_lst[simu_lst$indic==i,]
  global_tas=prep_global_tas(path_temp,ref_year=centr_ref_year,simu_lst=scenAvail)
  ############all_chains=extract_chains(scenAvail=scenAvail,ref_cities=ref_c)
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




