# Alix Reverdy
# Explore 2
# check hydrological indicators: fit of splines

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(stringr)

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-hydro/"

indic=c("QA","QA_DJF","QA_MAM","QA_JJA","QA_SON","QA_janv","QA_fevr","QA_mars","QA_avr","QA_mai","QA_juin","QA_juill","QA_aout","QA_sept","QA_oct","QA_nov","QA_dec","QA05","QA10","QA50","QA90","QA95","QJXA","QMNA","VCN3","VCN10","VCX3")
rcp=c("historical","rcp26","rcp45","rcp85")
bc=c("ADAMONT","CDFt")
hm=c("CTRIP","EROS","GRSD","J2000","MORDOR-TS","MORDOR-SD","SMASH","ORCHIDEE")
hm_domain=c("FR","Lo-Br","FR","Lo-Rh","FR","Lo","FR","FR")
hm_NBbc=c(1,2,2,2,2,2,2,1)

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


####################
## Clean

i_old=c("QA_févr","QA_août","QA_déc")
i_new=c("QA_fevr","QA_aout","QA_dec")

for(i in 1:length(i_old)){
  
  scenAvail=simu_lst[simu_lst$indic==i_old[i],]
  for(c in 1:nrow(scenAvail)){# for each chain
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*",scenAvail$gcm[c],"*",scenAvail$rcp[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",scenAvail$hm[c],"*")))
    pth_tmp=Sys.glob(paths=paste0(dir_tmp,"/*/*/*",i_old[i],"*"))#sys.glob allow accents
    new_path=str_replace(pth_tmp,i_old[i],i_new[i])
    file.rename(pth_tmp,new_path)
  }
}

scenAvail=simu_lst[simu_lst$indic=="QA",]
for(c in 1:nrow(scenAvail)){# for each chain
  dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*",scenAvail$gcm[c],"*",scenAvail$rcp[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",scenAvail$hm[c],"*")))
  if(c==1){
    file.copy(paste0(dir_tmp,"/meta.fst"),paste0(dirname(dir_tmp),"/meta1.fst"))
  }
  pth_tmp=Sys.glob(paths=paste0(dir_tmp,"/*.fst"))
  file.remove(pth_tmp)#need to copy one of the
  unlink(paste0(dir_tmp,"/dataEX_Explore2_proj_check"),recursive = T)
}
