# Alix Reverdy
# Explore 2
# Last ran : 15/04/2022
# Run Qualypso with time then global temperature predictor

rm(list=ls())
gc()
dev.off()

#########
#LIBRARY#
#########

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R')

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
nbcore=detectCores()-2 #Number of cores for parallelization

var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

lst_indic=c("Q_mean_year","Q_q95_year","VCN10","VCN10_day")

spar=1.1
typeChangeVar="rel"
ref_year=1990# central year of 1975-2005 reference period
first_full_year=1972# from raw data filenames
last_full_year=2098# from raw data filenames
vecYears = seq(first_full_year,last_full_year,1)

select_gcm=c("CNRM-CM5-LR","EC-EARTH","IPSL-CM5A-MR")

#ref_GlobalTas=0
vec_futGlobaltas=c(1,1.5,2,2.5,3,4,5)


######
#MAIN#
######

load(file = paste0(path_data,"processed/simu_lst.Rdata"))
simu_lst=simu_lst[simu_lst$gcm %in% select_gcm,]
scenAvail=simu_lst[,c("rcp","gcm","rcm")]

mat_Globaltas=format_global_tas(path_data,first_full_year,last_full_year,simu_lst)

##Loop Qualypso on indexes
for (indc in lst_indic){
  
  all_chains=vector(length=nrow(simu_lst),mode="list")
  for (i in 1:nrow(simu_lst)){
    load(paste0(path_data,"processed/indic_hydro/",indc,"_",simu_lst$rcp[i],"_",simu_lst$gcm[i],"_",simu_lst$rcm[i],"_",simu_lst$bc[i],"_",simu_lst$hm[i],".Rdata"))
    all_chains[[i]]=res
  }
  
  n_bv=ncol(all_chains[[1]])-1
  lst.QUALYPSOOUT_time=vector(mode="list",length=n_bv)
  lst.QUALYPSOOUT_temp=vector(mode="list",length=n_bv)
  
  tic()
  for(i in 2:(n_bv+1)){
    
    ClimateProjections=lapply(all_chains, function(x) x[,c(1,i)])
    ClimateProjections=lapply(ClimateProjections,function(x) x[x$year>=first_full_year & x$year<=last_full_year,][,2])
    Y=t(do.call(cbind,ClimateProjections))

    # call main function QUALYPSO
    listOption = list(typeChangeVariable=typeChangeVar,nBurn=1000,nKeep=5000,nCluster=nbcore,
                      parSmooth=spar,quantileCompress=c(0.05,0.5,0.95))
    ##Time predictor
    lst.QUALYPSOOUT_time[[i-1]] = QUALYPSO(Y=Y, #one Y and run per basin because otherwise we cannot have several future times
                                      scenAvail=scenAvail,
                                      X = vecYears,
                                      Xref = ref_year,
                                      listOption=listOption)# no Xfut or iFut because we want all values
    ##Temperature predictor    
    lst.QUALYPSOOUT_temp[[i-1]] = QUALYPSO(Y=Y, #one Y and run per basin because otherwise we cannot have several future times
                                           scenAvail=scenAvail,
                                           X = mat_Globaltas,
                                           Xref = min(mat_Globaltas),# need to be changed
                                           Xfut=vec_futGlobaltas,
                                           listOption=listOption)# no iFut because we want all values
    
    if(((i-1) %% 100)==0){print(i-1)}
    
  }
  toc()  
  
  save(lst.QUALYPSOOUT_time,file=paste0(path_data,"processed/qualypso/",indc,"_list_QUALYPSOOUT_3GCM_time.RData"))
  save(lst.QUALYPSOOUT_temp,file=paste0(path_data,"processed/qualypso/",indc,"_list_QUALYPSOOUT_3GCM_temp.RData"))
  
}
