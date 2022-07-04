# Alix Reverdy
# Explore 2
# Run Qualypso with time then global temperature predictor

rm(list=ls())
gc()

#########
#LIBRARY#
#########

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")
#View(QUALYPSO.ANOVA)
#lsf.str("package:QUALYPSO")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
nbcore=detectCores()-2 #Number of cores for parallelization

var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

load(file=paste0(path_data,"processed/lst_indic.Rdata"))

typeChangeVar="rel"
ref_year=1990# central year of 1975-2005 reference period
first_ref_year=1975
last_ref_year=2005
first_full_year=1972# from raw data filenames
last_full_year=2098# from raw data filenames
first_data_year=1951
last_data_year=2099
vecYears = seq(first_full_year,last_full_year,1)

select_gcm=c("CNRM-CM5-LR","EC-EARTH","IPSL-CM5A-MR")



######
#MAIN#
######

load(file = paste0(path_data,"processed/simu_lst.Rdata"))
simu_lst=simu_lst[simu_lst$gcm %in% select_gcm,]
scenAvail=simu_lst[,c("rcp","gcm","rcm")]

tmp=format_global_tas(path_data,first_data_year,last_data_year,simu_lst,first_ref_year,last_ref_year)
mat_Globaltas=tmp[[1]]
ref_Globaltas=tmp[[2]]

################################
## Linear method
for (indc in lst_indic){
  
  all_chains=vector(length=nrow(simu_lst),mode="list")
  for (i in 1:nrow(simu_lst)){
    load(paste0(path_data,"processed/indic_hydro/",indc,"_",simu_lst$rcp[i],"_",simu_lst$gcm[i],"_",simu_lst$rcm[i],"_",simu_lst$bc[i],"_",simu_lst$hm[i],".Rdata"))
    all_chains[[i]]=res
  }
  
  n_bv=ncol(all_chains[[1]])-1
  lst.QUALYPSOOUT_time=vector(mode="list",length=n_bv)
  lst.QUALYPSOOUT_temp_3rcp=vector(mode="list",length=n_bv)
  lst.QUALYPSOOUT_temp_2rcp=vector(mode="list",length=n_bv)
  lst.QUALYPSOOUT_temp_1rcp=vector(mode="list",length=n_bv)
  
  tic()
  for(i in 2:(n_bv+1)){
    
    ClimateProjections=lapply(all_chains, function(x) x[,c(1,i)])
    Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections)[,-1])
    #Warnings okay
    
    #ClimateProjections=lapply(ClimateProjections,function(x) x[x$year>=first_full_year & x$year<=last_full_year,][,2])
    #Y=t(do.call(cbind,ClimateProjections))
    
    ##Time predictor
    if(indc=="VCN10"){ #transformation to log to avoid negative values
      tmp=prepare_clim_resp(Y=Y,X=seq(first_data_year,last_data_year),Xref = ref_year,Xfut = vecYears,typeChangeVariable = "rel",spar = rep(1.1,nrow(simu_lst)),type = "log_spline")
      listOption = list(spar=1.1,typeChangeVariable=typeChangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99),climResponse=tmp)
    }else{
      listOption = list(spar=1.1,typeChangeVariable=typeChangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
    }
    lst.QUALYPSOOUT_time[[i-1]] = QUALYPSO(Y=Y, #one Y and run per basin because otherwise we cannot have several future times
                                           scenAvail=scenAvail,
                                           X=seq(first_data_year,last_data_year),
                                           Xref = ref_year,
                                           Xfut = vecYears,
                                           listOption=listOption)# no Xfut or iFut because we want all values
    ##Temperature predictor
    spar_tmp=rep(1.1,nrow(simu_lst))
    idx_mpi=which(simu_lst$gcm=="MPI-ESM-LR")
    spar_tmp[idx_mpi]=1.5
    tmp=prepare_clim_resp(Y=Y,X=mat_Globaltas,Xref =ref_Globaltas,Xfut = seq(0.5,1.5,0.01),typeChangeVariable = "rel",spar = spar_tmp,type = "spline")
    listOption = list(spar=1.2,typeChangeVariable=typeChangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99),climResponse=tmp)
    lst.QUALYPSOOUT_temp_3rcp[[i-1]] = QUALYPSO(Y=Y, #one Y and run per basin because otherwise we cannot have several future times
                                           scenAvail=scenAvail,
                                           X = mat_Globaltas,
                                           Xref = ref_Globaltas,
                                           Xfut=seq(0.5,1.5,0.01),
                                           listOption=listOption)# no iFut because we want all values
    
    idx_2rcp=which(scenAvail$rcp=="rcp4.5"|scenAvail$rcp=="rcp8.5")
    tmp=prepare_clim_resp(Y=Y[idx_2rcp,],X=mat_Globaltas[idx_2rcp,],Xref = ref_Globaltas[idx_2rcp],Xfut = seq(0.5,2.5,0.01),typeChangeVariable = "rel",spar = spar_tmp,type = "spline")
    listOption = list(spar=1.2,typeChangeVariable=typeChangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99),climResponse=tmp)
    lst.QUALYPSOOUT_temp_2rcp[[i-1]] = QUALYPSO(Y=Y[idx_2rcp,], #one Y and run per basin because otherwise we cannot have several future times
                                                scenAvail=scenAvail[idx_2rcp,],
                                                X = mat_Globaltas[idx_2rcp,],
                                                Xref = ref_Globaltas[idx_2rcp],
                                                Xfut=seq(0.5,2.5,0.01),
                                                listOption=listOption)# no iFut because we want all values
    
    ##For now this problem is under-constrained(3 + 4 effects + intercept for 6 chains)
    # idx_1rcp=which(scenAvail$rcp=="rcp8.5")
    # lst.QUALYPSOOUT_temp_1rcp[[i-1]] = QUALYPSO(Y=Y[idx_1rcp,], #one Y and run per basin because otherwise we cannot have several future times
    #                                             scenAvail=scenAvail[idx_1rcp,c("gcm","rcm")],
    #                                             X = mat_Globaltas[idx_1rcp,],
    #                                             Xref = ref_Globaltas[idx_1rcp],
    #                                             Xfut=seq(0.5,5,0.05),
    #                                             listOption=listOption)# no iFut because we want all values
    
    if(((i-1) %% 100)==0){print(i-1)}
    
  }
  toc()  
  
  save(lst.QUALYPSOOUT_time,file=paste0(path_data,"processed/qualypso/",indc,"_list_QUALYPSOOUT_3GCM_time_lm.RData"))
  save(lst.QUALYPSOOUT_temp_3rcp,file=paste0(path_data,"processed/qualypso/",indc,"_list_QUALYPSOOUT_3GCM_temp_3rcp_lm.RData"))
  save(lst.QUALYPSOOUT_temp_2rcp,file=paste0(path_data,"processed/qualypso/",indc,"_list_QUALYPSOOUT_3GCM_temp_2rcp_lm.RData"))
  #save(lst.QUALYPSOOUT_temp_1rcp,file=paste0(path_data,"processed/qualypso/",indc,"_list_QUALYPSOOUT_3GCM_temp_1rcp_lm.RData"))
}

