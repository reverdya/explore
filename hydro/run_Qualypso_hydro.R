# Alix Reverdy
# Explore 2
# Run Qualypso with time then global temperature predictor

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(dplyr)#distinct

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")
#View(QUALYPSO.ANOVA)
#lsf.str("package:QUALYPSO")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-hydro/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/processed/"
nbcore=detectCores()-2 #Number of cores for parallelization

load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"ref.Rdata"))

ref_year=1990# central year of 1975-2005 reference period
horiz=c(2030,2050,2085)
final_year=2100

######
#MAIN#
######


####################################################################
## Run for all steps and all models hydro for biggest common sample

for(i in unique(simu_lst$indic)){
  dir.create(paste0(path_data,"Qualypso/",i,"/"))
  typechangeVar="rel"
  SPAR=1.1
  
  
  tic()
  scenAvail=simu_lst[simu_lst$indic==i,]
  all_chains=vector(length=nrow(scenAvail),mode="list")
  for(c in 1:nrow(scenAvail)){# for each chain
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*",scenAvail$gcm[c],"*",scenAvail$rcp[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",scenAvail$hm[c],"*")))
    pth_tmp=list.files(dir_tmp,full.names=T,recursive = T,include.dirs = F,pattern=glob2rx(paste0("*",i,".f*")))
    res=read_fst(pth_tmp)
    colnames(res)=c("gcm","rcp","rcm","bc","hm","code","year","indic")
    res$year=year(res$year)
    res=res[!is.na(res$indic),]
    res=res[,c("year","code","indic")]
    res=pivot_wider(res,names_from = code,values_from = indic)
    all_chains[[c]]=res
  }

  #################
  ##To be adjusted
  ## After that need to adjust scenAvail in run QUALYPSO to include or not bc
  basin_sample=ref[ref$n==8,]$code
  n_bv=length(basin_sample)
  # bc_sample="ADAMONT"
  bc_sample=c("ADAMONT","CDFt")
  hm_sample=c("CTRIP","EROS","GRSD","J2000","MORDOR-TS","MORDOR-SD","SMASH","ORCHIDEE")
  ################
  
  all_chains=all_chains[scenAvail$bc %in% bc_sample]
  scenAvail=scenAvail[scenAvail$bc %in% bc_sample,]
  all_chains=all_chains[scenAvail$hm %in% hm_sample]
  scenAvail=scenAvail[scenAvail$hm %in% hm_sample,]
  all_chains=lapply(all_chains,function(x) x[,c("year",basin_sample)])
  
  ClimateProjections=Reduce(function(...) merge(...,by="year", all=T), all_chains)#allows for NA
  Y=abind(split(data.frame(t(ClimateProjections[,-1])),rep(seq(1,length(all_chains)),each=n_bv) ), along=3)
  Y=aperm(Y,c(1,3,2))
  X=unique(ClimateProjections$year)
  Xfut=seq(ref_year,final_year)
  rm(ClimateProjections,all_chains)
  gc()
  
  ## Time
  tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = rep(SPAR,nrow(scenAvail)),type="spline",nbcores = nbcore)
  listOption = list(spar=SPAR,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
  rm(tmp)
  gc()
  
  lst.QUALYPSOOUT_time=vector(mode="list",length=length((Xfut)))
  for(cpt in 1:length(Xfut)){
    # for(x in c(2030,2050,2085)){
    lst.QUALYPSOOUT_time[[cpt]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                           scenAvail=scenAvail[,c("rcp","gcm","rcm","bc","hm")],
                                           X=X,
                                           Xfut=Xfut,
                                           iFut=cpt,
                                           listOption=listOption)
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse=NA #to not store twice the same information
    lst.QUALYPSOOUT_time[[cpt]]$RESERR=NA
    lst.QUALYPSOOUT_time[[cpt]]$CHANGEBYEFFECT=NA
    lst.QUALYPSOOUT_time[[cpt]]$CLIMATEESPONSE$YStar=NA
    if(cpt!=1){
      lst.QUALYPSOOUT_time[[cpt]]$CLIMATEESPONSE=NA #to not store 9892 times the same information, stored only the first time
      lst.QUALYPSOOUT_time[[cpt]]$Y=NA #to not store 9892 times the same information, stored only the first time
    }
    if(((cpt) %% 10)==0){print(cpt)}
  }
  rm(listOption)
  closeAllConnections()
  gc()
  save(lst.QUALYPSOOUT_time,file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time.RData"))
  # rm(lst.QUALYPSOOUT_time) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
  closeAllConnections()
  gc()
  
  ## Temperature
  vec_years=X
  global_tas=prep_global_tas(path_temp,ref_year=ref_year,simu_lst=scenAvail,cat="hydro")
  X=global_tas[["mat_Globaltas"]][,global_tas[["gcm_years"]] %in% vec_years]
  Xfut=c(global_tas[["warming_1990"]],seq(1,4,0.5))
  idx_rcp=which(scenAvail$rcp=="rcp85")
  scenAvail=scenAvail[idx_rcp,]
  X=X[idx_rcp,]
  Y=Y[,idx_rcp,]
  tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = rep(1.4,nrow(scenAvail)),type="spline",nbcores = nbcore,scenAvail = scenAvail)
  listOption = list(spar=SPAR,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
  rm(tmp)
  gc()
  
  lst.QUALYPSOOUT_temp=vector(mode="list",length=length((Xfut)))
  for(cpt in 1:length(Xfut)){
    lst.QUALYPSOOUT_temp[[cpt]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                           scenAvail=scenAvail[,c("gcm","rcm","bc","hm")],
                                           X=X,
                                           Xfut=Xfut,
                                           iFut=cpt,
                                           listOption=listOption)
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse=NA #to not store twice the same information
    lst.QUALYPSOOUT_temp[[cpt]]$RESERR=NA
    lst.QUALYPSOOUT_temp[[cpt]]$CHANGEBYEFFECT=NA
    lst.QUALYPSOOUT_temp[[cpt]]$CLIMATEESPONSE$YStar=NA
    if(cpt!=1){
      lst.QUALYPSOOUT_temp[[cpt]]$CLIMATEESPONSE=NA #to not store 9892 times the same information, stored only the first time
      lst.QUALYPSOOUT_temp[[cpt]]$Y=NA #to not store 9892 times the same information, stored only the first time
    }
    if(((cpt) %% 10)==0){print(cpt)}
  }
  save(lst.QUALYPSOOUT_temp,file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_temp.RData"))
  rm(lst.QUALYPSOOUT_temp,listOption) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
  closeAllConnections()
  gc()
  rm(Y,X)
  closeAllConnections()
  gc()
  toc()
}
