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

typechangeVar="rel"
SPAR_time=1.1
SPAR_temp=1.2
basin_sample=ref[ref$n==9,]$code
n_bv=length(basin_sample)
# bc_sample=c("ADAMONT","CDFt")
bc_sample=c("ADAMONT")
hm_sample=c("CTRIP","EROS","GRSD","J2000","MORDOR-SD","MORDOR-TS","ORCHIDEE","SIM2","SMASH")
hm_sample2=c("CTRIP","GRSD","MORDOR-SD","ORCHIDEE","SMASH")


######
#MAIN#
######


###########################################################################################
## Run for all steps and all models hydro for biggest common sample (Loire SIM2 stations)

for(i in unique(simu_lst$indic)){
  dir.create(paste0(path_data,"Qualypso/",i,"/"))

  tic()
  scenAvail=simu_lst[simu_lst$indic==i,]
  all_chains=vector(length=nrow(scenAvail),mode="list")
  for(c in 1:nrow(scenAvail)){# for each chain
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*",scenAvail$gcm[c],"*",scenAvail$rcp[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",scenAvail$hm[c],"*")))
    pth_tmp=list.files(dir_tmp,full.names=T,recursive = T,include.dirs = F,pattern=glob2rx(paste0("*",i,".f*")))
    res=read_fst(pth_tmp)
    colnames(res)=c("gcm","rcp","rcm","bc","hm","code","year","indic")
    res$year=year(res$year)
    #res=res[!is.na(res$indic),]
    res=res[,c("year","code","indic")]
    if(scenAvail$rcp=="rcp26"&scenAvail$gcm=="EC-EARTH"&scenAvail$rcm=="HadREM3-GA7-05"){
      res=res[!nrow(res),]
    }
    res=pivot_wider(res,names_from = code,values_from = indic)
    all_chains[[c]]=res
  }

  all_chains=all_chains[scenAvail$bc %in% bc_sample]
  scenAvail=scenAvail[scenAvail$bc %in% bc_sample,]
  all_chains=all_chains[scenAvail$hm %in% hm_sample]
  scenAvail=scenAvail[scenAvail$hm %in% hm_sample,]
  all_chains=lapply(all_chains,function(x) x[,c("year",basin_sample)])

  ClimateProjections=Reduce(function(...) merge(...,by="year", all=T), all_chains)#allows for NA
  Y=abind(split(data.frame(t(ClimateProjections[,-1])),rep(seq(1,length(all_chains)),each=n_bv) ), along=3)
  Y=aperm(Y,c(1,3,2))
  X=unique(ClimateProjections$year)
  Xfut=sort(c(seq(ref_year,final_year,10),2085))
  rm(ClimateProjections,all_chains)
  gc()

  ## Time
  tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = rep(SPAR_time,nrow(scenAvail)),type="spline",nbcores = nbcore)
  listOption = list(spar=SPAR_time,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
  rm(tmp)
  gc()

  lst.QUALYPSOOUT_time=vector(mode="list",length=length((Xfut)))
  for(cpt in 1:length(Xfut)){
    lst.QUALYPSOOUT_time[[cpt]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                           # scenAvail=scenAvail[,c("rcp","gcm","rcm","bc","hm")],
                                           scenAvail=scenAvail[,c("rcp","gcm","rcm","hm")],
                                           X=X,
                                           Xfut=Xfut,
                                           iFut=cpt,
                                           listOption=listOption)
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$climateResponse=NA #to not store twice the same information
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$YStar=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$phiStar=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$etaStar=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$phi=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$varInterVariability=NA
    lst.QUALYPSOOUT_time[[cpt]]$RESERR=NA
    lst.QUALYPSOOUT_time[[cpt]]$CHANGEBYEFFECT=NA
    lst.QUALYPSOOUT_time[[cpt]]$CLIMATEESPONSE$YStar=NA
    if(cpt!=1){
      lst.QUALYPSOOUT_time[[cpt]]$CLIMATEESPONSE=NA #to not store 4000 times the same information, stored only the first time
      lst.QUALYPSOOUT_time[[cpt]]$Y=NA #to not store 4000 times the same information, stored only the first time
    }
    if(((cpt) %% 2)==0){print(cpt)}
  }
  rm(listOption)
  closeAllConnections()
  gc()
  save(lst.QUALYPSOOUT_time,file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time.RData"))
  rm(lst.QUALYPSOOUT_time) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
  closeAllConnections()
  gc()

  ## Temperature
  vec_years=X
  global_tas=prep_global_tas(path_temp,simu_lst=scenAvail,cat="hydro")
  X=as.matrix(t(global_tas[["mat_Globaltas"]][global_tas[["gcm_years"]] %in% vec_years[vec_years!=2099],]))#have to remove 2099/2100, because Tyear_FR is not available for HadGEM2
  Xmax=round(max(X,na.rm=T),1)-0.1#goes far to be able to make the correspondance with global temperature
  Xfut=c(seq(0,Xmax,0.1))
  idx_rcp=which(scenAvail$rcp=="rcp85")
  scenAvail=scenAvail[idx_rcp,]
  X=X[idx_rcp,]
  if(any(vec_years==2099)){
    if(any(vec_years==2100)){
      Y=Y[,idx_rcp,-c(dim(Y)[3],dim(Y)[3]-1)]#have to remove 2099 and 2100, because Tyear_FR is not available for HadGEM2
    }else{
      Y=Y[,idx_rcp,-(dim(Y)[3])]#have to remove 2099, because Tyear_FR is not available for HadGEM2
    }
  }else{
    Y=Y[,idx_rcp,]
  }


  tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = rep(SPAR_temp,nrow(scenAvail)),type="spline",nbcores = nbcore,scenAvail = scenAvail)
  listOption = list(spar=SPAR_temp,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
  rm(tmp)
  gc()

  lst.QUALYPSOOUT_temp=vector(mode="list",length=length((Xfut)))
  for(cpt in 1:length(Xfut)){
    lst.QUALYPSOOUT_temp[[cpt]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                           # scenAvail=scenAvail[,c("gcm","rcm","bc","hm")],
                                           scenAvail=scenAvail[,c("gcm","rcm","hm")],
                                           X=X,
                                           Xfut=Xfut,
                                           iFut=cpt,
                                           listOption=listOption)
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$climateResponse=NA #to not store twice the same information
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$YStar=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$phiStar=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$etaStar=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$phi=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$varInterVariability=NA
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
  print(i)
  toc()
}


##################################################################################################
## Run for all steps and all models hydro for biggest common national sample  (without SIM2)


codes=vector(length=length(hm_sample2),mode="list")
for(c in 1:length(hm_sample2)){# for each chain
  if(hm_sample2[c]!="SMASH"){#because some station not simulated for SMASH
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*CNRM-CM5*rcp85*ALADIN*ADAMONT*",hm_sample2[c],"*")))
  }else{
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*CNRM-CM5*rcp85*ALADIN*CDFt*",hm_sample2[c],"*")))
  }
  pth_tmp=list.files(dir_tmp,full.names=T,recursive = T,include.dirs = F,pattern=glob2rx(paste0("*QA.f*")))
  res=read_fst(pth_tmp)
  colnames(res)=c("gcm","rcp","rcm","bc","hm","code","year","indic")
  codes[[c]]=unique(res$code)
}
basin_sample2=Reduce(intersect, codes)
n_bv2=length(basin_sample2)

for(i in unique(simu_lst$indic)){
  dir.create(paste0(path_data,"Qualypso/",i,"/"))
  
  tic()
  scenAvail=simu_lst[simu_lst$indic==i,]
  all_chains=vector(length=nrow(scenAvail),mode="list")
  for(c in 1:nrow(scenAvail)){# for each chain
    dir_tmp <- list.files(paste0(path_data,"indic"), recursive = TRUE, include.dirs = TRUE,full.names = T,pattern =glob2rx(paste0("*",scenAvail$gcm[c],"*",scenAvail$rcp[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",scenAvail$hm[c],"*")))
    pth_tmp=list.files(dir_tmp,full.names=T,recursive = T,include.dirs = F,pattern=glob2rx(paste0("*",i,".f*")))
    res=read_fst(pth_tmp)
    colnames(res)=c("gcm","rcp","rcm","bc","hm","code","year","indic")
    res$year=year(res$year)
    #res=res[!is.na(res$indic),]
    res=res[,c("year","code","indic")]
    if(scenAvail$rcp=="rcp26"&scenAvail$gcm=="EC-EARTH"&scenAvail$rcm=="HadREM3-GA7-05"){
      res=res[!nrow(res),]
    }
    res=pivot_wider(res,names_from = code,values_from = indic)
    all_chains[[c]]=res
  }
  
  all_chains=all_chains[scenAvail$bc %in% bc_sample]
  scenAvail=scenAvail[scenAvail$bc %in% bc_sample,]
  all_chains=all_chains[scenAvail$hm %in% hm_sample2]
  scenAvail=scenAvail[scenAvail$hm %in% hm_sample2,]
  all_chains=lapply(all_chains,function(x) x[,c("year",basin_sample2)])
  
  ClimateProjections=Reduce(function(...) merge(...,by="year", all=T), all_chains)#allows for NA
  Y=abind(split(data.frame(t(ClimateProjections[,-1])),rep(seq(1,length(all_chains)),each=n_bv2) ), along=3)
  Y=aperm(Y,c(1,3,2))
  X=unique(ClimateProjections$year)
  Xfut=sort(c(seq(ref_year,final_year,10),2085))
  rm(ClimateProjections,all_chains)
  gc()
  
  ## Time
  tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = rep(SPAR_time,nrow(scenAvail)),type="spline",nbcores = nbcore)
  listOption = list(spar=SPAR_time,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
  rm(tmp)
  gc()
  
  lst.QUALYPSOOUT_time=vector(mode="list",length=length((Xfut)))
  for(cpt in 1:length(Xfut)){
    lst.QUALYPSOOUT_time[[cpt]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                           # scenAvail=scenAvail[,c("rcp","gcm","rcm","bc","hm")],
                                           scenAvail=scenAvail[,c("rcp","gcm","rcm","hm")],
                                           X=X,
                                           Xfut=Xfut,
                                           iFut=cpt,
                                           listOption=listOption)
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$climateResponse=NA #to not store twice the same information
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$YStar=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$phiStar=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$etaStar=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$phi=NA
    lst.QUALYPSOOUT_time[[cpt]]$listOption$climResponse$varInterVariability=NA
    lst.QUALYPSOOUT_time[[cpt]]$RESERR=NA
    lst.QUALYPSOOUT_time[[cpt]]$CHANGEBYEFFECT=NA
    lst.QUALYPSOOUT_time[[cpt]]$CLIMATEESPONSE$YStar=NA
    if(cpt!=1){
      lst.QUALYPSOOUT_time[[cpt]]$CLIMATEESPONSE=NA #to not store 4000 times the same information, stored only the first time
      lst.QUALYPSOOUT_time[[cpt]]$Y=NA #to not store 4000 times the same information, stored only the first time
    }
    if(((cpt) %% 2)==0){print(cpt)}
  }
  rm(listOption)
  closeAllConnections()
  gc()
  save(lst.QUALYPSOOUT_time,file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_time_FR.RData"))
  rm(lst.QUALYPSOOUT_time) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
  closeAllConnections()
  gc()
  
  ## Temperature
  vec_years=X
  global_tas=prep_global_tas(path_temp,simu_lst=scenAvail,cat="hydro")
  X=as.matrix(t(global_tas[["mat_Globaltas"]][global_tas[["gcm_years"]] %in% vec_years[vec_years!=2099],]))#have to remove 2099/2100, because Tyear_FR is not available for HadGEM2
  Xmax=round(max(X,na.rm=T),1)-0.1#goes far to be able to make the correspondance with global temperature
  Xfut=c(seq(0,Xmax,0.1))
  idx_rcp=which(scenAvail$rcp=="rcp85")
  scenAvail=scenAvail[idx_rcp,]
  X=X[idx_rcp,]
  if(any(vec_years==2099)){
    if(any(vec_years==2100)){
      Y=Y[,idx_rcp,-c(dim(Y)[3],dim(Y)[3]-1)]#have to remove 2099 and 2100, because Tyear_FR is not available for HadGEM2
    }else{
      Y=Y[,idx_rcp,-(dim(Y)[3])]#have to remove 2099, because Tyear_FR is not available for HadGEM2
    }
  }else{
    Y=Y[,idx_rcp,]
  }
  
  
  tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = rep(SPAR_temp,nrow(scenAvail)),type="spline",nbcores = nbcore,scenAvail = scenAvail)
  listOption = list(spar=SPAR_temp,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
  rm(tmp)
  gc()
  
  lst.QUALYPSOOUT_temp=vector(mode="list",length=length((Xfut)))
  for(cpt in 1:length(Xfut)){
    lst.QUALYPSOOUT_temp[[cpt]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                           # scenAvail=scenAvail[,c("gcm","rcm","bc","hm")],
                                           scenAvail=scenAvail[,c("gcm","rcm","hm")],
                                           X=X,
                                           Xfut=Xfut,
                                           iFut=cpt,
                                           listOption=listOption)
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$climateResponse=NA #to not store twice the same information
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$YStar=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$phiStar=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$etaStar=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$phi=NA
    lst.QUALYPSOOUT_temp[[cpt]]$listOption$climResponse$varInterVariability=NA
    lst.QUALYPSOOUT_temp[[cpt]]$RESERR=NA
    lst.QUALYPSOOUT_temp[[cpt]]$CHANGEBYEFFECT=NA
    lst.QUALYPSOOUT_temp[[cpt]]$CLIMATEESPONSE$YStar=NA
    if(cpt!=1){
      lst.QUALYPSOOUT_temp[[cpt]]$CLIMATEESPONSE=NA #to not store 9892 times the same information, stored only the first time
      lst.QUALYPSOOUT_temp[[cpt]]$Y=NA #to not store 9892 times the same information, stored only the first time
    }
    if(((cpt) %% 10)==0){print(cpt)}
  }
  save(lst.QUALYPSOOUT_temp,file=paste0(path_data,"Qualypso/",i,"/",i,"_list_QUALYPSOOUT_temp_FR.RData"))
  rm(lst.QUALYPSOOUT_temp,listOption) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
  closeAllConnections()
  gc()
  rm(Y,X)
  closeAllConnections()
  gc()
  print(i)
  toc()
}
