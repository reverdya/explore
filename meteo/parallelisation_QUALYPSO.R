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

path_data="C:/Users/reverdya/Documents/Docs/2_Data/processed/Explore2-meteo/"
nbcore=detectCores()-2 #Number of cores for parallelization


load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"refs.Rdata"))

ref_year=1990# central year of 1975-2005 reference period
first_ref_year=1975
last_ref_year=2005
first_data_year=1950
last_data_year=2100
vecYears=seq(first_data_year,last_data_year)

######
#MAIN#
######

v=unique(simu_lst$var)[c(1)]
dir.create(paste0(path_data,"Qualypso/parallel_test/"))
if(v=="tasAdjust"){
  typechangeVar="abs"
  SPAR=1
}else{
  typechangeVar="rel"
  SPAR=1.1
}
i=unique(simu_lst[simu_lst$var==v,]$indic)[5]
dir.create(paste0(path_data,"Qualypso/",v,"/",i))
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
  if(scenAvail$bc[c]=="CDFt"){
    full_years=year(as.Date(full_years,origin="1850-01-01"))
  }
  nc_close(nc)
  rm(nc)
  gc()
  
  vec_mask=as.logical(refs$mask)
  dim(res)=c(dim(res)[1]*dim(res)[2],dim(res)[3])# collapses one dimension
  res=res[vec_mask,]
  res=cbind(full_years,t(res))
  colnames(res)[1]="year"
  all_chains[[c]]=res
}
n_pix=ncol(all_chains[[1]])-1




## Qualypso parallelised over space fit climate response outside
start <- Sys.time ()

ClimateProjections=Reduce(function(...) merge(...,by="year", all=T), all_chains)#allows for NA
X=unique(ClimateProjections$year)
Y=abind(split(data.frame(t(ClimateProjections[,-1])),rep(seq(1,length(all_chains)),each=n_pix) ), along=3)
lst.QUALYPSOOUT_space_parallel=vector(mode="list",length=length(X))
rm(ClimateProjections)
gc()
Y=aperm(Y,c(1,3,2))
Xfut=seq(ref_year,X[length(X)])
tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = SPAR,type="spline",nbcores = nbcore)
listOption = list(spar=SPAR,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
rm(tmp)
gc()
for(x in Xfu){
  lst.QUALYPSOOUT_space_parallel[[x+1-Xfut[1]]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                                         scenAvail=scenAvail[,c("rcp","gcm","rcm","bc")],
                                                         X=X,
                                                         Xfut = Xfut,
                                                         iFut=x+1-Xfut[1],
                                                         listOption=listOption)
  lst.QUALYPSOOUT_space_parallel[[x+1-Xfut[1]]]$listOption$climResponse=NA #to not store twice the same information
  lst.QUALYPSOOUT_space_parallel[[x+1-Xfut[1]]]$RESERR=NA
  lst.QUALYPSOOUT_space_parallel[[x+1-Xfut[1]]]$CHANGEBYEFFECT=NA
  if(x!=Xfut[1]){
    lst.QUALYPSOOUT_space_parallel[[x+1-Xfut[1]]]$CLIMATEESPONSE=NA #to not store 9892 times the same information, stored only the first time
    lst.QUALYPSOOUT_space_parallel[[x+1-Xfut[1]]]$Y=NA #to not store 9892 times the same information, stored only the first time
  }else{
    lst.QUALYPSOOUT_space_parallel[[x+1-Xfut[1]]]$CLIMATEESPONSE$YStar=NA
  }
  if(((x+1-Xfut[1]) %% 10)==0){print(x+1-Xfut[1])}
}

rm(Y,X,listOption)
closeAllConnections()
gc()
save(lst.QUALYPSOOUT_space_parallel,file=paste0(path_data,"Qualypso/parallel_test/",v,"_",i,"_list_QUALYPSOOUT_space_parallel.RData"))
rm(lst.QUALYPSOOUT_space_parallel) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
closeAllConnections()
gc()

times_store=Sys.time () - start
save(times_store,file=paste0(path_data,"Qualypso/parallel_test/times_run_parallel.RData"))





## Qualypso parallelised over time
start <- Sys.time ()
lst.QUALYPSOOUT_time_parallel=vector(mode="list",length=n_pix)
for(p in 2:(n_pix+1)){
  ClimateProjections=lapply(all_chains, function(x) x[,c(1,p)])
  Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections)[,-1])#allows for NA
  X=Reduce(function(...) merge(...,by="year", all=T), ClimateProjections)[,1]
  listOption = list(spar=SPAR,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5)
  
  lst.QUALYPSOOUT_time_parallel[[p-1]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                         scenAvail=scenAvail[,c("rcp","gcm","rcm","bc")],
                                         X=X,
                                         Xfut = seq(ref_year,X[length(X)]),
                                         listOption=listOption)# no Xfut or iFut because we want all values
  if(((p-1) %% 500)==0){print(p-1)}
}
rm(ClimateProjections,Y,X)
gc()
save(lst.QUALYPSOOUT_time_parallel,file=paste0(path_data,"Qualypso/parallel_test/",v,"_",i,"_list_QUALYPSOOUT_time_parallel.RData"))
rm(lst.QUALYPSOOUT_time_parallel) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
closeAllConnections()
gc()
times_store=c(times_store,Sys.time () - start)
save(times_store,file=paste0(path_data,"Qualypso/parallel_test/times_run_parallel.RData"))







