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

# for(v in unique(simu_lst$var)){
for(v in unique(simu_lst$var)[c(2)]){
  dir.create(paste0(path_data,"Qualypso/",v,"/"))
  if(v=="tasAdjust"){
    typechangeVar="abs"
    SPAR=1
  }else{
    typechangeVar="rel"
    SPAR=1.1
  }
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[14]){
  # for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
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
      if(scenAvail$bc[c]=="R2D2"){
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
    lst.QUALYPSOOUT_time=vector(mode="list",length=n_pix)
    
    tic()
    for(p in 2:(n_pix+1)){
      ClimateProjections=lapply(all_chains, function(x) x[,c(1,p)])
      Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections)[,-1])#allows for NA
      X=Reduce(function(...) merge(...,by="year", all=T), ClimateProjections)[,1]
      #Warnings okay
      
      ##Time predictor
      listOption = list(spar=SPAR,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
      
      lst.QUALYPSOOUT_time[[p-1]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                             scenAvail=scenAvail[,c("rcp","gcm","rcm","bc")],
                                             X=X,
                                             Xref = ref_year,
                                             Xfut = X,
                                             listOption=listOption)# no Xfut or iFut because we want all values
      if(((p-1) %% 500)==0){print(p-1)}
    }
    save(lst.QUALYPSOOUT_time,file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_lm.RData"))
    toc() 
  }
  rm(ClimateProjections,Y,X,lst.QUALYPSOOUT_time) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
  closeAllConnections()
  gc()
}

