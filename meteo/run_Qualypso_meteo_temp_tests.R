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
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"
nbcore=detectCores()-2 #Number of cores for parallelization


load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"refs.Rdata"))

ref_year=1990# central year of 1975-2005 reference period

######
#MAIN#
######


for(v in unique(simu_lst$var)[c(1,2)]){
  dir.create(paste0(path_data,"Qualypso/",v,"/"))
  if(v=="tasAdjust"){
    typechangeVar="abs"
    SPAR=1
  }else{
    typechangeVar="rel"
    SPAR=1.1
  }
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[17]){
    tic()
    
    scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
    global_tas=prep_global_tas(path_temp,ref_year=ref_year,simu_lst=scenAvail)
    
    dir.create(paste0(path_data,"Qualypso/",v,"/",i))
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
    ClimateProjections=Reduce(function(...) merge(...,by="year", all=T), all_chains)#allows for NA
    Y=abind(split(data.frame(t(ClimateProjections[,-1])),rep(seq(1,length(all_chains)),each=n_pix) ), along=3)
    Y=aperm(Y,c(1,3,2))
    vec_years=unique(ClimateProjections$year)
    X=global_tas[["mat_Globaltas"]][,global_tas[["gcm_years"]] %in% vec_years]
    Xfut=c(global_tas[["warming_1990"]],seq(0.7,1.5,0.1))
    rm(ClimateProjections)
    gc()
    
    tmp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = typechangeVar,spar = SPAR,type="spline",nbcores = nbcore)
    listOption = list(spar=SPAR,typeChangeVariable=typechangeVar,ANOVAmethod="lm",nBurn=1000,nKeep=5000,nCluster=nbcore,probCI=0.9,quantilePosterior =0.5,climResponse=tmp)
    rm(tmp)
    gc()
    
    lst.QUALYPSOOUT_temp_3rcp=vector(mode="list",length=length((Xfut)))
    cpt=1
    for(x in Xfut){
      lst.QUALYPSOOUT_temp_3rcp[[cpt]] = QUALYPSO(Y=Y, #one Y and run per pixel because otherwise we cannot have several future times
                                                            scenAvail=scenAvail[,c("rcp","gcm","rcm","bc")],
                                                            X=X,
                                                            Xfut=Xfut,
                                                            iFut=cpt,
                                                            listOption=listOption)
      lst.QUALYPSOOUT_temp_3rcp[[cpt]]$listOption$climResponse=NA #to not store twice the same information
      lst.QUALYPSOOUT_temp_3rcp[[cpt]]$RESERR=NA
      lst.QUALYPSOOUT_temp_3rcp[[cpt]]$CHANGEBYEFFECT=NA
      lst.QUALYPSOOUT_temp_3rcp[[cpt]]$CLIMATEESPONSE$YStar=NA
      if(x!=Xfut[1]){
        lst.QUALYPSOOUT_temp_3rcp[[cpt]]$CLIMATEESPONSE=NA #to not store 9892 times the same information, stored only the first time
        lst.QUALYPSOOUT_temp_3rcp[[cpt]]$Y=NA #to not store 9892 times the same information, stored only the first time
      }
      print(cpt)
      cpt=cpt+1
    }
    
    rm(Y,X,listOption)
    closeAllConnections()
    gc()
    save(lst.QUALYPSOOUT_temp_3rcp,file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_allyears.RData"))
    rm(lst.QUALYPSOOUT_temp_3rcp) # on local computer (don't know for server) performances degrade through iterations (due to memory saturation? And memory is only given back by closing R)
    closeAllConnections()
    gc()
    
    toc()
  }
}

