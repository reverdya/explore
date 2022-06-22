# Alix Reverdy
# Explore 2
# Calculate hydrological indicators from hydrological projections (sample of indicators on SIM2). Checks for Na in raw projections

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

path_data="C:/Users/reverdya/Documents/Docs/2_data/"

var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

time_origin=ymd("1949-12-01") #for SIM2 data

#Netcdf could be modified for coherence of varid with var name using cdo
var_id=list("debit")
names(var_id)=var


sim_stations=read.csv(file = paste0(path_data,"raw/SIM2/Infos_stations_modcou_OK.csv"),sep=";")
watershed_select=sim_stations$Num_ordre_Modcou #take only stations with coordinates
watershed_select=watershed_select[!watershed_select %in% c(626,669,763,764,861)]#take only stations with no NA

period_agreg=c("date","year","season","month") #different periods of aggregations for indicators

######
#MAIN#
######

###########################################
## Make list of calculated variables

lst_indic=c("Q_mean_year","Q_q95_year","VCN10")
name_indic=c("Module annuel du débit","Q95 annuel du débit","log(VCN10)")
save(lst_indic,name_indic,file=paste0(path_data,"processed/lst_indic.Rdata"))
load(file=paste0(path_data,"processed/lst_indic.Rdata"))


###################################################
## Make table of available simulations

simu_lst=vector(mode = "list",length=6)
names(simu_lst)=c("var","rcp","gcm","rcm","bc","hm")
for (v in var){
  for(r in rcp[-1]){# -1 for not historical
    for(b in bc){
      for(h in hm){
        lst_f=list.files(paste0(path_data,"raw/",h,"/",v,"/"),pattern=glob2rx(paste0(v,"*",gsub("[.]","",r),"*",b,"*",h,"*"))) #list all files of projections in folder
        gcm=sapply(strsplit(lst_f,"_"),"[",4)
        rcm=sapply(strsplit(lst_f,"_"),"[",5)
        n=length(gcm)
        simu_lst$var=c(simu_lst$var,rep(v,n))
        simu_lst$rcp=c(simu_lst$rcp,rep(r,n))
        simu_lst$gcm=c(simu_lst$gcm,gcm)
        simu_lst$rcm=c(simu_lst$rcm,rcm)
        simu_lst$bc=c(simu_lst$bc,rep(b,n))
        simu_lst$hm=c(simu_lst$hm,rep(h,n))
      }
    }
  }
}
simu_lst=data.frame(simu_lst)
simu_lst[simu_lst$rcm=="REMO2009",]$rcm="REMO"# the 2 versions of REMO have been signaled as identical
simu_lst[simu_lst$rcm=="REMO2015",]$rcm="REMO"
save(simu_lst,file=paste0(path_data,"processed/simu_lst.Rdata"))

######################################################################################
# Calculate an indicator and save it as a specific file (one per chain and indicator)

load(file=paste0(path_data,"processed/simu_lst.Rdata"))#list of available simulations

## Function that calculates a specific statistical "stat" function on a given "chain" with a potential roll mean of agreg_day. Saves it with an "indic_name"
calc_indic=function(chain,indic_name,stat,agreg_day){
  
  pth_tmp=list.files(paste0(path_data,"raw/",chain$hm,"/",chain$var,"/"),pattern=glob2rx(paste0(chain$var,"*",gsub("[.]","",chain$rcp),"*",chain$gcm,"*",chain$rcm,"*",chain$bc,"*",chain$hm,"*")),full.names = T)
  nc=load_nc(pth_tmp)#retrieve raw projection for given chain
  dates=nc$dim$time$vals
  dates=as_date(dates,origin=time_origin) #get dates for future
  fut=ncvar_get(nc,varid=var_id[[chain$var]])# get data for future
  rm(nc)
  gc()# memory management as some files can get big
  fut=data.frame(dates,t(fut))
  colnames(fut)[1]="date"
  rm(dates)
  gc()
  #Same than before but for historical
  pth_tmp=list.files(paste0(path_data,"raw/",chain$hm,"/",chain$var,"/"),pattern=glob2rx(paste0(chain$var,"*historical*",chain$gcm,"*",chain$rcm,"*",chain$bc,"*",chain$hm,"*")),full.names = T)
  nc=load_nc(pth_tmp)
  dates=nc$dim$time$vals
  dates=as_date(dates,origin=time_origin)
  histo=ncvar_get(nc,varid=var_id[[chain$var]])
  rm(nc)
  gc()
  histo=data.frame(dates,t(histo))
  colnames(histo)[1]="date"
  rm(dates)
  gc()
  # Combine historical and rcp
  data=rbind(histo,fut)
  rm(histo)
  rm(fut)
  gc()
  
  #Only stations with no NA and coordinates
  data=data[,c(1,watershed_select+1)]#+1 because of date
  if(any(is.na(data))){stop("This chain has NA")}
  
  # Set negative values to 0
  if(any(data[,colnames(data)!="date"]<0)){
    print("This chain has negative values")
    data[,colnames(data)!="date"][data[,colnames(data)!="date"]<0]=0
  }

  
  data$year=year(data$date)
  data$season=date_to_season(data$date)
  data$month=month(data$date)
  
  #10 days rolling mean
  if(any(agreg_day==10)){
    data10=data
    data10[,!colnames(data10) %in% period_agreg ]=apply(data[,!colnames(data) %in% period_agreg ],MARGIN = 2,function(x) rollmean(x,k=10,align="center",fill=NA))
  }
  
  #Remove potential uncomplete first and last years
  if(mday(data$date[1])!=1 | data$month[1]!=1){
    data=data[-which(data$year==data$year[1]),]
    data10=data10[-which(data10$year==data10$year[1]),]
  }
  if(mday(data$date[nrow(data)])!=31 | data$month[nrow(data)]!=12){
    data=data[-which(data$year==data$year[nrow(data)]),]
    data10=data10[-which(data10$year==data10$year[nrow(data10)]),]
  }
  
  if(any(data10[,!colnames(data10) %in% period_agreg ]<0)){
    data10[,!colnames(data10) %in% period_agreg ][data10[,!colnames(data10) %in% period_agreg ]<0]=0#for some reason roll mean of very small positive values produces very small negative values
  }
  
  #Calculate statistic
  j=1
  for (st in stat){
    if(agreg_day[j]==10){
      res=st(data10)
    }
    if(agreg_day[j]==1){
      res=st(data)
    }
    # if(indic_name[j]=="log10VCN10"){
    #   res[res==0]=1e-40 #to allow use of res
    #   res[,!colnames(res) %in% "year"]=log10(res[,!colnames(res) %in% "year"])
    # }
    if(any(is.na(res))){
      print(i)
      print(j)
      stop("NA production")
      #res[rowSums(res<0)>0,]
    }
    save(res,file = paste0(path_data,"processed/indic_hydro/",indic_name[j],"_",chain$rcp,"_",chain$gcm,"_",chain$rcm,"_",chain$bc,"_",chain$hm,".Rdata"))
    rm(res)
    gc()
    j=j+1
  }
  
  rm(data)
  rm(data10)
  gc()
  
}

###########################################
## Series of statistical functions

## Out of convenience the hydrological year is always the civilian year, but this will need to be optimized

mean_year=function(data){aggregate(data[,-which(colnames(data) %in% period_agreg )],by=list(year=data$year),mean)}
q95_year=function(data){aggregate(data[,-which(colnames(data) %in% period_agreg )],by=list(year=data$year),quantile,probs=0.95)}
vcn10=function(data){aggregate(data[,-which(colnames(data) %in% period_agreg )],by=list(year=data$year),min,na.rm=T)}# na.rm because potential NA from rolling mean if start on 1st jan. or ends on 31st dec. 
vcn10_day=function(data){aggregate(data[,-which(colnames(data) %in% period_agreg )],by=list(year=data$year),which.min )}#day of year of VCN10

#################################################################################
## In this loop old indicators can easily be commented and new ones created
## When adding something with month or season check for coherence as never ran

tic()

for (i in 1:nrow(simu_lst)){
  #calc_indic(chain=simu_lst[i,],indic_name=lst_indic,stat = list(mean_year,q95_year,vcn10),agreg_day=c(1,1,10))
  calc_indic(chain=simu_lst[i,],indic_name=lst_indic[3],stat = list(vcn10),agreg_day=c(10))
  print(simu_lst[i,])
}

toc()


