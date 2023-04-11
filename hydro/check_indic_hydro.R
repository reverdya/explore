# Alix Reverdy
# Explore 2
# check meteorological indicators: fit of splines

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(plot.matrix)

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
hm=c("CTRIP","EROS","GRSD","J2000","MORDOR-SD","MORDOR-TS","SMASH")
hm_domain=c("FR","Lo-Br","FR","Lo-Rh","FR","Lo","FR")
hm_NBbc=c(1,2,2,2,2,2,2)

centr_ref_year=1990# central year of 1975-2005 reference period



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
# simu_lst[simu_lst$rcm=="REMO2009",]$rcm="REMO"# the 2 versions of REMO have been signaled as identical
# simu_lst[simu_lst$rcm=="REMO2015",]$rcm="REMO"
save(simu_lst,file=paste0(path_data,"simu_lst.Rdata"))
load(paste0(path_data,"simu_lst.Rdata"))

#######################################################################################
## Extract coordinates and indexes of reference basins



###############################################################################################
## Plot raw indicator , and its spline for all models and selection of watersheds by RCP for time
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
# type : raw_spline, raw, diff, diff_spline

##Merge data frame warnings are okay
## Sometimes problem with too many connections opened requires running step by step (i by i)


# for(v in unique(simu_lst$var)){
#   dir.create(paste0(path_fig,v,"/"))
#   if(v=="prsnAdjust"){
#     ref_c=ref_snow
#   }else{
#     ref_c=ref_cities
#   }
#   for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
#     closeAllConnections()
#     gc()
#     dir.create(paste0(path_fig,v,"/",i))
#     
#     scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
#     global_tas=prep_global_tas(path_temp,ref_year=centr_ref_year,simu_lst=scenAvail)
#     all_chains=extract_chains(scenAvail=scenAvail,ref_cities=ref_c)
#     for(c in 1:nrow(ref_c)){
#       for(R in c("rcp26","rcp45","rcp85")){
#         # plot_spline(all_chains=all_chains,type="diff",pred="time",scenAvail = scenAvail,SPAR=1,rcp=R,city_name = ref_c$name[c],idx=c)
#         # plot_spline(all_chains=all_chains,type="raw",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],idx=c)
#         for(S in c(0.8,0.9,1,1.1,1.2)){
#           plot_spline(all_chains=all_chains,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],idx=c)
#           # plot_spline(all_chains=all_chains,type="diff_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],idx=c)
#         }
#       }
#       for(R in c("rcp85")){
#         # plot_spline(all_chains=all_chains,type="raw",pred="temp",scenAvail = scenAvail,SPAR=1,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
#         # plot_spline(all_chains=all_chains,type="diff",pred="temp",scenAvail = scenAvail,SPAR=1,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
#         for(S in c(1.2,1.3,1.4,1.5,1.6)){
#           plot_spline(all_chains=all_chains,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
#           # plot_spline(all_chains=all_chains,type="diff_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = ref_c$name[c],globaltas = global_tas,idx=c)
#         }
#       }
#     }
#   }
# }
#   
# ## Idem for regions hydro and departments
# 
# 
# for(v in unique(simu_lst$var)[unique(simu_lst$var)!="prsnAdjust"]){
#   dir.create(paste0(path_fig,v,"/"))
#   for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
#     closeAllConnections()
#     gc()
#     dir.create(paste0(path_fig,v,"/",i))
#     dir.create(paste0(path_fig,v,"/",i,"/reg/"))
#     dir.create(paste0(path_fig,v,"/",i,"/dep/"))
#     
#     scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
#     global_tas=prep_global_tas(path_temp,ref_year=centr_ref_year,simu_lst=scenAvail)
#     all_chains_reg=extract_chains(scenAvail,ref_cities = ref_reg[idx_ref_reg,],type = "reg")
#     all_chains_dep=extract_chains(scenAvail,ref_cities = ref_dep[idx_ref_dep,],type = "dep")
#     for(c in 1:nrow(ref_reg[idx_ref_reg,])){
#       for(R in c("rcp26","rcp45","rcp85")){
#         for(S in c(0.8,0.9,1,1.1,1.2)){
#           plot_spline(all_chains=all_chains_reg,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_reg$name[idx_ref_reg[c]]),categ="reg",idx=c)
#         }
#       }
#       for(R in c("rcp85")){
#         for(S in c(1.2,1.3,1.4,1.5,1.6)){
#           plot_spline(all_chains=all_chains_reg,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_reg$name[idx_ref_reg[c]]),globaltas = global_tas,categ="reg",idx=c)
#         }
#       }
#     }
#     for(c in 1:nrow(ref_dep[idx_ref_dep,])){
#       for(R in c("rcp26","rcp45","rcp85")){
#         for(S in c(0.8,0.9,1,1.1,1.2)){
#           plot_spline(all_chains=all_chains_dep,type="raw_spline",pred="time",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_dep$name[idx_ref_dep[c]]),categ="dep",idx=c)
#         }
#       }
#       for(R in c("rcp85")){
#         for(S in c(1.2,1.3,1.4,1.5,1.6)){
#           plot_spline(all_chains=all_chains_dep,type="raw_spline",pred="temp",scenAvail = scenAvail,SPAR=S,rcp=R,city_name = gsub(" ","",ref_dep$name[idx_ref_dep[c]]),globaltas = global_tas,categ="dep",idx=c)
#         }
#       }
#     }
#   }
# }

