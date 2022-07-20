# Alix Reverdy
# Explore 2
# Make figures from Qualypso runs

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(knitr)
library(markdown)
library(rmarkdown)
#library(tinytex) # for pdf
#library(cowplot)##grid plot of png in knitr
library(bookdown)# figure numbers
library(gridExtra)#multiplot

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/"
path_Rmd="C:/Users/reverdya/Documents/Docs/1_code/explore/fiche_exemple_COUT.Rmd"

load(file=paste0(path_data,"processed/lst_indic.Rdata"))
units=c("m3/s","m3/s","log(m3/s)","day of year")
load(file=paste0(path_data,"processed/simu_lst.Rdata"))

centr_ref_year=1990# central year of 1975-2005 reference period
typeChangeVar="rel"
first_ref_year=1975
last_ref_year=2005
first_full_year=1972# from raw data filenames
last_full_year=2098# from raw data filenames
first_data_year=1951
last_data_year=2099

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions

######
#MAIN#
######

sim_stations=read.csv(file = paste0(path_data,"raw/SIM2/Infos_stations_modcou_OK.csv"),sep=";")
sim_stations=sim_stations[!sim_stations$Num_ordre_Modcou %in% c(626,669,763,764,861),]#take only stations with no NA

select_stations=read.xlsx(paste0(path_data,"raw/SIM2/selection_bassins_SIM2.xlsx"))# Sample of watersheds with a diversity of characteristic for plotting
idx=which(sim_stations$Num_ordre_Modcou%in%select_stations$Numero_Modcou)
select_stations=select_stations[order(select_stations$Numero_Modcou),]#because idx is reordered
select_stations$idx=idx
select_stations=select_stations[select_stations$idx==725,]

names_eff=colnames(simu_lst)[2:4]
lst_names_eff=vector(mode="list",length=length(names_eff))#list of effects and their possibilities
for(i in 1:length(lst_names_eff)){
  names(lst_names_eff)[i]=names_eff[i]
  lst_names_eff[[i]]=unique(simu_lst[,names_eff[i]])
}

##########################################################################
## Raw and spline times series


SPAR=1.1
i=1 # Qmean_year
clim_resp=vector(length=nrow(simu_lst),mode="list")
clim_resp_spline=vector(length=nrow(simu_lst),mode="list")

##Merge data frame warnings are okay
for(c in 1:nrow(simu_lst)){# for each chain
  
  load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
  res=res[,c(1,select_stations$idx+1)]
  
  #res=res[res$year>=first_full_year&res$year<=last_full_year,]
  zz = !is.na(res[,2])
  Years=res[zz,1]
  #spline
  res_spline=res
  for(j in 2:ncol(res)){
    res_spline[zz,j]=smooth.spline(x=Years,y=res[zz,j],spar = SPAR)$y
  }
  colnames(res)[1]="year"
  colnames(res_spline)[1]="year"
  clim_resp[[c]]=res
  clim_resp_spline[[c]]=res_spline
}

w=2
plt_raw_time=vector(length=length(lst_names_eff$rcp),mode="list")
plt_spline_time=vector(length=length(lst_names_eff$rcp),mode="list")
for (r in lst_names_eff$rcp){
  
  chain_r=which(simu_lst$rcp==r)#chains with the right rcp
  raw=clim_resp[[chain_r[1]]][,c(1,w)]#first iteration outside loop
  spline=clim_resp_spline[[chain_r[1]]][,c(1,w)]
  for (R in 2:length(chain_r)){
    raw=merge(raw,clim_resp[[chain_r[R]]][,c(1,w)],by="year",all=T)
    spline=merge(spline,clim_resp_spline[[chain_r[R]]][,c(1,w)],by="year",all=T)
    ## Warnings okay
  }
  colnames(raw)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
  colnames(spline)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
  raw=pivot_longer(data=raw,cols=!year,names_to = "model",values_to = "val")
  raw$type="raw"
  spline=pivot_longer(data=spline,cols=!year,names_to = "model",values_to = "val")
  spline$type="spline"
  data=rbind(raw,spline)
  data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
  data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
  
  plt_raw_time[[r]]=ggplot(data[data$type=="raw",])+#Warnings okay
    geom_line(aes(x=year,y=val,color=rcm),size=1)+
    scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+#we keep that because we need 8 colors
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    #ggtitle(paste0("Chronique du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
    scale_x_continuous("")+
    scale_y_continuous(paste0("[",units[i],"]"))+
    guides(color = guide_legend(override.aes = list(size = 1.7)))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))
  plt_spline_time[[r]]=ggplot(data)+#Warnings okay
    geom_line(aes(x=year,y=val,size=type,color=rcm))+
    scale_size_manual("",values=c(0.7,1.7),label=c('Module "brut"',"Réponse climatique"))+
    scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
    theme_bw(base_size = 18)+
    theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
    theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
    #ggtitle(paste0("Chronique du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
    scale_x_continuous("")+
    scale_y_continuous(paste0("[",units[i],"]"))+
    guides(color = guide_legend(override.aes = list(size = 1.7)))+
    facet_wrap(vars(gcm))+
    theme(panel.spacing.x = unit(2, "lines"))
}


#########################################################
## Relative spline time series

##Merge data frame warnings are okay
SPAR=1.1
i=1
  
all_chains=vector(length=nrow(simu_lst),mode="list")
for (j in 1:nrow(simu_lst)){
  load(paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[j],"_",simu_lst$gcm[j],"_",simu_lst$rcm[j],"_",simu_lst$bc[j],"_",simu_lst$hm[j],".Rdata"))
  res=res[,c(1,select_stations$idx+1)]
  #res=res[res$year>=first_full_year&res$year<=last_full_year,]
  all_chains[[j]]=res
}

n_bv=ncol(all_chains[[1]])-1
w=2
  
ClimateProjections=lapply(all_chains, function(x) x[,c(1,w)])
#ClimateProjections=lapply(ClimateProjections,function(x) x[x$year>=first_full_year & x$year<=last_full_year,][,2])
Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections)[,-1])
#Y=t(do.call(cbind,ClimateProjections))
nS=nrow(simu_lst)
X=seq(first_data_year,last_data_year)
clim_resp=prepare_clim_resp(Y=Y,X=seq(first_data_year,last_data_year),Xref = centr_ref_year,Xfut = seq(first_data_year,last_data_year),typeChangeVariable = "rel",spar = rep(SPAR,nrow(simu_lst)),type = "spline")
raw=data.frame(t(clim_resp$phiStar+clim_resp$etaStar))*100
colnames(raw)=paste0(simu_lst$rcp,"_",simu_lst$gcm,"_",simu_lst$rcm)
raw[is.na(t(Y))]=NA
raw$year=X
raw=pivot_longer(data=raw,cols=!year,names_to = "model",values_to = "val")
raw$type="raw"
spline=data.frame(t(clim_resp$phiStar))*100
colnames(spline)=paste0(simu_lst$rcp,"_",simu_lst$gcm,"_",simu_lst$rcm)
spline[is.na(t(Y))]=NA
spline$year=X
spline=pivot_longer(data=spline,cols=!year,names_to = "model",values_to = "val")
spline$type="spline"
data=rbind(raw,spline)
data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[3]))

r="rcp8.5"
plt_spline_time_rel=ggplot(data[data$rcp==r,])+#Warnings okay
  geom_line(aes(x=year,y=val,size=type,color=rcm))+
  #geom_line(aes(x=year,y=ciinf,color=rcm))+
  #geom_line(aes(x=year,y=cisup,color=rcm))+
  scale_size_manual("",values=c(0.7,1.7),label=c('Module "brut"',"Réponse en\n changement climatique"))+
  scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  #ggtitle(paste0("Chronique du changement relatif du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
  scale_x_continuous("")+
  scale_y_continuous(paste0("[%]"))+
  guides(color = guide_legend(override.aes = list(size = 1.7)))+
  facet_wrap(vars(gcm))+
  theme(panel.spacing.x = unit(2, "lines"))


##################################################################################
## Mean change and variance partition per RCP


i=1
load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_time_lm.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
pred_name="temps"
pred_unit=""
xlim=c(1990,2100)
b=1
idx=select_stations$idx[b]

plt_var_rcp8.5=plotQUALYPSOTotalVarianceByScenario_noIV_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff = "rcp",nameScenario = "rcp8.5",plain_name_Scen = "RCP 8.5",pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim,iv_type = "tot")
plt_var_rcp8.5=plt_var_rcp8.5+
  labs(title=NULL,)+
  scale_y_continuous("[%]")+
  theme(axis.title.x = element_blank())+
  guides(fill=guide_legend(title="Incertitude de la\nréponse climatique"))+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(a))", vjust=1, hjust=-2,parse=T,size=10)

plt_var_rcp8.5_bis=plotQUALYPSOTotalVarianceByScenario_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff = "rcp",nameScenario = "rcp8.5",plain_name_Scen = "RCP 8.5",pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
plt_var_rcp8.5_bis=plt_var_rcp8.5_bis+
  labs(title=NULL)+
  scale_y_continuous("[%]")+
  theme(axis.title.x = element_blank())+
  guides(fill=guide_legend(title="Incertitude totale"))+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(b))", vjust=1, hjust=-2,parse=T,size=10)

#####################################
## Effet GCM, effet RCM

plt_gcm_effect=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
plt_gcm_effect=plt_gcm_effect+
  labs(title=NULL)+
  scale_y_continuous("[%]")+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(a))", vjust=1, hjust=-2,parse=T,size=10)

plt_rcm_effect=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
plt_rcm_effect=plt_rcm_effect+
  labs(title=NULL)+
  scale_y_continuous("[%]")+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(b))", vjust=1, hjust=-2,parse=T,size=10)

####################################
## Effet BC, effet HM

data_bc=data.frame(year=seq(1990,2098))
data_bc$bc1=(data_bc$year-1990)^1.5*0.01
data_bc$bc2=-data_bc$bc1
data_bc=pivot_longer(data=data_bc,cols=!year,names_to = "bc",values_to = "val")

plt_bc_effect=ggplot(data_bc)+
  geom_line(aes(x=year,y=val,group=bc,color=bc),size=1)+
  scale_x_continuous("",limits=xlim)+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = ipcc_6col[1:2],labels=c("Correction A","Correction B"))+
  scale_y_continuous(paste0("[%]"))+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(c))", vjust=1, hjust=-2,parse=T,size=10)


data_hm=data.frame(year=seq(1990,2098))
data_hm$hm1=(data_hm$year-1990)^0.5*2
data_hm$hm2=-data_hm$hm1
data_hm=pivot_longer(data=data_hm,cols=!year,names_to = "hm",values_to = "val")

plt_hm_effect=ggplot(data_hm)+
  geom_line(aes(x=year,y=val,group=hm,color=hm),size=1)+
  scale_x_continuous("",limits=xlim)+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = ipcc_6col[1:2],labels=c("Modèle A","Modèle B"))+
  scale_y_continuous(paste0("[%]"))+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(d))", vjust=1, hjust=-2,parse=T,size=10)


#######################################
## Changements RCM, GCM, BC, HM

plt_gcm_change=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="gcm",plain_nameEff = "GCM",pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim,includeMean = T)
plt_gcm_change=plt_gcm_change+
  labs(title=NULL)+
  scale_y_continuous("[%]")+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(a))", vjust=1, hjust=-2,parse=T,size=10)

plt_rcm_change=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcm",plain_nameEff = "RCM",pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim,includeMean = T)
plt_rcm_change=plt_rcm_change+
  labs(title=NULL)+
  scale_y_continuous("[%]")+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(b))", vjust=1, hjust=-2,parse=T,size=10)

grand_mean=rep(lst.QUALYPSOOUT[[idx]]$GRANDMEAN$MEAN,2)
grand_mean=grand_mean[which(lst.QUALYPSOOUT[[idx]]$Xfut>=1990&lst.QUALYPSOOUT[[idx]]$Xfut<=last_full_year)]
data_bc$val_ch=data_bc$val+rep(grand_mean,2)*100
data_hm$val_ch=data_hm$val+rep(grand_mean,2)*100

plt_bc_change=ggplot(data_bc)+
  geom_line(aes(x=year,y=val_ch,group=bc,color=bc),size=1)+
  scale_x_continuous("",limits=xlim)+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = ipcc_6col[1:2],labels=c("Correction A","Correction B"))+
  scale_y_continuous(paste0("[%]"))+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(c))", vjust=1, hjust=-2,parse=T,size=10)

plt_hm_change=ggplot(data_hm)+
  geom_line(aes(x=year,y=val_ch,group=hm,color=hm),size=1)+
  scale_x_continuous("",limits=xlim)+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = ipcc_6col[1:2],labels=c("Modèle A","Modèle B"))+
  scale_y_continuous(paste0("[%]"))+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(d))", vjust=1, hjust=-2,parse=T,size=10)

########################################
## Variance partition

plt_var_decomp=plotQUALYPSOTotalVarianceDecomposition_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
plt_var_decomp=plt_var_decomp+
  labs(title=NULL)+
  scale_y_continuous("[%]")+
  theme(legend.title=element_blank())+
  theme(axis.title.x = element_blank())

############################################
## Change by RCP + CI in time

plt_change_rcp_time=plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],nameEff="rcp",plain_nameEff = "RCP",pred="time",pred_name = pred_name,ind_name = lst_indic[i],ind_name_full=name_indic[i],bv_name = select_stations$Nom[b],bv_full_name = select_stations$Nom_complet[b],pred_unit = pred_unit,folder_out=NA,includeMean = T,xlim=xlim,incert=T)
plt_change_rcp_time=plt_change_rcp_time+
  labs(title=NULL)+
  scale_y_continuous("[%]")+
  theme(legend.title=element_blank())+
  theme(axis.title.x = element_blank())


###############################################
## Maps

map_iv=map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = 2085,pred_name = pred_name,pred = "time",pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = NA)
map_iv=map_iv+
  labs(title=NULL)+
  guides(fill=guide_colorbar(title="[%]",barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
map_quant_horiz=map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = c(2030,2050,2085),pred_name = pred_name,pred = "time",pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],rcp_name = "rcp8.5",rcp_plainname="RCP 8.5",folder_out = NA)
map_quant_horiz=map_quant_horiz+
  labs(title=NULL)+
  guides(fill=guide_colorbar(title="[%]",barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
map_rcmeff=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = 2050,name_eff = "rcm",name_eff_plain = "RCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = NA)
map_rcmeff=map_rcmeff+
  labs(title=NULL)+
  guides(fill=guide_colorbar(title="[%]",barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
map_gcmeff=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = 2050,name_eff = "gcm",name_eff_plain = "GCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = NA)
map_gcmeff=map_gcmeff+
  labs(title=NULL)+
  guides(fill=guide_colorbar(title="[%]",barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
map_rcmchang=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = 2050,name_eff = "rcm",name_eff_plain = "RCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = NA)
map_rcmchang=map_rcmchang+
  labs(title=NULL)+
  guides(fill=guide_colorbar(title="[%]",barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
map_gcmchang=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeMean=T,horiz = 2050,name_eff = "gcm",name_eff_plain = "GCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = lst_indic[i],ind_name_full=name_indic[i],folder_out = NA)
map_gcmchang=map_gcmchang+
  labs(title=NULL)+
  guides(fill=guide_colorbar(title="[%]",barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))

##########
#MARKDOWN#
##########

rmarkdown::render(path_Rmd,output_format = "word_document2",output_options = list(reference_docx="C:/Users/reverdya/Documents/Docs/1_code/explore/template_word.docx") ,output_file = "fiche_exemple_COUT_v0.1.docx",output_dir =paste0(path_fig,"COUT/"),quiet = TRUE)
