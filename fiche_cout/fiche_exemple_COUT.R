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
library(bookdown)# figure numbers
library(gridExtra)#multiplot

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/"
path_Rmd="C:/Users/reverdya/Documents/Docs/1_code/explore/fiche_cout/fiche_exemple_COUT.Rmd"
path_Rmd_faster="C:/Users/reverdya/Documents/Docs/1_code/explore/fiche_cout/fiche_exemple_COUT_faster.Rmd"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/French_cities/"
path_sig2="C:/Users/reverdya/Documents/Docs/2_data/SIG/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/processed/"
path_hadcrut="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"

load(file=paste0(path_data,"simu_lst.Rdata"))
load(file=paste0(path_data,"refs.Rdata"))

centr_ref_year=1990# central year of 1975-2005 reference period
typeChangeVar="rel"
first_ref_year=1975
last_ref_year=2005
first_data_year=1951
last_data_year=2099

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions

nbcores=detectCores()-2

######
#MAIN#
######

#########################################################################################
## Reference basins

basHy=read.csv(paste0(path_sig2,"processed/SAFRAN_ref_basHy.csv"))
colnames(basHy)=c("id","code","name")


names_eff=colnames(simu_lst)[4:7]
lst_names_eff=vector(mode="list",length=length(names_eff))#list of effects and their possibilities
for(i in 1:length(lst_names_eff)){
  names(lst_names_eff)[i]=names_eff[i]
  lst_names_eff[[i]]=unique(simu_lst[,names_eff[i]])
}

##########################################################################
## Raw and spline times series
SPAR=1.1
v=unique(simu_lst$var)[2]
i=unique(simu_lst[simu_lst$var==v,]$indic)[2]
bas=1#rhone-mediterannée-corse

clim_resp=vector(length=nrow(simu_lst),mode="list")
clim_resp_spline=vector(length=nrow(simu_lst),mode="list")
scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
all_chains=vector(length=nrow(scenAvail),mode="list")
for(c in 1:nrow(scenAvail)){# for each chain
  
  pth_tmp=list.files(paste0(path_data,"indic/",v,"/bas/"),full.names=T,pattern=glob2rx(paste0(v,"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c],"_")[[1]][1],"*",scenAvail$period[c],"*")))
  nc=load_nc(pth_tmp)
  res=ncvar_get(nc,varid=v)
  full_years=nc$dim$time$vals
  if(scenAvail$bc[c]=="ADAMONT"){
    full_years=year(as.Date(full_years,origin="1950-01-01"))
  }
  if(scenAvail$bc[c]=="CDFt"){
    full_years=year(as.Date(full_years,origin="1850-01-01"))
  }
  rm(nc)
  gc()
  res2=data.frame(matrix(nrow=length(full_years),ncol=2))
  res2[,1]=full_years
  res2[,2]=res[bas,]
  colnames(res2)[1]="year"
  all_chains[[c]]=res2
}


ClimateProjections=lapply(all_chains, function(x) x[,c(1,2)])
Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections))
Y=Y[,Y[1,]<=2100]
X=Y[1,]
Y=Y[-1,]
nS=nrow(scenAvail)
Xfut=seq(centr_ref_year,X[length(X)])
idx_ref0=which(X==centr_ref_year)
clim_resp=prepare_clim_resp(Y=Y,X=X,Xfut = Xfut,typeChangeVariable = "rel",spar = rep(SPAR,nrow(scenAvail)),type="spline",nbcores = nbcore)
raw_rel=data.frame(t(clim_resp$phiStar+clim_resp$etaStar[,idx_ref0:length(X)]))
colnames(raw_rel)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
raw_rel[is.na(t(Y[,idx_ref0:length(X)]))]=NA
raw_rel$year=Xfut
raw_rel=pivot_longer(data=raw_rel,cols=!year,names_to = "model",values_to = "val")
raw_rel$type="raw_rel"
raw_rel$val=raw_rel$val*100
spline_rel=data.frame(t(clim_resp$phiStar))
colnames(spline_rel)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
spline_rel[is.na(t(Y[,idx_ref0:length(X)]))]=NA
spline_rel$year=Xfut
spline_rel=pivot_longer(data=spline_rel,cols=!year,names_to = "model",values_to = "val")
spline_rel$type="spline_rel"
spline_rel$val=spline_rel$val*100
raw=data.frame(t(Y[,idx_ref0:length(X)]))
colnames(raw)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
raw[is.na(t(Y[,idx_ref0:length(X)]))]=NA
raw$year=Xfut
raw=pivot_longer(data=raw,cols=!year,names_to = "model",values_to = "val")
raw$type="raw"
spline=data.frame(t(clim_resp$phi))
colnames(spline)=paste0(scenAvail$rcp,"_",scenAvail$gcm,"_",scenAvail$rcm,"_",scenAvail$bc)
spline[is.na(t(Y[,idx_ref0:length(X)]))]=NA
spline$year=Xfut
spline=pivot_longer(data=spline,cols=!year,names_to = "model",values_to = "val")
spline$type="spline"


data=rbind(raw,spline,raw_rel,spline_rel)
data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[3]))
data$bc=unlist(lapply(strsplit(data$model,"_"),function(x) x[4]))


r="rcp85"

plt_raw_time=ggplot(data[data$rcp==r&data$type=="raw",])+#Warnings okay
  geom_line(aes(x=year,y=val,color=rcm),size=0.7)+
  #scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse climatique"))+
  scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
  #scale_linetype("")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  scale_x_continuous("")+
  scale_y_continuous(paste0("Précipitation estivales (mm)"))+
  guides(color = guide_legend(override.aes = list(size = 1.7)))+
  facet_grid(gcm~bc)+
  theme(panel.spacing.x = unit(0.5, "lines"))+
  theme(strip.text.y = element_text(size = 9))

# plt_spline_time=ggplot(data[data$rcp==r&(data$type=="raw"|data$type=="spline"),])+#Warnings okay
#   geom_line(aes(x=year,y=val,size=type,color=rcm))+
#   scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse climatique"))+
#   scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
#   #scale_linetype("")+
#   theme_bw(base_size = 18)+
#   theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
#   theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
#   scale_x_continuous("")+
#   scale_y_continuous(paste0("Réponse climatique (mm)"))+
#   guides(color = guide_legend(override.aes = list(size = 1.7)))+
#   facet_grid(gcm~bc)+
#   theme(panel.spacing.x = unit(0.5, "lines"))+
#   theme(strip.text.y = element_text(size = 9))

plt_spline_time_rel=ggplot(data[data$rcp==r&(data$type=="raw_rel"|data$type=="spline_rel"),])+#Warnings okay
  geom_line(aes(x=year,y=val,size=type,color=rcm))+
  scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse au\nchangement climatique"))+
  scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
  #scale_linetype("")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  scale_x_continuous("")+
  scale_y_continuous(paste0("Réponse au\nchangement climatique (%)"))+
  guides(color = guide_legend(override.aes = list(size = 1.7)))+
  facet_grid(gcm~bc)+
  theme(panel.spacing.x = unit(0.5, "lines"))+
  theme(strip.text.y = element_text(size = 9))

##################################################################################
## Mean change and variance partition per RCP

v="prtotAdjust"
i="seassum_JJA"
load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time_bas.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
pred_name="temps"
pred_unit=""
xlim=c(1990,2100)
b=1

idx=1#Rhone-mediterrannée -corse

plt_bilan=plotQUALYPSO_summary_change(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred = "time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,xlim=c(1990,2105),var="prtotAdjust",indic="seassum_JJA",idx_pix = idx,path_hadcrut=path_hadcrut,path_processed=path_temp,storyl=F,type="bas")


##################################################################################
## Boxplot per horizon and RCP


plt_bxplt=plotQUALYPSO_boxplot_horiz_rcp(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,pred = "time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,var="prtotAdjust",indic="yearsum",horiz = c(2030,2050,2085),title=F)

#####################################
## Effet GCM, effet RCM, effet BC

# plt_gcm_effect=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred="time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
# plt_gcm_effect=plt_gcm_effect+
#   labs(title=NULL)+
#   ylab("[%]")+
#   theme(axis.title.x = element_blank())+
#   annotate("text",  x=-Inf, y = Inf, label = "atop(bold(a))", vjust=1, hjust=-2,parse=T,size=10)
# 
# 
# plt_rcm_effect=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred="time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
# plt_rcm_effect=plt_rcm_effect+
#   labs(title=NULL)+
#   ylab("[%]")+
#   theme(axis.title.x = element_blank())+
#   annotate("text",  x=-Inf, y = Inf, label = "atop(bold(b))", vjust=1, hjust=-2,parse=T,size=10)+
#   scale_color_discrete("",type=plasma(4))
# 
# plt_bc_effect=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred="time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,xlim=xlim)
# plt_bc_effect=plt_bc_effect+
#   labs(title=NULL)+
#   ylab("[%]")+
#   theme(axis.title.x = element_blank())+
#   annotate("text",  x=-Inf, y = Inf, label = "atop(bold(c))", vjust=1, hjust=-2,parse=T,size=10)+
#   scale_color_discrete("",type=kovesi.rainbow(4)[c(1,4)])
# 

#######################################
## Changements RCM, GCM, BC, HM

plt_gcm_change=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="gcm",plain_nameEff = "GCM",pred="time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,xlim=xlim,includeRCP = "rcp85")
plt_gcm_change=plt_gcm_change+
  labs(title=NULL)+
  ylab("[%]")+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(a))", vjust=1, hjust=-2,parse=T,size=10)
  

plt_rcm_change=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="rcm",plain_nameEff = "RCM",pred="time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,xlim=xlim,includeRCP = "rcp85")
plt_rcm_change=plt_rcm_change+
  labs(title=NULL)+
  ylab("[%]")+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(b))", vjust=1, hjust=-2,parse=T,size=10)+
  scale_color_discrete("",type=plasma(4))

plt_bc_change=plotQUALYPSOeffect_ggplot(lst.QUALYPSOOUT = lst.QUALYPSOOUT,idx=idx,nameEff="bc",plain_nameEff = "BC",pred="time",pred_name = pred_name,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),bv_name = basHy$name[1],bv_full_name = basHy$name[1],pred_unit = pred_unit,folder_out=NA,xlim=xlim,includeRCP = "rcp85")
plt_bc_change=plt_bc_change+
  labs(title=NULL)+
  ylab("[%]")+
  theme(axis.title.x = element_blank())+
  annotate("text",  x=-Inf, y = Inf, label = "atop(bold(c))", vjust=1, hjust=-2,parse=T,size=10)+
  scale_color_discrete("",type=kovesi.rainbow(4)[c(1,4)])


###############################################
## Maps

load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_time.RData"))
lst.QUALYPSOOUT=lst.QUALYPSOOUT_time

mask_fr=as.vector(refs$mask)
mask_fr=mask_fr[mask_fr!=0]

path_river=paste0(path_sig2,"processed/CoursEau_idx1_wgs84.shp")
path_fr=paste0(path_sig2,"/raw/IGN/contours_FR/gadm36_FRA_0.shp")
background_for_maps(path_river,path_fr)

map_iv=map_one_var(lst.QUALYPSOOUT = lst.QUALYPSOOUT,vartype="varint",horiz = 2085,pred_name = pred_name,pred = "time",pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = NA,pix=T)
map_iv=map_iv+
  labs(title=NULL)+
  binned_scale(aesthetics = "fill",scale_name = "toto",name="Variabilité\ninterne (%)",ggplot2:::binned_pal(scales::manual_pal(ipcc_yelblue_5)),guide="coloursteps",limits=c(20,50),breaks=seq(20,50,length.out=6),oob=squish,show.limits = T,labels=c(paste0("< ",20),seq(26,44,6),paste0("> ",50)))+
  guides(fill=guide_colorbar(title="[%]",barwidth = 1, barheight = 10,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))

map_part=map_var_part(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = 2085,pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = NA,pix=T,title = F)
map_part=map_part+
  labs(title=NULL)+
  guides(fill=guide_colorbar(title="[%]",barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))

map_quant_horiz=map_3quant_1rcp_3horiz(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = c(2030,2050,2085),pred_name = pred_name,pred = "time",pred_unit = pred_unit,ind_name = paste0(v,"_",i),ind_name_full=paste0(v,"_",i),rcp_name = "rcp85",rcp_plainname="RCP 8.5",folder_out = NA,freq_col=0.99,pix=T,var=v)
map_quant_horiz=map_quant_horiz+
  labs(title=NULL)

# map_rcmeff=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = 2085,name_eff = "rcm",name_eff_plain = "RCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = NA,pix=T)
# map_rcmeff=map_rcmeff+
#   labs(title=NULL)+
#   guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
# 
# map_gcmeff=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,horiz = 2085,name_eff = "gcm",name_eff_plain = "GCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = NA,pix=T)
# map_gcmeff=map_gcmeff+
#   labs(title=NULL)+
#   guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))
# 
# map_rcmchang=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeRCP = "rcp85",horiz = 2085,name_eff = "rcm",name_eff_plain = "RCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = NA,pix=T)
# map_rcmchang=map_rcmchang+
#   labs(title=NULL)+
#   guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))

map_gcmchang=map_main_effect(lst.QUALYPSOOUT = lst.QUALYPSOOUT,includeRCP = "rcp85",horiz = 2085,name_eff = "gcm",name_eff_plain = "GCM",pred = "time",pred_name = pred_name,pred_unit = pred_unit,ind_name = paste0(v,"-",i),ind_name_full=paste0(v,"-",i),folder_out = NA,pix=T)
map_gcmchang=map_gcmchang+
  labs(title=NULL)+
  guides(fill=guide_colorbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = c("bold"),color=c("black")),title.theme=element_text(size = 14, face = "bold")))

##########
#MARKDOWN#
##########

rmarkdown::render(path_Rmd,output_format = "word_document2",output_options = list(reference_docx="C:/Users/reverdya/Documents/Docs/1_code/explore/fiche_cout/template_word.docx") ,output_file = "fiche_exemple_COUT.docx",output_dir =paste0(path_fig,"COUT/"),quiet = TRUE)
