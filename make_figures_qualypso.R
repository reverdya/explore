# Alix Reverdy
# Explore 2
# Make figures from Qualypso runs

rm(list=ls())
gc()
dev.off()

#########
#LIBRARY#
#########

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R')

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/Qualypso/"

var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

lst_indic=c("Q_mean_year","Q_q95_year","VCN10","VCN10_day")
predict=c("time","temp")

select_gcm=c("CNRM-CM5-LR","EC-EARTH","IPSL-CM5A-MR")

labels_rcp=c("RCP 2.6","RCP 4.5","RCP 8.5")#check coherence of order with Qualypsoout, same for color scale. Used inside plot functions
Unit="%" #so far all changes are relative

######
#MAIN#
######

sim_stations=read.csv(file = paste0(path_data,"raw/SIM2/Infos_stations_modcou_OK.csv"),sep=";")
sim_stations=sim_stations[!sim_stations$Num_ordre_Modcou %in% c(626,669,763,764,861),]#take only stations with no NA

select_stations=read.xlsx(paste0(path_data,"raw/SIM2/selection_bassins_SIM2.xlsx"))# Sample of watersheds with a diversity of characteristic for plotting
idx=which(sim_stations$Num_ordre_Modcou%in%select_stations$Numero_Modcou)
select_stations=select_stations[order(select_stations$Numero_Modcou),]#because idx is reordered
select_stations$idx=idx

##############################################
## Times series Qualypso for selected basins


for (i in 1:length(lst_indic)){
  folder_out=paste0(path_fig,"3GCM_all_basins/",lst_indic[i],"/")
  dir.create(folder_out)
  for(p in 1:length(predict)){
    load(file=paste0("C:/Users/reverdya/Documents/Docs/2_data/processed/qualypso/",lst_indic[i],"_list_QUALYPSOOUT_3GCM_",predict[p],".RData"))
    if(predict[p]=="time"){
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_time
      pred_name="temps"
      pred_unit=""
    }else{
      lst.QUALYPSOOUT=lst.QUALYPSOOUT_temp
      pred_name="deg C"
    }
    for(b in 1:nrow(select_stations)){
      idx=select_stations$idx[b]
      #ieff_x and  name_Scen should match what's in QUALYSPOOUT
      ieff_rcp=which(colnames(lst.QUALYPSOOUT[[idx]]$listScenarioInput$scenAvail)=="rcp")
      ieff_gcm=which(colnames(lst.QUALYPSOOUT[[idx]]$listScenarioInput$scenAvail)=="gcm")
      ieff_rcm=which(colnames(lst.QUALYPSOOUT[[idx]]$listScenarioInput$scenAvail)=="rcm")
      
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],iEff = ieff_rcp,eff_name = "RCP",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],ind_unit = Unit,pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],iEff = ieff_rcp,eff_name = "RCP",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],ind_unit = Unit,pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],iEff = ieff_gcm,eff_name = "GCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],ind_unit = Unit,pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],iEff = ieff_gcm,eff_name = "GCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],ind_unit = Unit,pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],iEff = ieff_rcm,eff_name = "RCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],ind_unit = Unit,pred_unit = pred_unit,folder_out=folder_out)
      plotQUALYPSOeffect_ggplot(QUALYPSOOUT = lst.QUALYPSOOUT[[idx]],iEff = ieff_rcm,eff_name = "RCM",pred_name = pred_name,ind_name = lst_indic[i],bv_name = select_stations$Nom[b],ind_unit = Unit,pred_unit = pred_unit,folder_out=folder_out,includeMean = T)
    }
  }
}















# plot grand mean
save.plot(plot.object=NULL,
          Folder=path_fig,
          Filename=paste0("QUALYPSOgrandmean_Garonne_Q_year_mean"),
          Type="plot",
          plot.function=plotQUALYPSOgrandmean(lst.QUALYPSOOUT[[test_bv]],main=paste0("Q_year_mean_Garonne"),CIlevel = c(0.05,0.95)),
          Format = "jpeg")
# plot main GCM effects
save.plot(plot.object=NULL,
          Folder=path_fig,
          Filename=paste0("QUALYPSOeffectGCM_Garonne_Q_year_mean"),
          Type="plot",
          plot.function=plotQUALYPSOeffect(lst.QUALYPSOOUT[[test_bv]],iEff=ieff_gcm,main=paste0("Q_year_mean_Garonne"),CIlevel = c(0.05,0.95)),
          Format = "jpeg")
# plot main RCM effects
save.plot(plot.object=NULL,
          Folder=path_fig,
          Filename=paste0("QUALYPSOeffectRCM_Garonne_Q_year_mean"),
          Type="plot",
          plot.function=plotQUALYPSOeffect(lst.QUALYPSOOUT[[test_bv]],iEff=ieff_rcm,main=paste0("Q_year_mean_Garonne"),CIlevel = c(0.05,0.95)),
          Format = "jpeg")
# plot main RCP effects
save.plot(plot.object=NULL,
          Folder=path_fig,
          Filename=paste0("QUALYPSOeffectRCP_Garonne_Q_year_mean"),
          Type="plot",
          plot.function=plotQUALYPSOeffect(lst.QUALYPSOOUT[[test_bv]],iEff=ieff_rcp,main=paste0("Q_year_mean_Garonne"),CIlevel = c(0.05,0.95)),
          Format = "jpeg")
# plot fraction of total variance for the differences sources of uncertainty
save.plot(plot.object=NULL,
          Folder=path_fig,
          Filename=paste0("QUALYPSOTotalVarianceDecomposition_Garonne_Q_year_mean"),
          Type="plot",
          plot.function=plotQUALYPSOTotalVarianceDecomposition(lst.QUALYPSOOUT[[test_bv]],main=paste0("Q_year_mean_Garonne")),
          Format = "jpeg")
# plot mean prediction and total variance with the differences sources of uncertainty
# for one scenario (e.g. a RCP scenario)
save.plot(plot.object=NULL,
          Folder=path_fig,
          Filename=paste0("QUALYPSOTotalVarianceByScenario_RCP8.5_Garonne_Q_year_mean"),
          Type="plot",
          plotQUALYPSOTotalVarianceByScenario(lst.QUALYPSOOUT[[test_bv]],iEff=ieff_rcp,nameScenario=name_Scen_rcp85,main=paste0("Q_year_mean_Garonne_RCP8.5")),
          Format = "jpeg")


## Change by RCP

data_quant=lst.QUALYPSOOUT[[test_bv]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]]
dates=lst.QUALYPSOOUT[[test_bv]]$vecYearsANOVA
data_q5=as.vector(unlist(apply(data_quant[,c("5%"),],2,function(x) smooth.spline(dates,x)$y)))*100
data_q95=as.vector(unlist(apply(data_quant[,c("95%"),],2,function(x) smooth.spline(dates,x)$y)))*100
data_mean=as.vector(unlist(apply(lst.QUALYPSOOUT[[test_bv]]$POINT$CHANGEBYEFFECT[[ieff_rcp]],2,function(x) smooth.spline(dates,x)$y)))*100

labs=lst.QUALYPSOOUT[[test_bv]]$listScenarioInput$listEff[[ieff_rcp]]

data=data.frame(date=rep(dates,times=3),q5=data_q5,mean=data_mean,q95=data_q95,rcp=rep(labs,each=nrow(data_quant)))


palette=c("#0000FF","#79BCFF","#FF0000")
plt=ggplot(data=data)+
  geom_ribbon(aes(x=date,ymin=q5,ymax=q95,fill=rcp),alpha=0.5)+
  geom_line(aes(x=date,y=mean,group=rcp,color=rcp),size=1)+
  scale_color_discrete("",type = palette,labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  scale_fill_discrete("",type=palette,labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  scale_x_continuous("")+
  scale_y_continuous("Changement relatif (%)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  ggtitle(paste("Changement relatif du module annuel de la Garonne ? Tonneins et son incertitude\npour diff?rents RCP et 6 couples GCM/RCM (r?f?rence 1976-2005)"))
save.plot(plt,Filename = paste0("3GCM_RCP_incert_Qmean_year_Garonne"),Folder = path_fig,Format = "jpeg")


##########################################
## Change as maps of 3 RCP by 3 quantiles

data(wrld_simpl)
wrld <- fortify(wrld_simpl)

rcp_names=c("rcp2.6","rcp4.5","rcp8.5")
quant=c("5%","mean","95%")

path_shp=paste0(path_data,"SIM2/Infos_stations_modcou_OK.csv")
exut=read.csv(path_shp,sep=";")
exut=exut[,c("Num_ordre_Modcou","Lat","Lon")]
exut$idx=seq(1:nrow(exut))


tmp=exut
for (i in 1:8){
  exut=rbind(exut,tmp)
}
exut$rcp=rep(rcp_names,each=nrow(exut)/3)
exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
exut$val=0


for (i in 1:length(lst.QUALYPSOOUT)){
  for(r in rcp_names){
    idx_fut_year=which(lst.QUALYPSOOUT[[i]]$vecYearsANOVA==fut_year)
    ieff_rcp=which(colnames(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail)=="rcp")
    ieff_this_rcp=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]]==r)
    chg_mean=lst.QUALYPSOOUT[[i]]$POINT$CHANGEBYEFFECT[[ieff_rcp]][idx_fut_year,ieff_this_rcp]*100 #*100 for percentages
    exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[2]]=chg_mean
    chg_q5=lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_fut_year,,ieff_this_rcp][names(lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_fut_year,,ieff_this_rcp])==quant[1]]*100
    exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[1]]=chg_q5
    chg_q95=lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_fut_year,,ieff_this_rcp][names(lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_fut_year,,ieff_this_rcp])==quant[3]]*100
    exut$val[exut$idx==i & exut$rcp==r & exut$quant==quant[3]]=chg_q95
  }
}


exut$quant=factor(exut$quant,levels=quant)
rcp.labs <- c("RCP 2.6", "RCP 4.5", "RCP 8.5")
names(rcp.labs) <- rcp_names
quant.labs <- c("Q5", "Module", "Q95")
names(quant.labs) <- quant
plt=ggplot() +
  geom_polygon(data=wrld,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=1)+
  geom_point(data = exut, aes(x = Lon, y = Lat,fill=val),color="black",size=2,alpha=1,shape=21)+
  scale_fill_gradientn("Changement relatif (%)",colours = warmcool(100),limits=c(-60,60),breaks=seq(-60,60,20),oob=squish,labels=c("< -60",seq(-40,40,20),"> 60"))+
  coord_equal(ratio=111/78,xlim = c(-6, 9),ylim = c(41.5,52),expand=F)+## ratio of 1?lat by 1?long at 45?N
  scale_x_continuous("")+
  scale_y_continuous("")+
  theme_bw(base_size = 10)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  ggtitle(paste("Changement relatif du module annuel et son incertitude\npour diff?rents RCP et 6 couples GCM/RCM (2069-2098 VS 1976-2005)"))+
  theme(axis.ticks =element_blank(),axis.text = element_blank() )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(rcp ~ quant,labeller = labeller(rcp = rcp.labs, quant = quant.labs))+
  theme(strip.text = element_text(size = 12, face = "bold"))+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
save.plot(plt,Filename = paste0("3GCM_RCP_incert_Qmean_year"),Folder = path_fig,Format = "jpeg")


###########################################################
## Change as maps of 3 horizons by 3 quantiles for RCP 8.5

horiz=c(2030,2050,2083)
quant=c("5%","mean","95%")

path_shp=paste0(path_data,"SIM2/Infos_stations_modcou_OK.csv")
exut=read.csv(path_shp,sep=";")
exut=exut[,c("Num_ordre_Modcou","Lat","Lon")]
exut$idx=seq(1:nrow(exut))


tmp=exut
for (i in 1:8){
  exut=rbind(exut,tmp)
}
exut$horiz=rep(horiz,each=nrow(exut)/3)
exut$quant=rep(rep(quant,each=nrow(exut)/9),times=3)
exut$val=0


for (i in 1:length(lst.QUALYPSOOUT)){
  for(h in horiz){
    idx_horiz=which(lst.QUALYPSOOUT[[i]]$vecYearsANOVA==h)
    ieff_rcp=which(colnames(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail)=="rcp")
    ieff_rcp85=which(lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcp]]=="rcp8.5")
    chg_mean=lst.QUALYPSOOUT[[i]]$POINT$CHANGEBYEFFECT[[ieff_rcp]][idx_horiz,ieff_rcp85]*100 #*100 for percentages
    exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[2]]=chg_mean
    chg_q5=lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_horiz,,ieff_rcp85][names(lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_horiz,,ieff_rcp85])==quant[1]]*100
    exut$val[exut$idx==i & exut$horiz==h& exut$quant==quant[1]]=chg_q5
    chg_q95=lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_horiz,,ieff_rcp85][names(lst.QUALYPSOOUT[[i]]$BAYES$CHANGEBYEFFECT[[ieff_rcp]][idx_horiz,,ieff_rcp85])==quant[3]]*100
    exut$val[exut$idx==i & exut$horiz==h & exut$quant==quant[3]]=chg_q95
  }
}


exut$quant=factor(exut$quant,levels=quant)
horiz.labs <- horiz
names(horiz.labs) <- horiz
quant.labs <- c("Q5", "Module", "Q95")
names(quant.labs) <- quant
plt=ggplot() +
  geom_polygon(data=wrld,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=1)+
  geom_point(data = exut, aes(x = Lon, y = Lat,fill=val),color="black",size=2,alpha=1,shape=21)+
  scale_fill_gradientn("Changement relatif (%)",colours = warmcool(100),limits=c(-60,60),breaks=seq(-60,60,20),oob=squish,labels=c("< -60",seq(-40,40,20),"> 60"))+
  coord_equal(ratio=111/78,xlim = c(-6, 9),ylim = c(41.5,52),expand=F)+## ratio of 1?lat by 1?long at 45?N
  scale_x_continuous("")+
  scale_y_continuous("")+
  theme_bw(base_size = 10)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  ggtitle(paste("Changement relatif du module annuel et son incertitude\npour diff?rents horizons(30 ans & ann?e centrale)\nRCP 8.5 et 6 couples GCM/RCM (r?f?rence 1976-2005)"))+
  theme(axis.ticks =element_blank(),axis.text = element_blank() )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(horiz ~ quant,labeller = labeller(horiz=horiz.labs, quant = quant.labs))+
  theme(strip.text = element_text(size = 12, face = "bold"))+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
save.plot(plt,Filename = paste0("3GCM_RCP8.5_horizons_incert_Qmean_year"),Folder = path_fig,Format = "jpeg")


###############
## Effets GCM

path_shp=paste0(path_data,"SIM2/Infos_stations_modcou_OK.csv")
exut=read.csv(path_shp,sep=";")
exut=exut[,c("Num_ordre_Modcou","Lat","Lon")]
exut$idx=seq(1:nrow(exut))

gcms=c(1,2,3)

tmp=exut
for (i in 1:(length(gcms)-1)){
  exut=rbind(exut,tmp)
}
exut$gcm=rep(gcms,each=nrow(exut)/length(gcms))
exut$val=0


for (i in 1:length(lst.QUALYPSOOUT)){
  idx_fut_year=which(lst.QUALYPSOOUT[[i]]$vecYearsANOVA==fut_year)
  ieff_gcm=which(colnames(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail)=="gcm")
  chg=lst.QUALYPSOOUT[[i]]$POINT$MAINEFFECT[[ieff_gcm]][idx_fut_year,]*100 #*100 for percentages
  for(j in 1:length(chg)){
    exut$val[exut$idx==i & exut$gcm==j]=chg[j]
  }
}


gcms.labs <- lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_gcm]]
names(gcms.labs) <- gcms
plt=ggplot() +
  geom_polygon(data=wrld,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=1)+
  geom_point(data = exut, aes(x = Lon, y = Lat,fill=val),color="black",size=2,alpha=1,shape=21)+
  scale_fill_gradientn("Effet (%)",colours = warmcool(100),limits=c(-20,20),breaks=seq(-20,20,5),oob=squish,labels=c("< -20",seq(-15,15,5),"> 20"))+
  coord_equal(ratio=111/78,xlim = c(-6, 9),ylim = c(41.5,52),expand=F)+## ratio of 1?lat by 1?long at 45?N
  scale_x_continuous("")+
  scale_y_continuous("")+
  theme_bw(base_size = 10)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  ggtitle(paste("Effet GCM pour le module annuel\n6 couples GCM/RCM (2069-2098 VS 1976-2005)"))+
  theme(axis.ticks =element_blank(),axis.text = element_blank() )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(cols=vars(gcm),labeller = labeller(gcm=gcms.labs))+
  theme(strip.text = element_text(size = 12, face = "bold"))+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
save.plot(plt,Filename = paste0("gcm_effect_Qmean_year"),Folder = path_fig,Format = "jpeg")

###############
## Effets RCM

path_shp=paste0(path_data,"SIM2/Infos_stations_modcou_OK.csv")
exut=read.csv(path_shp,sep=";")
exut=exut[,c("Num_ordre_Modcou","Lat","Lon")]
exut$idx=seq(1:nrow(exut))

rcms=c(1,2,3,4)

tmp=exut
for (i in 1:(length(rcms)-1)){
  exut=rbind(exut,tmp)
}
exut$rcm=rep(rcms,each=nrow(exut)/length(rcms))
exut$val=0


for (i in 1:length(lst.QUALYPSOOUT)){
  idx_fut_year=which(lst.QUALYPSOOUT[[i]]$vecYearsANOVA==fut_year)
  ieff_rcm=which(colnames(lst.QUALYPSOOUT[[i]]$listScenarioInput$scenAvail)=="rcm")
  chg=lst.QUALYPSOOUT[[i]]$POINT$MAINEFFECT[[ieff_rcm]][idx_fut_year,]*100 #*100 for percentages
  for(j in 1:length(chg)){
    exut$val[exut$idx==i & exut$rcm==j]=chg[j]
  }
}


rcms.labs <- lst.QUALYPSOOUT[[i]]$listScenarioInput$listEff[[ieff_rcm]]
names(rcms.labs) <- rcms
plt=ggplot() +
  geom_polygon(data=wrld,aes(x=long,y=lat,group=group),fill=NA,colour="black",size=1)+
  geom_point(data = exut, aes(x = Lon, y = Lat,fill=val),color="black",size=2,alpha=1,shape=21)+
  scale_fill_gradientn("Effet (%)",colours = warmcool(100),limits=c(-60,60),breaks=seq(-60,60,20),oob=squish,labels=c("< -60",seq(-40,40,20),"> 60"))+
  coord_equal(ratio=111/78,xlim = c(-6, 9),ylim = c(41.5,52),expand=F)+## ratio of 1?lat by 1?long at 45?N
  scale_x_continuous("")+
  scale_y_continuous("")+
  theme_bw(base_size = 10)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  ggtitle(paste("Effet RCM pour le module annuel\n6 couples GCM/RCM (2069-2098 VS 1976-2005)"))+
  theme(axis.ticks =element_blank(),axis.text = element_blank() )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(cols=vars(rcm),labeller = labeller(rcm=rcms.labs))+
  theme(strip.text = element_text(size = 12, face = "bold"))+
  guides(fill = guide_colourbar(barwidth = 2, barheight = 20,label.theme = element_text(size = 11, face = "bold"),title.theme=element_text(size = 14, face = "bold")))
save.plot(plt,Filename = paste0("rcm_effect_Qmean_year"),Folder = path_fig,Format = "jpeg")



