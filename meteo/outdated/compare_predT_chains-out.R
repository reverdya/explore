# Alix Reverdy
# Explore 2
# Compare chains out for predictor temperature at 1.5°C for 1, 2 and 3 RCPs

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

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse-indic/compare_predT/"
path_temp="C:/Users/reverdya/Documents/Docs/2_Data/raw/Global_temp/"
path_sig="C:/Users/reverdya/Documents/Docs/2_data/SIG/raw/French_cities/"

ref_year=1990# central year of 1976-2005 reference period

load(paste0(path_data,"simu_lst.Rdata"))

###########
#FUNCTIONS#
###########



######
#MAIN#
######

#######################################################################################
## Extract indexes of reference "cities"

ref_cities=read.xlsx(paste0(path_sig,"French_cities_coord.xlsx"))
ref_cities$col=ref_cities$row=ref_cities$xcoord
for (i in 1:nrow(ref_cities)){
  dist=sqrt((refs$lon-ref_cities$xcoord[i])^2+(refs$lat-ref_cities$ycoord[i])^2)
  min_dist=as.vector(which(dist==min(dist),arr.ind = T))
  ref_cities$row[i]=min_dist[1]
  ref_cities$col[i]=min_dist[2]
}
ref_cities$idx=(ref_cities$col-1)*nrow(refs$mask)+ref_cities$row
tmp=c(1:length(as.vector(refs$mask)))
tmp=tmp[as.logical(refs$mask)]
ref_cities$idx_masked=which(tmp %in% ref_cities$idx)

#######################################################################################
## Make figures


for(v in unique(simu_lst$var)[c(1,2)]){
  for (i in unique(simu_lst[simu_lst$var==v,]$indic)[c(5)]){
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_1rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_1rcp[[1]]$Xfut==1.5)
    chains_1rcp=reconstruct_chains(lst.QUALYPSOOUT_temp_1rcp,idx_space = idx_Xfut)
    rm(lst.QUALYPSOOUT_temp_1rcp)
    gc()
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_2rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_2rcp[[1]]$Xfut==1.5)
    chains_2rcp=reconstruct_chains(lst.QUALYPSOOUT_temp_2rcp,idx_space = idx_Xfut)
    rm(lst.QUALYPSOOUT_temp_2rcp)
    gc()
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_3rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_3rcp[[1]]$Xfut==1.5)
    chains_3rcp=reconstruct_chains(lst.QUALYPSOOUT_temp_3rcp,idx_space = idx_Xfut)
    rm(lst.QUALYPSOOUT_temp_3rcp)
    gc()
    
    ########################################
    ## Compare the 3 analyses for RCP8.5
    
    chains1=data.frame(t(chains_1rcp))
    chains2=data.frame(t(chains_2rcp[seq(2,nrow(chains_2rcp),2),]))
    chains3=data.frame(t(chains_3rcp[seq(3,nrow(chains_3rcp),3),]))
    chains1=pivot_longer(chains1,cols=everything(),names_to = "chain",values_to = "rcp1")
    chains2=pivot_longer(chains2,cols=everything(),names_to = "chain",values_to = "rcp2")
    chains3=pivot_longer(chains3,cols=everything(),names_to = "chain",values_to = "rcp3")
    
    data=data.frame(cbind(chains1$rcp1,chains2$rcp2,chains3$rcp3))
    colnames(data)=c("rcp1","rcp2","rcp3")
    binssize=c(0.025,0.025)
    if(v!="tasAdjust"){
      data=data*100
      binssize=c(0.25,0.25)
    }
    lims=c(min(data),max(data))
    plt1=ggplot(data)+
      geom_bin2d(aes(x=rcp3,y=rcp2),binwidth=binssize)+
      scale_fill_viridis_c("Nombre")+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("3 RCPs")+
      ylab("2 RCPs")
    plt2=ggplot(data)+
      geom_bin2d(aes(x=rcp3,y=rcp1),binwidth=binssize)+
      scale_fill_viridis_c("Nombre")+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("3 RCPs")+
      ylab("1 RCP")
    
    plt=ggarrange(plt1,plt2,common.legend = T,legend="right")
    if(v!="tasAdjust"){
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des analyses en prédicteur température\npour tous les pixels et chaînes RCP8.5 reconstituées\nà l'horizon 1.5°C (Changement relatif en % du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }else{
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des analyses en prédicteur température\npour tous les pixels et chaînes RCP8.5 reconstituées\nà l'horizon 1.5°C (Changement en °C du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }
    save.plot(plt,Filename = paste0("comp_runs_predT_all_",v,"_",i),Folder = path_fig,Format = "jpeg")
    
    
    
    ## Compare the 3 analyses for RCP8.5 a few cities
    
    for(c in 1:nrow(ref_cities)){
      idx=ref_cities$idx_masked[c]
      Nscen=length(unique(chains1$chain))
      start=(idx-1)*Nscen+1
      end=idx*Nscen
      data_c=data[start:end,]
      lims=c(min(data_c),max(data_c))
      
      plt=ggplot(data_c)+
        geom_point(aes(x=rcp3,y=rcp2,color="2 RCPs"))+
        geom_point(aes(x=rcp3,y=rcp1,color="1 RCP"))+
        coord_equal(xlim=lims,ylim=lims)+
        geom_abline(slope=1, intercept=0,color="red",size=1)+
        theme_bw(base_size = 18)+
        theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
        xlab("3 RCPs")+
        ylab("1 ou 2 RCPs")+
        scale_color_manual("analyse",values = c("2 RCPs"=ipcc_6col[2],"1 RCP"=ipcc_6col[6]))+
        theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))
        if(v!="tasAdjust"){
          plt=plt+
            ggtitle(paste0("Comparaison des analyses en prédicteur température\npour toutes les chaînes RCP8.5 reconstituées\nà l'horizon 1.5°C (Changement relatif en % du ",v,"_",i," )"))
        }else{
          plt=plt+
            ggtitle(paste0("Comparaison des analyses en prédicteur température\npour toutes les chaînes RCP8.5 reconstituées\nà l'horizon 1.5°C (Changement en °C du ",v,"_",i," )"))
        }
        save.plot(plt,Filename = paste0("comp_runs_predT_",ref_cities$name[c],"_",v,"_",i),Folder = path_fig,Format = "jpeg")
    }
    
    ############################################
    ## Compare intra analyse 3RCP between RCP
    
    data=data.frame(t(chains_3rcp))
    data=pivot_longer(data,cols=everything(),names_to="chains",values_to = "val")
    data$rcp=rep(rep(c("rcp26","rcp45","rcp85"),each=dim(chains_3rcp)[2]),times=dim(chains_3rcp)[1]/3)
    data$chains=rep(1:dim(chains_3rcp)[2],times=dim(chains_3rcp)[1])+rep(seq(from=0,length.out=dim(chains_3rcp)[1]/3,by=dim(chains_3rcp)[2]),each=dim(chains_3rcp)[2]*3)
    data=pivot_wider(data,names_from=rcp,values_from =val)
    data=data[,-1]
    
    binssize=c(0.025,0.025)
    if(v!="tasAdjust"){
      data=data*100
      binssize=c(0.25,0.25)
    }
    lims=c(min(data),max(data))
    plt1=ggplot(data)+
      geom_bin2d(aes(x=rcp85,y=rcp45),binwidth=binssize)+
      scale_fill_viridis_c("Nombre")+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("RCP 8.5")+
      ylab("RCP 4.5")
    plt2=ggplot(data)+
      geom_bin2d(aes(x=rcp85,y=rcp26),binwidth=binssize)+
      scale_fill_viridis_c("Nombre")+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("RCP 8.5")+
      ylab("RCP 2.6")
    
    plt=ggarrange(plt1,plt2,common.legend = T,legend="right")
    if(v!="tasAdjust"){
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des chaines reconstituées pour les 3 RCPs en prédicteur température\npour tous les pixels et l'analyse à 3 RCPs\nà l'horizon 1.5°C (Changement relatif en % du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }else{
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des chaines reconstituées pour les 3 RCPs en prédicteur température\npour tous les pixels et l'analyse à 3 RCPs\nà l'horizon 1.5°C (Changement en °C du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }
    save.plot(plt,Filename = paste0("comp_rcps_predT_all_",v,"_",i),Folder = path_fig,Format = "jpeg")
    
    
    
    ## Compare intra analyse 3RCP between RCP for a few cities
    
    for(c in 1:nrow(ref_cities)){
      idx=ref_cities$idx_masked[c]
      Nscen=length(unique(chains1$chain))
      npix=dim(chains_3rcp)[2]
      idx_seq=seq(idx,length.out=Nscen,by=npix)
      data_c=data[idx_seq,]
      lims=c(min(data_c),max(data_c))
      
      plt=ggplot(data_c)+
        geom_point(aes(x=rcp85,y=rcp45,color="RCP 4.5"))+
        geom_point(aes(x=rcp85,y=rcp26,color="RCP 2.6"))+
        coord_equal(xlim=lims,ylim=lims)+
        geom_abline(slope=1, intercept=0,color="red",size=1)+
        theme_bw(base_size = 18)+
        theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
        xlab("RCP 8.5")+
        ylab("RCP 2.6 ou 4.5")+
        scale_color_manual("",values = c("RCP 4.5"=col_3rcp[[2]],"RCP 2.6"=col_3rcp[[1]]))+
        theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))
      if(v!="tasAdjust"){
        plt=plt+
          ggtitle(paste0("Comparaison des chaines reconstituées pour les 3 RCPs en prédicteur température\npour tous les pixels et l'analyse à 3 RCPs\nà l'horizon 1.5°C (Changement relatif en % du ",v,"_",i," )"))
      }else{
        plt=plt+
          ggtitle(paste0("Comparaison des chaines reconstituées pour les 3 RCPs en prédicteur température\npour tous les pixels et l'analyse à 3 RCPs\nà l'horizon 1.5°C (Changement en °C du ",v,"_",i," )"))
      }
      save.plot(plt,Filename = paste0("comp_rcps_predT_",ref_cities$name[c],"_",v,"_",i),Folder = path_fig,Format = "jpeg")
    }
    
    ########################################
    ## Compare ensemble mean between analyses
    
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_1rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_1rcp[[1]]$Xfut==1.5)
    rcp1=lst.QUALYPSOOUT_temp_1rcp[[idx_Xfut]]$GRANDMEAN$MEAN
    rm(lst.QUALYPSOOUT_temp_1rcp)
    gc()
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_2rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_2rcp[[1]]$Xfut==1.5)
    rcp2=lst.QUALYPSOOUT_temp_2rcp[[idx_Xfut]]$GRANDMEAN$MEAN
    rm(lst.QUALYPSOOUT_temp_2rcp)
    gc()
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_3rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_3rcp[[1]]$Xfut==1.5)
    rcp3=lst.QUALYPSOOUT_temp_3rcp[[idx_Xfut]]$GRANDMEAN$MEAN
    rm(lst.QUALYPSOOUT_temp_3rcp)
    gc()
    
    data=data.frame(cbind(rcp1,rcp2,rcp3))
    if(v!="tasAdjust"){
      data=data*100
    }
    lims=c(min(data),max(data))
    plt1=ggplot(data)+
      geom_point(aes(x=rcp3,y=rcp2),alpha=0.2,shape=20)+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("3 RCPs")+
      ylab("2 RCPs")
    plt2=ggplot(data)+
      geom_point(aes(x=rcp3,y=rcp1),alpha=0.2,shape=20)+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("3 RCPs")+
      ylab("1 RCP")
    
    plt=ggarrange(plt1,plt2,common.legend = T,legend="right")
    if(v!="tasAdjust"){
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des analyses en prédicteur température\npour tous les pixels et moyenne d'ensemble\nà l'horizon 1.5°C (Changement relatif en % du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }else{
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des analyses en prédicteur température\npour tous les pixels et moyenne d'ensemble\nà l'horizon 1.5°C (Changement en °C du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }
    save.plot(plt,Filename = paste0("comp_runs_mean_predT_",v,"_",i),Folder = path_fig,Format = "jpeg")
    
    #######################################################
    ## Compare ensemble means between RCPs for 3RCPs analyse
    
    load(file=paste0(path_data,"Qualypso/",v,"/",i,"/",v,"_",i,"_list_QUALYPSOOUT_temp_3rcp_allyears.RData"))
    idx_Xfut=which(lst.QUALYPSOOUT_temp_3rcp[[1]]$Xfut==1.5)
    data=lst.QUALYPSOOUT_temp_3rcp[[idx_Xfut]]$MAINEFFECT$rcp$MEAN+lst.QUALYPSOOUT_temp_3rcp[[idx_Xfut]]$GRANDMEAN$MEAN
    data=data.frame(data)
    colnames(data)=lst.QUALYPSOOUT_temp_3rcp[[idx_Xfut]]$listScenarioInput$listEff[[1]]
    rm(lst.QUALYPSOOUT_temp_3rcp)
    gc()

    if(v!="tasAdjust"){
      data=data*100
    }
    lims=c(min(data),max(data))
    plt1=ggplot(data)+
      geom_point(aes(x=rcp85,y=rcp45),alpha=0.2,shape=20)+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("RCP 8.5")+
      ylab("RCP 4.5")
    plt2=ggplot(data)+
      geom_point(aes(x=rcp85,y=rcp26),alpha=0.2,shape=20)+
      coord_equal(xlim=lims,ylim=lims)+
      geom_abline(slope=1, intercept=0,color="red",size=1)+
      theme_bw(base_size = 18)+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      xlab("RCP 8.5")+
      ylab("RCp 2.6")
    
    plt=ggarrange(plt1,plt2,common.legend = T,legend="right")
    if(v!="tasAdjust"){
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des RCPs de l'analyse à 3RCPs en prédicteur température\npour tous les pixels et moyenne d'ensemble\nà l'horizon 1.5°C (Changement relatif en % du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }else{
      plt=annotate_figure(plt, top = text_grob(paste0("Comparaison des RCPs de l'analyse à 3RCPs en prédicteur température\npour tous les pixels et moyenne d'ensemble\nà l'horizon 1.5°C (Changement en °C du ",v,"_",i," )"), face = "bold", size = 18,hjust=0.5))
    }
    save.plot(plt,Filename = paste0("comp_rcps_mean_predT_",v,"_",i),Folder = path_fig,Format = "jpeg")
      
  }
}
  
      