# Alix Reverdy
# Explore 2
# check meteorological indicators: fit of splines

rm(list=ls())
gc()

#########
#LIBRARY#
#########

library(moments)#skewness
library(forecast)#tsoutliers
#tsoutliers fits trend on data (Friedman’s super smoother) and looking at values outside of Q75 + 3 * IQR (or Q25)
library(abind)#abind

########
#SOURCE#
########

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R',encoding = "utf-8")

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/processed/Explore2-meteo/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse-indic/"

Var=vector(mode="list")
Var[["tasAdjust"]]=c("seasmean","seasmean","seasmean","seasmean","yearmean")
Var[["prtotAdjust"]]=c("seassum","seassum","seassum","seassum","yearsum")
seas=c("DJF","MAM","JJA","SON","")
rcp=c("historical","rcp26","rcp45","rcp85")
bc=c("ADAMONT")

units=c("°C","mm")

centr_ref_year=1990# central year of 1975-2005 reference period
first_ref_year=1975
last_ref_year=2005
first_full_year=1972# from raw data filenames and DJF
last_full_year=2098# from raw data filenames and DJF
first_data_year=1950
last_data_year=2100


###########
#FUNCTIONS#
###########



######
#MAIN#
######


###################################################
## Make table of available simulations

simu_lst=vector(mode = "list",length=7)
names(simu_lst)=c("var","indic","seas","rcp","gcm","rcm","bc")
for (v in names(Var)){
  for(r in rcp[-1]){# -1 for not historical
    for(b in bc){
      s=1
      for(v2 in Var[[v]]){
        lst_f=list.files(paste0(path_data,"indic/",v,"/"),pattern=glob2rx(paste0(v,"*",r,"*",b,"*",v2,"*",seas[s],"*"))) #list all files of projections in folder
        gcm=sapply(strsplit(lst_f,"_"),"[",4)
        rcm=sapply(strsplit(lst_f,"_"),"[",5)
        n=length(gcm)
        simu_lst$var=c(simu_lst$var,rep(v,n))
        simu_lst$seas=c(simu_lst$seas,rep(seas[s],n))
        if(seas[s]==""){
          simu_lst$indic=c(simu_lst$indic,rep(paste0(v2),n))
        }else{
          simu_lst$indic=c(simu_lst$indic,rep(paste0(v2,"_",seas[s]),n))
        }
        simu_lst$rcp=c(simu_lst$rcp,rep(r,n))
        simu_lst$gcm=c(simu_lst$gcm,gcm)
        simu_lst$rcm=c(simu_lst$rcm,rcm)
        simu_lst$bc=c(simu_lst$bc,rep(b,n))
        s=s+1
      }
    }
  }
}
simu_lst=data.frame(simu_lst)
simu_lst[simu_lst$rcm=="REMO2009",]$rcm="REMO"# the 2 versions of REMO have been signaled as identical
simu_lst[simu_lst$rcm=="REMO2015",]$rcm="REMO"
save(simu_lst,file=paste0(path_data,"simu_lst.Rdata"))

###############################################################################################
## Plot raw indicator , and its spline and residual anomalies for all models and selection of watersheds by RCP for time
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
## Climate response not climate change response


##Merge data frame warnings are okay
for (SPAR in c(0.5,0.8,0.9,1.0,1.1,1.2,1.5)){
  for(v in unique(simu_lst$var)){
    dir.create(paste0(path_fig,v,"/"))
    for (i in unique(simu_lst[simu_lst$var==v,]$indic)){
      clim_resp=vector(length=nrow(simu_lst),mode="list")
      clim_resp_spline=vector(length=nrow(simu_lst),mode="list")
      dir.create(paste0(path_fig,v,"/",i))
      dir.create(paste0(path_fig,v,"/",i,"/plot_chains/"))
      scenAvail=simu_lst[simu_lst$var==v & simu_lst$indic==i,]
      for(c in 1:nrow(scenAvail)){# for each chain
        
        pth_tmp=list.files(paste0(path_data,"indic/",v,"/"),full.names=T,pattern=glob2rx(paste0(v,"*",scenAvail$rcp[c],"*",scenAvail$gcm[c],"*",scenAvail$rcm[c],"*",scenAvail$bc[c],"*",strsplit(scenAvail$indic[c],"_")[[1]][1],"*",scenAvail$seas[c],"*")))
        nc=load_nc(pth_tmp)
        res=ncvar_get(nc,varid=v)
        
        

        
        
        
        
        
        
        
        
        
        
        
        
          
          
          
          
        
        
        zz = !is.na(res[,2])
        vecYears=res[zz,1]
        res_spline=res
        for(j in 2:ncol(res)){
          if(lst_indic[i]=="VCN10"){
            res_spline[zz,j]=10^smooth.spline(x=vecYears,y=log10(res[zz,j]),spar = SPAR)$y
          }else{
            res_spline[zz,j]=smooth.spline(x=vecYears,y=res[zz,j],spar = SPAR)$y
          }
        }
        colnames(res)[1]="year"
        colnames(res_spline)[1]="year"
        clim_resp[[c]]=res
        clim_resp_spline[[c]]=res_spline
      }
      for (w in 2:ncol(clim_resp[[1]])){
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
          variab=raw
          variab$val=raw$val-spline$val
          variab$gcm=unlist(lapply(strsplit(variab$model,"_"),function(x) x[1]))
          variab$rcm=unlist(lapply(strsplit(variab$model,"_"),function(x) x[2]))
          tmp=pivot_wider(variab[c("year","model","val")],names_from = model,values_from = val)
          sdIV=sqrt(mean(apply(tmp[,-1],2,var,na.rm=T),na.rm=T))
          data=rbind(raw,spline)
          data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
          data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
          plt=ggplot(data)+#Warnings okay
            geom_line(aes(x=year,y=val,size=type,color=rcm))+
            scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse climatique"))+
            scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+# keep these colors because need 8
            theme_bw(base_size = 18)+
            theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
            theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
            ggtitle(paste0("Chronique du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
            scale_x_continuous("")+
            scale_y_continuous(paste0("Réponse climatique ( ",units[i]," )"))+
            guides(color = guide_legend(override.aes = list(size = 1.7)))+
            facet_wrap(vars(gcm))+
            theme(panel.spacing.x = unit(2, "lines"))
          if(SPAR==1){
            save.plot(plt,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w-1],"_",r,"_spar1.0"),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
            }else{#just to ease file sorting
              save.plot(plt,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w-1],"_",r,"_spar",SPAR),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
            }
          plt=ggplot(variab)+#Warnings okay
            geom_line(aes(x=year,y=val,color=rcm),size=0.7)+
            geom_hline(aes(yintercept=sdIV/2,linetype="a"),size=1)+
            geom_hline(aes(yintercept=-sdIV/2,linetype="a"),size=1,linetype="dotted")+
            geom_hline(aes(yintercept=0),size=0.5)+
            scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+# keep these colors because need 8
            scale_linetype_manual("",values=c("a"="dotted"),label="Incertitude interne")+
            theme_bw(base_size = 18)+
            theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
            theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
            ggtitle(paste0("Anomalies résiduelles du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
            scale_x_continuous("")+
            scale_y_continuous(paste0("Anomalies résiduelles ( ",units[i]," )"))+
            guides(color = guide_legend(override.aes = list(size = 1.7)))+
            facet_wrap(vars(gcm))+
            theme(panel.spacing.x = unit(2, "lines"))
          if(SPAR==1){
            save.plot(plt,Filename = paste0(lst_indic[i],"_residuals-anomalies_",select_stations$Nom[w-1],"_",r,"_spar1.0"),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
          }else{#just to ease file sorting
            save.plot(plt,Filename = paste0(lst_indic[i],"_residuals-anomalies_",select_stations$Nom[w-1],"_",r,"_spar",SPAR),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
          }
        }
      }
    }
  }
}


###############################################################################################
## Plot change indicator, and its spline and residual anomalies for all models and selection of watersheds by RCP for time
## climate change response

##Merge data frame warnings are okay
SPAR=1.1
for (i in 1:length(lst_indic)){# for each indicator
  dir.create(paste0(path_fig,lst_indic[i],"/plot_chains_relative/"))
  
  all_chains=vector(length=nrow(simu_lst),mode="list")
  for (j in 1:nrow(simu_lst)){
    load(paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[j],"_",simu_lst$gcm[j],"_",simu_lst$rcm[j],"_",simu_lst$bc[j],"_",simu_lst$hm[j],".Rdata"))
    res=res[,c(1,select_stations$idx+1)]
    #res=res[res$year>=first_full_year&res$year<=last_full_year,]
    all_chains[[j]]=res
  }
  
  n_bv=ncol(all_chains[[1]])-1
  for(w in 2:(n_bv+1)){
    
    ClimateProjections=lapply(all_chains, function(x) x[,c(1,w)])
    #ClimateProjections=lapply(ClimateProjections,function(x) x[x$year>=first_full_year & x$year<=last_full_year,][,2])
    Y=t(Reduce(function(...) merge(...,by="year", all=T), ClimateProjections)[,-1])
    nS=nrow(simu_lst)
    X=seq(first_data_year,last_data_year)
    if(lst_indic[i]=="VNC10"){
      clim_resp=prepare_clim_resp(Y=Y,X=X,Xref=1990,Xfut=X,typeChangeVariable = "rel",spar = rep(1.1,nrow(simu_lst)),type = "log_spline")
    }else{
      clim_resp=prepare_clim_resp(Y=Y,X=X,Xref=1990,Xfut=X,typeChangeVariable = "rel",spar = rep(1.1,nrow(simu_lst)),type = "spline")
    }
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
    
    variab=raw
    variab$val=raw$val-spline$val
    variab$gcm=unlist(lapply(strsplit(variab$model,"_"),function(x) x[1]))
    variab$rcm=unlist(lapply(strsplit(variab$model,"_"),function(x) x[2]))
    tmp=pivot_wider(variab[c("year","model","val")],names_from = model,values_from = val)
    sdIV=sqrt(mean(apply(tmp[,-1],2,var,na.rm=T),na.rm=T))
    
    data=rbind(raw,spline)
    data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
    data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
    data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[3]))
    
    # IV=clim_resp$varInterVariability
    # data$ivinf=data$val-IV
    # data$ivsup=data$val+IV
    # data$ciinf=qnorm(p = (1 - 0.9)/2, mean = data$val, sd = sqrt(IV))
    # data$cisup = qnorm(p = 0.5 + 0.9/2, mean = data$val, sd = sqrt(IV))
    # data[data$type=="raw",]$ivinf = data[data$type=="raw",]$ivsup = data[data$type=="raw",]$ciinf = data[data$type=="raw",]$cisup = NA
    # 
    
    for (r in lst_names_eff$rcp){
      plt=ggplot(data[data$rcp==r,])+#Warnings okay
        geom_line(aes(x=year,y=val,size=type,color=rcm))+
        #geom_line(aes(x=year,y=ciinf,color=rcm))+
        #geom_line(aes(x=year,y=cisup,color=rcm))+
        scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse au\nchangement climatique"))+
        scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
        theme_bw(base_size = 18)+
        theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
        theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
        ggtitle(paste0("Chronique du changement relatif du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
        scale_x_continuous("")+
        scale_y_continuous(paste0("Réponse au changement climatique (%)"))+
        guides(color = guide_legend(override.aes = list(size = 1.7)))+
        facet_wrap(vars(gcm))+
        theme(panel.spacing.x = unit(2, "lines"))
     save.plot(plt,Filename = paste0(lst_indic[i],"_chronique_rel_",select_stations$Nom[w-1],"_",r,"_spar",SPAR),Folder = paste0(path_fig,lst_indic[i],"/plot_chains_relative/"),Format = "jpeg")
    
     plt=ggplot(variab)+#Warnings okay
       geom_line(aes(x=year,y=val,color=rcm),size=0.7)+
       geom_hline(aes(yintercept=sdIV/2,linetype="a"),size=1)+
       geom_hline(aes(yintercept=-sdIV/2,linetype="a"),size=1,linetype="dotted")+
       geom_hline(aes(yintercept=0),size=0.5)+
       scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+# keep these colors because need 8
       scale_linetype_manual("",values=c("a"="dotted"),label="Incertitude interne")+
       theme_bw(base_size = 18)+
       theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
       theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
       ggtitle(paste0("Anomalies résiduelles relatives du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
       scale_x_continuous("")+
       scale_y_continuous(paste0("Anomalies résiduelles relatives (%)"))+
       guides(color = guide_legend(override.aes = list(size = 1.7)))+
       facet_wrap(vars(gcm))+
       theme(panel.spacing.x = unit(2, "lines"))
    save.plot(plt,Filename = paste0(lst_indic[i],"_residuals-anomalies_rel_",select_stations$Nom[w-1],"_",r,"_spar",SPAR),Folder = paste0(path_fig,lst_indic[i],"/plot_chains_relative/"),Format = "jpeg")
    }
  }
}





###############################################################################################
## Plot raw indicator, and its spline and residual anomalies for all models and selection of watersheds by RCP for temperature
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
## Climate response not climate change response

tmp=format_global_tas(path_data,first_data_year,last_data_year,simu_lst,first_ref_year,last_ref_year)
mat_Globaltas=tmp[[1]]
tas_years=tmp[[3]][,1]

##Merge data frame warnings are okay
for (SPAR in c(0.5,0.8,0.9,1.0,1.1,1.2,1.5)){
  for (i in 1:length(lst_indic)){# for each indicator
    clim_resp=vector(length=nrow(simu_lst),mode="list")
    clim_resp_spline=vector(length=nrow(simu_lst),mode="list")
    dir.create(paste0(path_fig,lst_indic[i],"/plot_chains_temp/"))
    for(c in 1:nrow(simu_lst)){# for each chain
      
      load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
      res=res[,c(1,select_stations$idx+1)]
      
      #res=cbind(res[,1],apply(res[,-1],2,rollmean,k=nb_year_mean,align="center",fill=NA))#30 years rolling mean
      #res=res[res$year>=first_full_year&res$year<=last_full_year,]
      zz = !is.na(res[,2])

      #spline
      res_spline=res
      tas=mat_Globaltas[c,which(tas_years %in% res[,1])]
      for(j in 2:ncol(res)){
        res_spline[zz,j]=smooth.spline(x=tas,y=res[zz,j],spar = SPAR)$y
      }
      res=res[,-1]
      res_spline=res_spline[,-1]
      res$tas=tas
      res_spline$tas=tas
      clim_resp[[c]]=res
      clim_resp_spline[[c]]=res_spline
    }
    
    for (w in 1:(ncol(clim_resp[[1]])-1)){
      for (r in lst_names_eff$rcp){
        
        chain_r=which(simu_lst$rcp==r)#chains with the right rcp
        raw=clim_resp[[chain_r[1]]][,c(w,ncol(clim_resp[[1]]))]#first iteration outside loop
        spline=clim_resp_spline[[chain_r[1]]][,c(w,ncol(clim_resp[[1]]))]
        for (R in 2:length(chain_r)){
          raw=merge(raw,clim_resp[[chain_r[R]]][,c(w,ncol(clim_resp[[1]]))],by="tas",all=T)
          spline=merge(spline,clim_resp_spline[[chain_r[R]]][,c(w,ncol(clim_resp[[1]]))],by="tas",all=T)
          ## Warnings okay
        }
        colnames(raw)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
        colnames(spline)[-1]=paste0(simu_lst[simu_lst$rcp==r,]$gcm,"_",simu_lst[simu_lst$rcp==r,]$rcm)
        raw=pivot_longer(data=raw,cols=!tas,names_to = "model",values_to = "val")
        raw$type="raw"
        spline=pivot_longer(data=spline,cols=!tas,names_to = "model",values_to = "val")
        spline$type="spline"
        
        variab=raw
        variab$val=raw$val-spline$val
        variab$gcm=unlist(lapply(strsplit(variab$model,"_"),function(x) x[1]))
        variab$rcm=unlist(lapply(strsplit(variab$model,"_"),function(x) x[2]))
        tmp=pivot_wider(variab[c("tas","model","val")],names_from = model,values_from = val)
        sdIV=sqrt(mean(apply(tmp[,-1],2,var,na.rm=T),na.rm=T))
        variab=variab[!is.na(variab$val),]
        
        data=rbind(raw,spline)
        data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
        data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
        data=data[!is.na(data$val),]
        
        plt=ggplot(data)+#Warnings okay
          geom_line(aes(x=tas,y=val,size=type,color=rcm))+
          scale_size_manual("",values=c(0.7,1.7),label=c("Indicateur","Réponse climatique"))+
          scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
          theme_bw(base_size = 18)+
          theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
          theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
          ggtitle(paste0("Chronique du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w]))+
          scale_x_continuous("Température (°C)")+
          scale_y_continuous(paste0("Réponse climatique ( ",units[i]," )"))+
          guides(color = guide_legend(override.aes = list(size = 1.7)))+
          facet_wrap(vars(gcm))+
          theme(panel.spacing.x = unit(2, "lines"))
        # if(lst_indic[i]=="log10VCN10"){
        #   plt=plt+
        #     scale_y_continuous(name = paste0("Réponse climatique ( ",units[i]," )"),sec.axis = sec_axis(~10^(.), name="exposant 10",breaks=c(0.01,0.1,1,10,100,1000)))
        # }
        if(SPAR==1){
          save.plot(plt,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w],"_",r,"_spar1.0"),Folder = paste0(path_fig,lst_indic[i],"/plot_chains_temp/"),Format = "jpeg")
        }else{#just to ease file sorting
          save.plot(plt,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w],"_",r,"_spar",SPAR),Folder = paste0(path_fig,lst_indic[i],"/plot_chains_temp/"),Format = "jpeg")
        }
        # if(min(data$val,na.rm=T)>0){
        #   plt2=plt+coord_trans(y="log10")
        #   save.plot(plt2,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w],"_",r,"_log"),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
        # }else{
        #   print(paste0(lst_indic[i],"_chronique_",select_stations$Nom[w],"_",r,"_log is impossible because of null or negative values in spline"))
        # }
        plt=ggplot(variab)+#Warnings okay
          geom_line(aes(x=tas,y=val,color=rcm),size=0.7)+
          geom_hline(aes(yintercept=sdIV/2,linetype="a"),size=1)+
          geom_hline(aes(yintercept=-sdIV/2,linetype="a"),size=1,linetype="dotted")+
          geom_hline(aes(yintercept=0),size=0.5)+
          scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+# keep these colors because need 8
          scale_linetype_manual("",values=c("a"="dotted"),label="Incertitude interne")+
          theme_bw(base_size = 18)+
          theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
          theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
          ggtitle(paste0("Anomalies résiduelles du ",name_indic[i],"\npour le ",r," et ",select_stations$Nom_complet[w-1]))+
          scale_x_continuous("Température (°C)")+
          scale_y_continuous(paste0("Anomalies résiduelles (%)"))+
          guides(color = guide_legend(override.aes = list(size = 1.7)))+
          facet_wrap(vars(gcm))+
          theme(panel.spacing.x = unit(2, "lines"))
        save.plot(plt,Filename = paste0(lst_indic[i],"_residual-anomalies_",select_stations$Nom[w-1],"_",r,"_spar",SPAR),Folder = paste0(path_fig,lst_indic[i],"/plot_chains_temp/"),Format = "jpeg")
        
      }
    }
    
  }
}

###############################################################################################
## Plot spline for all models and selection of watersheds (color by RCP )for temperature

tmp=format_global_tas(path_data,first_data_year,last_data_year,simu_lst,first_ref_year,last_ref_year)
mat_Globaltas=tmp[[1]]
tas_years=tmp[[3]][,1]

##Merge data frame warnings are okay
SPAR=1.5
for (i in 1:length(lst_indic)){# for each indicator
  clim_resp_spline=vector(length=nrow(simu_lst),mode="list")
  dir.create(paste0(path_fig,lst_indic[i],"/plot_chains_temp_compare_rcp/"))
  for(c in 1:nrow(simu_lst)){# for each chain
    
    load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
    res=res[,c(1,select_stations$idx+1)]
    #res=res[res$year>=first_full_year&res$year<=last_full_year,]
    zz = !is.na(res[,2])
    res_spline=res
    tas=mat_Globaltas[c,which(tas_years %in% res[,1])]
    for(j in 2:ncol(res)){
      res_spline[zz,j]=smooth.spline(x=tas,y=res[zz,j],spar = SPAR)$y
    }
    res_spline=res_spline[,-1]
    res_spline$tas=tas
    clim_resp_spline[[c]]=res_spline
  }
  
  for (w in 1:(ncol(clim_resp_spline[[1]])-1)){
      
      spline=clim_resp_spline[[1]][,c(w,ncol(clim_resp_spline[[1]]))]
      for (R in 2:length(clim_resp_spline)){
        spline=merge(spline,clim_resp_spline[[R]][,c(w,ncol(clim_resp_spline[[1]]))],by="tas",all=T)
        ## Warnings okay
      }
      colnames(spline)[-1]=paste0(simu_lst$rcp,"_",simu_lst$gcm,"_",simu_lst$rcm)
      spline=pivot_longer(data=spline,cols=!tas,names_to = "model",values_to = "val")
      data=spline
      data$rcp=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
      data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
      data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[3]))
      data=data[!is.na(data$val),]
      
      plt=ggplot(data)+#Warnings okay
        geom_line(aes(x=tas,y=val,color=rcp,group=model),size=1)+
        scale_color_discrete("",type = as.vector(col_3rcp),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
        theme_bw(base_size = 18)+
        theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
        theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
        ggtitle(paste0("Réponse climatique de ",name_indic[i]," en fonction de la température\n(",select_stations$Nom_complet[w],")"))+
        scale_x_continuous("Température (°C)")+
        scale_y_continuous(paste0("Réponse climatique ( ",units[i]," )"))+
        guides(color = guide_legend(override.aes = list(size = 1.7)))
      # if(lst_indic[i]=="log10VCN10"){
      #   plt=plt+
      #     scale_y_continuous(name = paste0("Réponse climatique ( ",units[i]," )"),sec.axis = sec_axis(~10^(.), name="exposant 10",breaks=c(0.01,0.1,1,10,100,1000)))
      # }
     save.plot(plt,Filename = paste0(lst_indic[i],"_rep_clim_temp_",select_stations$Nom[w]),Folder = paste0(path_fig,lst_indic[i],"/plot_chains_temp_compare_rcp/"),Format = "jpeg")
  }
}


#################################################################################################################################
## Percentage of discharge value <0.001 (m3/s,days...)  and of outliers (chain of worst case for each basin) in indicators

for (i in 1:length(lst_indic)){# for each indicator
  nr=nrow(sim_stations)
  perc_0001=data.frame(matrix(ncol = nr, nrow = nrow(simu_lst)))#percentage values <0.001
  perc_out=data.frame(matrix(ncol = nr, nrow = nrow(simu_lst)))#percentage of outliers
  for(c in 1:nrow(simu_lst)){# for each chain
    load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
    perc_0001[c,]=as.vector(apply(res[,-1],MARGIN = 2,function(x) sum(x<0.001)/length(x)*100))
    perc_out[c,]=as.vector(apply(res[,-1],MARGIN = 2,function(x) if(length(tsoutliers(x)$index)!=0){length(tsoutliers(x)$index)/length(x)*100}else{0}))
  }
  
  data=data.frame(perc_0001=vector(length=nr),perc_out=vector(length=nr))
  data$x=as.numeric(sim_stations$Lon)
  data$y=as.numeric(sim_stations$Lat)
  data$perc_0001=apply(perc_0001,MARGIN = 2,function(x) max(x) )#worst case
  data$perc_out=apply(perc_out,MARGIN = 2,function(x) max(x) )#worst case
  
  plt1=base_map_outlets(data,"perc_0001")+
    scale_fill_stepsn("Values <0.001 (%)",colors=ipcc_yelblue_5,limits=c(0,10),oob=oob_squish)+
    ggtitle(paste0("Percentage of values <0.001 for ",lst_indic[i],"\n(worst chain for each watershed)"))
  save.plot(plt1,Filename = paste0(lst_indic[i],"_perc0001"),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  plt2=base_map_outlets(data,"perc_out")+
    scale_fill_stepsn("Outliers (%)",colors=ipcc_yelblue_5,limits=c(0,10),oob=oob_squish)+
    ggtitle(paste0("Percentage of outliers (from tsoutliers) for ",lst_indic[i],"\n(worst chain for each watershed)"))
  save.plot(plt2,Filename = paste0(lst_indic[i],"_percout"),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  
}

