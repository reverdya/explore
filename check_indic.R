# Alix Reverdy
# Explore 2
# check hydrological indicators: normality of residuals, 0, outliers and visual inspection

rm(list=ls())
gc()
dev.off()

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

source('C:/Users/reverdya/Documents/Docs/1_code/explore/general_functions.R')

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/analyse_indic/"

var=c("Debits")
rcp=c("historical","rcp2.6","rcp4.5","rcp8.5")
bc=c("ADAMONT")
hm=c("SIM2")

load(file=paste0(path_data,"processed/lst_indic.Rdata"))
units=c("m3/s","m3/s","log(m3/s)","day of year")

last_full_year_smooth=2083#last year with complete data for a 30 years rolling mean and all chains
spar=1.1
centr_ref_year=1990# central year of 1975-2005 reference period
nb_year_mean=30
typeChangeVar="rel"
first_ref_year=1975
last_ref_year=2005
first_full_year=1972# from raw data filenames
last_full_year=2098# from raw data filenames


###########
#FUNCTIONS#
###########



######
#MAIN#
######

sim_stations=read.csv(file = paste0(path_data,"raw/SIM2/Infos_stations_modcou_OK.csv"),sep=";")
sim_stations=sim_stations[!sim_stations$Num_ordre_Modcou %in% c(626,669,763,764,861),]#take only stations with no NA

select_stations=read.xlsx(paste0(path_data,"raw/SIM2/selection_bassins_SIM2.xlsx"))# Sample of watersheds with a diversity of characteristic for plotting
idx=which(sim_stations$Num_ordre_Modcou%in%select_stations$Numero_Modcou)
select_stations=select_stations[order(select_stations$Numero_Modcou),]#because idx is reordered
select_stations$idx=idx

load(file=paste0(path_data,"processed/simu_lst.Rdata"))

names_eff=colnames(simu_lst)[2:4]
lst_names_eff=vector(mode="list",length=length(names_eff))#list of effects and their possibilities
for(i in 1:length(lst_names_eff)){
  names(lst_names_eff)[i]=names_eff[i]
  lst_names_eff[[i]]=unique(simu_lst[,names_eff[i]])
}

####################################################################################
## Shapiro-Wilke and skewness maps for residuals manually calculated for last year
##Check normality of residuals

## Histogram and Q/Q plot 20 years and all residuals for selection of watershed
## Look more closely at normality of residuals for a few watersheds



for (i in 1:length(lst_indic)){# for each indicator
  clim_resp=vector(length=nrow(simu_lst),mode="list")
  dir.create(paste0(path_fig,lst_indic[i],"/"))
  for(c in 1:nrow(simu_lst)){# for each chain
    
    load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
    nr=ncol(res)-1
    
    #res_smooth=cbind(res[,1],apply(res[,-1],2,rollmean,k=nb_year_mean,align="center",fill=NA))#30 years rolling mean
    res=res[res$year>=first_full_year&res$year<=last_full_year,]
    zz = !is.na(res[,2])
    vecYears=res[zz,1]
    #spline
    res_spline=res
    for(j in 2:ncol(res)){
      res_spline[zz,j]=smooth.spline(x=vecYears,y=res[zz,j],spar = spar)$y
      if(typeChangeVar=='abs'){
        res_spline[zz,j]=res_spline[zz,j]-res_spline[which(res_spline[,1]==centr_ref_year),j]
      }else if(typeChangeVar=='rel'){
        res_spline[zz,j]=res_spline[zz,j]/res_spline[which(res_spline[,1]==centr_ref_year),j]-1
      }else{
        stop("argument type.change.var must be equal to 'abs' (absolute changes) or 'rel' (relative changes)")
      }
    }
    clim_resp[[c]]=res_spline[res_spline[,1] %in% c((last_full_year_smooth-19):last_full_year_smooth),-1]#keep 20 last years
  }
  
  clim_resp=abind(clim_resp,along=3)
  resid=clim_resp
  for(y in dim(resid)[1]){#loop 20 years
    for(w in dim(resid)[2]){#loop watersheds
      
      mu=mean(clim_resp[y,w,])#ensemble mean
      
      ## Individual effects
      eff_gcm=vector(length=length(lst_names_eff$gcm))
      for(e in length(eff_gcm)){
        eff_gcm=mean(clim_resp[y,w,simu_lst$gcm==lst_names_eff$gcm[e]])
      }
      eff_rcm=vector(length=length(lst_names_eff$rcm))
      for(e in length(eff_rcm)){
        eff_rcm=mean(clim_resp[y,w,simu_lst$rcm==lst_names_eff$rcm[e]])
      }
      eff_rcp=vector(length=length(lst_names_eff$rcp))
      for(e in length(eff_rcp)){
        eff_rcp=mean(clim_resp[y,w,simu_lst$rcp==lst_names_eff$rcp[e]])
      }
      for(c in dim(resid)[3]){#loop on chains
        sum_eff=mu+eff_gcm[lst_names_eff$gcm==simu_lst$gcm[c]]+eff_rcm[lst_names_eff$rcm==simu_lst$rcm[c]]+eff_rcp[lst_names_eff$rcp==simu_lst$rcp[c]]
        resid[y,w,c]=sum_eff - clim_resp[y,w,c]#residuals=sum effects - climate responses
      }
    }
    
  }
  
  data=data.frame(pv=vector(length=nr),skew=vector(length=nr))
  row.names(data)=colnames(res)[2:(nr+1)]
  data$x=as.numeric(sim_stations$Lon)
  data$y=as.numeric(sim_stations$Lat)
  data$pv=apply(resid[20,,],MARGIN = 1,function(x) shapiro.test(x)$p.value )#Shapiro-Wilk (p<0.05 distribution not normal)) for last year
  data$skew=apply(resid[20,,],MARGIN = 1,function(x) skewness(x) )#skewness of residuals for last year
  plt1=base_map_outlets(data,"pv")+
    scale_fill_stepsn("P-value",colours=yellow_blue_5,limits=c(0.05,1),breaks=c(0.05,0.2,0.4,0.6,0.8,1),na.value = "grey",oob=oob_censor)+#oob_censor puts <0.05 as NA
    ggtitle(paste0(lst_indic[i]," ( ",last_full_year_smooth ," )\nShapiro-Wilk normality test (grey is < 0.05 and non-normal)"))
  save.plot(plt1,Filename = paste0(lst_indic[i],"_shapiro-pv-resid"),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  plt2=base_map_outlets(data,"skew")+
    scale_fill_stepsn("Skewness",colours=temp_5,limits=c(-1,1),breaks=c(-1,-0.6,-0.2,0.2,0.6,1),na.value = "grey",oob=oob_censor)+
    ggtitle(paste0(lst_indic[i]," ( ",last_full_year_smooth ," )\nSkewness (grey is out of -1/1)"))
  save.plot(plt2,Filename = paste0(lst_indic[i],"_skewness-resid"),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  
  
  for(w in 1:nrow(select_stations)){
    data2=data.frame(val=c(resid[,select_stations$idx[w],]))
    plt3=ggplot(data2)+
      geom_qq(aes(sample=val))+
      geom_abline(slope=1)+
      theme_bw(base_size = 18)+
      theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      ggtitle(paste0("Q-Q plot of manually calculated residuals for 20 last years ( ",select_stations$Nom[w]," )"))
    save.plot(plt3,Filename = paste0(lst_indic[i],"_qqplot-resid_",select_stations$Nom[w]),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
    plt4=ggplot(data2)+
      geom_histogram(aes(x=val),bins=30,fill="cornflowerblue",color="black")+
      scale_x_continuous("Residuals")+
      theme_bw(base_size = 18)+
      theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      ggtitle(paste0("Histogram of manually calculated residuals for 20 last years ( ",select_stations$Nom[w]," )"))
    save.plot(plt4,Filename = paste0(lst_indic[i],"_hist-resid_",select_stations$Nom[w]),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  }

}


###############################################################################################
## Plot raw indicator, and its spline for all models and selection of watersheds by RCP for time
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
## Climate response not climate change response


##Merge data frame warnings are okay
for (SPAR in c(0.5,0.8,0.9,1.0,1.1,1.2,1.5)){
  for (i in 1:length(lst_indic)){# for each indicator
    clim_resp=vector(length=nrow(simu_lst),mode="list")
    clim_resp_spline=vector(length=nrow(simu_lst),mode="list")
    dir.create(paste0(path_fig,lst_indic[i],"/plot_chains/"))
    for(c in 1:nrow(simu_lst)){# for each chain
      
      load(file = paste0(path_data,"processed/indic_hydro/",lst_indic[i],"_",simu_lst$rcp[c],"_",simu_lst$gcm[c],"_",simu_lst$rcm[c],"_",simu_lst$bc[c],"_",simu_lst$hm[c],".Rdata"))
      res=res[,c(1,select_stations$idx+1)]
      
      #res=cbind(res[,1],apply(res[,-1],2,rollmean,k=nb_year_mean,align="center",fill=NA))#30 years rolling mean
      res=res[res$year>=first_full_year&res$year<=last_full_year,]
      zz = !is.na(res[,2])
      vecYears=res[zz,1]
      #spline
      res_spline=res
      for(j in 2:ncol(res)){
        res_spline[zz,j]=smooth.spline(x=vecYears,y=res[zz,j],spar = SPAR)$y
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
        raw=gather(raw,key = "model",value = "val",-year)
        raw$type="raw"
        spline=gather(spline,key = "model",value = "val",-year)
        spline$type="spline"
        data=rbind(raw,spline)
        data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
        data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
        
        plt=ggplot(data)+#Warnings okay
          geom_line(aes(x=year,y=val,size=type,color=rcm))+
          scale_size_manual("",values=c(0.7,1.7),label=c("Climate response","Spline fit"))+
          scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
          theme_bw(base_size = 18)+
          theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
          theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
          ggtitle(paste0("Time series of absolute change of ",lst_indic[i],"\nfor ",r," at ",select_stations$Nom[w-1]))+
          scale_x_continuous("")+
          scale_y_continuous(paste0("Climate response ( ",units[i]," )"))+
          guides(color = guide_legend(override.aes = list(size = 1.7)))+
          facet_wrap(vars(gcm))+
          theme(panel.spacing.x = unit(2, "lines"))
        if(SPAR==1){
          save.plot(plt,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w-1],"_",r,"_spar1.0"),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
          }else{#just to ease file sorting
            save.plot(plt,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w-1],"_",r,"_spar",SPAR),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
          }
        # if(min(data$val,na.rm=T)>0){
        #   plt2=plt+coord_trans(y="log10")
        #   save.plot(plt2,Filename = paste0(lst_indic[i],"_chronique_",select_stations$Nom[w-1],"_",r,"_log"),Folder = paste0(path_fig,lst_indic[i],"/plot_chains/"),Format = "jpeg")
        # }else{
        #   print(paste0(lst_indic[i],"_chronique_",select_stations$Nom[w-1],"_",r,"_log is impossible because of null or negative values in spline"))
        # }
        
      }
    }
    
  }
}

###############################################################################################
## Plot raw indicator, and its spline for all models and selection of watersheds by RCP for temperature
## Check for coherence of using spline and possible chains that are outlying
## checks particularly that data is not cyclical
## Climate response not climate change response

tmp=format_global_tas(path_data,first_full_year,last_full_year,simu_lst,first_ref_year,last_ref_year)
mat_Globaltas=tmp[[1]]

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
      res=res[res$year>=first_full_year&res$year<=last_full_year,]
      zz = !is.na(res[,2])

      #spline
      res_spline=res
      tas=mat_Globaltas[c,]
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
        raw=gather(raw,key = "model",value = "val",-tas)
        raw$type="raw"
        spline=gather(spline,key = "model",value = "val",-tas)
        spline$type="spline"
        data=rbind(raw,spline)
        data$gcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[1]))
        data$rcm=unlist(lapply(strsplit(data$model,"_"),function(x) x[2]))
        data=data[!is.na(data$val),]
        
        plt=ggplot(data)+#Warnings okay
          geom_line(aes(x=tas,y=val,size=type,color=rcm))+
          scale_size_manual("",values=c(0.7,1.7),label=c("Climate response","Spline fit"))+
          scale_color_manual("RCM",values=brewer.paired(length(unique(data$rcm))))+
          theme_bw(base_size = 18)+
          theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
          theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
          ggtitle(paste0("Time series of absolute change of ",lst_indic[i],"\nfor ",r," at ",select_stations$Nom[w]))+
          scale_x_continuous("Temperature (degC)")+
          scale_y_continuous(paste0("Climate response ( ",units[i]," )"))+
          guides(color = guide_legend(override.aes = list(size = 1.7)))+
          facet_wrap(vars(gcm))+
          theme(panel.spacing.x = unit(2, "lines"))
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
        
      }
    }
    
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
    scale_fill_gradientn("Values <0.001 (%)",colors=parula(100),limits=c(0,25),oob=oob_squish)+
    ggtitle(paste0("Percentage of values <0.001 for ",lst_indic[i],"\n(worst chain for each watershed)"))
  save.plot(plt1,Filename = paste0(lst_indic[i],"_perc0001"),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  plt2=base_map_outlets(data,"perc_out")+
    scale_fill_gradientn("Outliers (%)",colors=parula(100),limits=c(0,25),oob=oob_squish)+
    ggtitle(paste0("Percentage of outliers (from tsoutliers) for ",lst_indic[i],"\n(worst chain for each watershed)"))
  save.plot(plt2,Filename = paste0(lst_indic[i],"_percout"),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  
}
