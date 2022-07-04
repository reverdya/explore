#originally in check_indic

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
      if(lst_indic[i]=="VCN10"){
        res_spline[zz,j]=10^smooth.spline(x=vecYears,y=log10(res[zz,j]),spar = 1.1)$y
      }else{
        res_spline[zz,j]=smooth.spline(x=vecYears,y=res[zz,j],spar = 1.1)$y
      }
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
    scale_fill_stepsn("P-value",colours=ipcc_yelblue_5,limits=c(0.05,1),breaks=c(0.05,0.2,0.4,0.6,0.8,1),na.value = "grey",oob=oob_censor)+#oob_censor puts <0.05 as NA
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
      ggtitle(paste0("Q-Q plot of manually calculated residuals for 20 last years ( ",select_stations$Nom_complet[w]," )"))
    save.plot(plt3,Filename = paste0(lst_indic[i],"_qqplot-resid_",select_stations$Nom[w]),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
    plt4=ggplot(data2)+
      geom_histogram(aes(x=val),bins=30,fill="cornflowerblue",color="black")+
      scale_x_continuous("Residuals")+
      theme_bw(base_size = 18)+
      theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
      theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
      ggtitle(paste0("Histogram of manually calculated residuals for 20 last years ( ",select_stations$Nom_complet[w]," )"))
    save.plot(plt4,Filename = paste0(lst_indic[i],"_hist-resid_",select_stations$Nom[w]),Folder = paste0(path_fig,lst_indic[i],"/"),Format = "jpeg")
  }
  
}