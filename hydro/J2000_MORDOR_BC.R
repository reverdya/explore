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

####################
#DEFAULT PARAMETERS#
####################

path_data="C:/Users/reverdya/Documents/Docs/2_data/raw/J2000/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/meteo/analyse-indic/compare_BC/"


######
#MAIN#
######

load(paste0(path_data,"J2000_hist_5stations_2.Rdata"))
head(dataJ2000)
dim(dataJ2000)
print(unique(dataJ2000$Station))
print(unique(dataJ2000$gcm))
print(unique(dataJ2000$rcm))
print(unique(dataJ2000$bc))
dataJ2000$model=paste0(dataJ2000$gcm,"/",dataJ2000$rcm)
dataJ2000$Station_name=dataJ2000$Station
dataJ2000$Station_name[dataJ2000$Station_name=="M6240010"]="Loire à Nantes"
dataJ2000$Station_name[dataJ2000$Station_name=="L3200610"]="Vienne à Ingrandes"
dataJ2000$Station_name[dataJ2000$Station_name=="M0680610"]="Sarthe à St Denis d'Anjou"
dataJ2000$Station_name[dataJ2000$Station_name=="L8000020"]="Loire à Saumur"
dataJ2000$Station_name[dataJ2000$Station_name=="K1930010"]="Loire à Nevers"

duplicata=dataJ2000[dataJ2000$model=="safran/safran",]# duplicate curve safran
duplicata$model=paste0(unique(dataJ2000$gcm)[2],"/",unique(dataJ2000$rcm)[2])
dataJ2000[dataJ2000$model=="safran/safran",]$model=paste0(unique(dataJ2000$gcm)[3],"/",unique(dataJ2000$rcm)[3])
dataJ2000=rbind(dataJ2000,duplicata)

dataJ2000$Station_name=factor(dataJ2000$Station_name,levels=c("Loire à Nantes","Loire à Saumur","Loire à Nevers","Vienne à Ingrandes","Sarthe à St Denis d'Anjou"))


##Density plot with log10 x-axis

plt=ggplot(dataJ2000)+
  geom_density(aes(x=Runoff,color=bc,fill=bc),alpha=0.4)+
  facet_grid(model~Station_name, scales = "free_x")+
  scale_color_viridis_d("",labels=c("ADAMONT","R2D2-2L","SAFRAN"))+
  scale_fill_viridis_d("",labels=c("ADAMONT","R2D2-2L","SAFRAN"))+
  scale_x_continuous("Runoff (m3/s)",trans = "log10",expand = expansion(mult = c(0, 0),add = c(0.2, 0)))+
  scale_y_continuous("Density (KDE)")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"))+
  ggtitle("Historical (1976-2005)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 10))
plt
save.plot(plt,Filename = paste0("comparaisonBC_J2000"),Folder = path_fig,Format = "jpeg") 

## Sorted discharge curve

plt=ggplot(dataJ2000)+
  stat_ecdf(aes(x=Runoff,color=bc),geom="step",size=1.5,alpha=0.8)+
  facet_grid(model~Station_name, scales = "free_x")+
  scale_color_viridis_d("",labels=c("ADAMONT","R2D2-2L","SAFRAN"))+
  scale_x_continuous("Runoff (m3/s)",trans = "log10")+
  scale_y_continuous("Fréquence au non-dépassement")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"))+
  ggtitle("Courbe des débit classés\nRuns historical (1976-2005)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 10))
plt
save.plot(plt,Filename = paste0("comparaisonBC_J2000_ecdf"),Folder = path_fig,Format = "jpeg") 

##Density plot with year mean removed

dataJ2000$year=year(dataJ2000$Time)
data_year=aggregate(dataJ2000$Runoff,by=list(dataJ2000$year,dataJ2000$model,dataJ2000$Station),mean)
colnames(data_year)=c("year","model","Station","Runoff")
dataJ2000$Runoff_detrend=dataJ2000$Runoff
for(i in 1:nrow(dataJ2000)){
  dataJ2000$Runoff_detrend[i]=dataJ2000$Runoff[i]-data_year[data_year$year==dataJ2000$year[i]&data_year$Station==dataJ2000$Station[i]&data_year$model==dataJ2000$model[i],"Runoff"]
}


trans_log10neg <- trans_new(name = "log10neg",transform = function(x) log10(x+2000),inverse = function(x) (10^x)-2000)


plt=ggplot(dataJ2000)+
  geom_density(aes(x=Runoff_detrend,color=bc,fill=bc),alpha=0.4)+
  facet_grid(model~Station_name, scales = "free_x")+
  scale_color_viridis_d("",labels=c("ADAMONT","R2D2-2L","SAFRAN"))+
  scale_fill_viridis_d("",labels=c("ADAMONT","R2D2-2L","SAFRAN"))+
  scale_x_continuous("Runoff anomalies (m3/s)",trans=trans_log10neg,breaks = c(-1000,0,500,1000,5000),labels=c(-1000,0,500,1000,5000),expand = expansion(mult = c(0, 0),add = c(0.05, 0.03)))+
  scale_y_continuous("Density (KDE)",trans="sqrt")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"))+
  ggtitle("Historical (1976-2005)\n(yearly mean removed)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 10))
plt
save.plot(plt,Filename = paste0("comparaisonBC_J2000_detrend"),Folder = path_fig,Format = "jpeg") 


## Sorted discharge curve with mean year removed

plt=ggplot(dataJ2000)+
  stat_ecdf(aes(x=Runoff_detrend,color=bc),geom="step",size=1.5,alpha=0.8)+
  facet_grid(model~Station_name, scales = "free_x")+
  scale_color_viridis_d("",labels=c("ADAMONT","R2D2-2L","SAFRAN"))+
  scale_x_continuous("Runoff anomalies (m3/s)",trans=trans_log10neg,breaks = c(-1000,0,500,1000,5000),labels=c(-1000,0,500,1000,5000),expand = expansion(mult = c(0, 0),add = c(0.05, 0.03)))+
  scale_y_continuous("Fréquence au non-dépassement")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"))+
  ggtitle("Courbe des débit classés (soustraits de la moyenne annuelle)\nRuns historical (1976-2005)")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(strip.text.x = element_text(size = 10))
plt
save.plot(plt,Filename = paste0("comparaisonBC_J2000_detrend_ecdf"),Folder = path_fig,Format = "jpeg") 
