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

load(paste0(path_data,"J2000_CNRMCM5LR_ALADIN63_hist_3stations.Rdata"))
head(dataJ2000)
dim(dataJ2000)
station=unique(dataJ2000$Station)
forcage=unique(dataJ2000$forcage)
dataJ2000$Station_name=dataJ2000$Station
dataJ2000$Station_name[dataJ2000$Station_name=="M6240010"]="Loire à Nantes"
dataJ2000$Station_name[dataJ2000$Station_name=="L3200610"]="Vienne à Ingrandes"
dataJ2000$Station_name[dataJ2000$Station_name=="M0680610"]="Sarthe à St Denis d'Anjou"

# dataJ2000$year=year(dataJ2000$Time)
# Normalize by year mean?

##Density plot

plt=ggplot(dataJ2000)+
  geom_density(aes(x=Runoff,color=forcage,fill=forcage),alpha=0.4)+
  facet_wrap(~Station_name, scales = "free")+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  scale_x_continuous(trans = "log10")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"))+
  ggtitle("historical/CNRM-CM5/ALADIN63 (1976-2005)")
plt
save.plot(plt,Filename = paste0("comparaisonBC_J2000"),Folder = path_fig,Format = "jpeg") 

##Density plot woth year mean removed

dataJ2000$year=year(dataJ2000$Time)
data_year=aggregate(dataJ2000$Runoff,by=list(dataJ2000$year,dataJ2000$forcage,dataJ2000$Station),mean)
colnames(data_year)=c("year","forcage","Station","Runoff")
dataJ2000$Runoff_detrend=dataJ2000$Runoff
for(i in 1:nrow(dataJ2000)){
  dataJ2000$Runoff_detrend[i]=dataJ2000$Runoff[i]-data_year[data_year$year==dataJ2000$year[i]&data_year$Station==dataJ2000$Station[i]&data_year$forcage==dataJ2000$forcage[i],"Runoff"]
}


trans_log10neg <- trans_new(name = "log10neg",transform = function(x) log10(x+2000),inverse = function(x) (10^x)-2000)


plt=ggplot(dataJ2000)+
  geom_density(aes(x=Runoff_detrend,color=forcage,fill=forcage),alpha=0.4)+
  facet_wrap(~Station_name, scales = "free")+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  scale_x_continuous("Runoff anomalies",trans=trans_log10neg,breaks = c(-1000,0,500,1000,5000),labels=c(-1000,0,500,1000,5000))+
  scale_y_continuous(trans="sqrt")+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"))+
  ggtitle("historical/CNRM-CM5/ALADIN63 (1976-2005)\n(yearly mean removed)")
plt
save.plot(plt,Filename = paste0("comparaisonBC_J2000_detrend"),Folder = path_fig,Format = "jpeg") 
