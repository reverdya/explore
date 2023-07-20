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

path_data="C:/Users/reverdya/Documents/Docs/2_data/"
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/warming_level/"

load(file=paste0(path_data,"/processed/Explore2-meteo/simu_lst.Rdata"))
ref_year=1990


######
#MAIN#
######

load(file=paste0(path_data,"processed/pred_temp.Rdata"))
load(file=paste0(path_data,"processed/T_coef_spline1990toGlob.Rdata"))




df=data.frame(time=pred_temp[[1]]$year,x1=pred_temp[[46]]$temp_spline,x2=pred_temp[[47]]$temp_spline,y1=pred_temp[[46]]$temp_raw,y2=pred_temp[[47]]$temp_raw)
df[,"y1"]=log10(df[,"y1"]-279)*2.1+0.75 #fake local indicator from global temperature
df[,"y2"]=log10(df[,"y2"]-277)*2

Xref=c(df$x1[df$time==1990],df$x2[df$time==1990])

plt1=ggplot(df)+
  geom_line(aes(x=time,y=x1),color="red",size=1.2)+
  geom_line(aes(x=time,y=x2),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  geom_hline(yintercept=Xref[1],color="red",lty="dotted",size=1.2)+
  geom_hline(yintercept=Xref[2],color="blue",lty="dotted",size=1.2)+
  scale_x_continuous(limits=c(1950,2105),expand=c(0,0))+
  scale_y_continuous(limits=c(283,290),expand=c(0,0))+
  xlab("Temps")+
  ylab("Température (K)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Température France\nen fonction du temps")+
  annotate("text", x = 1990, y = 289, label = "1990",fontface = "bold",size=3)+
  annotate("text", x = 1960, y = Xref[1]-0.2, label = "Xref1",fontface = "bold",size=3,color="red")+
  annotate("text", x = 1960, y = Xref[2]+0.2, label = "Xref2",fontface = "bold",size=3,color="blue")
plt1

df$Dx1=df$x1-Xref[1]
df$Dx2=df$x2-Xref[2]

plt2=ggplot(df)+
  geom_line(aes(x=time,y=Dx1),color="red",size=1.2)+
  geom_line(aes(x=time,y=Dx2),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  scale_x_continuous(limits=c(1950,2105),expand=c(0,0))+
  scale_y_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("Temps")+
  ylab("DTempérature (°C)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Anomalie de température France en\nfonction du temps (par rapport à 1990)")+
  annotate("text", x = 1990, y = 4.5, label = "1990",fontface = "bold",size=3)
plt2

# X_1990_obs=0.6
# df$Dx1_PI=df$Dx1+X_1990_obs
# df$Dx2_PI=df$Dx2+X_1990_obs
# 
# plt3=ggplot(df)+
#   geom_line(aes(x=time,y=Dx1_PI),color="red",size=1.2)+
#   geom_line(aes(x=time,y=Dx2_PI),color="blue",size=1.2)+
#   geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
#   scale_x_continuous(limits=c(1965,2105),expand=c(0,0))+
#   scale_y_continuous(limits=c(-0.5,6),expand=c(0,0))+
#   xlab("Temps")+
#   ylab("Niveau de réchauffement (°C)")+
#   theme_bw(base_size = 12)+
#   theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
#   theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
#   ggtitle("Réchauffement planétaire en fonction\ndu temps (référence 1875)")+
#   annotate("text", x = 1990, y = 4.5, label = "1990",fontface = "bold",size=3)
# plt3

plt4=ggplot(df)+
  geom_line(aes(x=time,y=y1),color="red",size=1.2)+
  geom_line(aes(x=time,y=y2),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  scale_x_continuous(limits=c(1950,2105),expand=c(0,0))+
  scale_y_continuous(limits=c(1.25,3),expand=c(0,0))+
  xlab("Temps")+
  ylab("Qan (m3/s)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Débit moyen annuel en fonction\ndu temps")+
  annotate("text", x = 1990, y = 2.75, label = "1990",fontface = "bold",size=3)
plt4

plt5=ggplot(df)+
  geom_line(aes(x=Dx1,y=y1),color="red",size=1.2)+
  geom_line(aes(x=Dx2,y=y2),color="blue",size=1.2)+
  scale_y_continuous(limits=c(1.25,3),expand=c(0,0))+
  scale_x_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("DTempérature (°C)")+
  ylab("Qan (m3/s)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Débit moyen annuel en fonction de\nl'anomalie de température France\n(par rapport à 1990)")
plt5


df$y1_spline=df$y1
df$y2_spline=df$y2
spline1=smooth.spline(x=df$Dx1,y=df$y1,spar = 1)
spline2=smooth.spline(x=df$Dx2,y=df$y2,spar = 1)
df$y1_spline=spline1$y
df$y2_spline=spline2$y
Yref=c(predict(spline1,0)$y,predict(spline2,0)$y)

plt6=ggplot(df)+
  geom_line(aes(x=Dx1,y=y1),color="red",size=0.8)+
  geom_line(aes(x=Dx2,y=y2),color="blue",size=0.8)+
  geom_line(aes(x=Dx1,y=y1_spline),color="red",size=1.2)+
  geom_line(aes(x=Dx2,y=y2_spline),color="blue",size=1.2)+
  geom_vline(xintercept=0,color="black",lty="dotted",size=1.2)+
  geom_hline(yintercept=Yref[1],color="red",lty="dotted",size=1.2)+
  geom_hline(yintercept=Yref[2],color="blue",lty="dotted",size=1.2)+
  scale_y_continuous(limits=c(1.25,3),expand=c(0,0))+
  scale_x_continuous(limits=c(-1.5,5),expand=c(0,0))+
  xlab("DTempérature (°C)")+
  ylab("Qan (m3/s)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Débit moyen annuel en fonction de\nl'anomalie de température France\n(par rapport à 1990), avec spline")+
  annotate("text", x = -0.75, y = Yref[1], label = "Yref1",fontface = "bold",size=3,color="red")+
  annotate("text", x = -0.75, y = Yref[2], label = "Yref2",fontface = "bold",size=3,color="blue")+
  annotate("text", x = 0, y = 2.75, label = "1990",fontface = "bold",size=3)
plt6

df$Dy1_spline=df$y1_spline-Yref[1]
df$Dy2_spline=df$y2_spline-Yref[2]

plt7=ggplot(df)+
  geom_line(aes(x=Dx1,y=Dy1_spline),color="red",size=1.2)+
  geom_line(aes(x=Dx2,y=Dy2_spline),color="blue",size=1.2)+
  scale_y_continuous(limits=c(-0.125,0.75),expand=c(0,0))+
  scale_x_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("DTempérature (°C)")+
  ylab("DQan (m3/s)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Différence de débit moyen annuel\nen fonction de l'anomalie de température\nFrance (par rapport à 1990)")
plt7


df$Dx1glo=T_coef[1]*df$Dx1+T_coef[2]
df$Dx2glo=T_coef[1]*df$Dx2+T_coef[2]

plt7_bis=ggplot(df)+
  geom_line(aes(x=Dx1glo,y=Dy1_spline),color="red",size=1.2)+
  geom_line(aes(x=Dx2glo,y=Dy2_spline),color="blue",size=1.2)+
  scale_y_continuous(limits=c(-0.125,0.75),expand=c(0,0))+
  scale_x_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("Niveau de réchauffement (°C)")+
  ylab("DQan (m3/s)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Différence de débit moyen annuel\nen fonction du réchauffement planétaire")
plt7_bis




df$y1_splinetime=df$y1
df$y2_splinetime=df$y2
spline1time=smooth.spline(x=df$time,y=df$y1,spar = 1)
spline2time=smooth.spline(x=df$time,y=df$y2,spar = 1)
df$y1_splinetime=spline1time$y
df$y2_splinetime=spline2time$y
Yreftime=c(predict(spline1time,1990)$y,predict(spline2time,1990)$y)

plt8=ggplot(df)+
  geom_line(aes(x=time,y=y1),color="red",size=0.8)+
  geom_line(aes(x=time,y=y2),color="blue",size=0.8)+
  geom_line(aes(x=time,y=y1_splinetime),color="red",size=1.2)+
  geom_line(aes(x=time,y=y2_splinetime),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  geom_hline(yintercept=Yreftime[1],color="red",lty="dotted",size=1.2)+
  geom_hline(yintercept=Yreftime[2],color="blue",lty="dotted",size=1.2)+
  scale_y_continuous(limits=c(1.25,3),expand=c(0,0))+
  scale_x_continuous(limits=c(1950,2105),expand=c(0,0))+
  xlab("Temps")+
  ylab("Qan (m3/s)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Débit moyen annuel en fonction\ndu temps, avec spline")+
  annotate("text", x = 1960, y = Yreftime[1], label = "Yref1",fontface = "bold",size=3,color="red")+
  annotate("text", x = 1960, y = Yreftime[2], label = "Yref2",fontface = "bold",size=3,color="blue")+
  annotate("text", x = 1990, y = 2.75, label = "1990",fontface = "bold",size=3)
plt8

df$Dy1_splinetime=df$y1_splinetime-Yreftime[1]
df$Dy2_splinetime=df$y2_splinetime-Yreftime[2]

plt9=ggplot(df)+
  geom_line(aes(x=time,y=Dy1_splinetime),color="red",size=1.2)+
  geom_line(aes(x=time,y=Dy2_splinetime),color="blue",size=1.2)+
  scale_y_continuous(limits=c(-0.15,0.75),expand=c(0,0))+
  scale_x_continuous(limits=c(1950,2105),expand=c(0,0))+
  xlab("Temps")+
  ylab("DQan (m3/s)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Différence de débit moyen annuel\nen fonction du temps")
plt9




plt=ggarrange(plt1,plt2,plt4,plt5,plt6,plt7,plt7_bis,plt8,plt9,ncol=3,nrow=3,align="v")
plt
save.plot(plt,Filename = "schema_predictor_T",Folder = path_fig,Format = "jpeg")
# plt=ggarrange(plt1+theme_bw(base_size = 6),
#               plt1_bis+theme_bw(base_size = 6),
#               plt2+theme_bw(base_size = 6),
#               plt3+theme_bw(base_size = 6),
#               plt4+theme_bw(base_size = 6),
#               plt5+theme_bw(base_size = 6),
#               plt7+theme_bw(base_size = 6),
#               ncol=3,nrow=3,align="v")
# save.plot(plt,Filename = "schema_predictor_T",Folder = path_fig,Format = "svg")

## Easier to cut and rearrange from jpeg

#########################
## Plot GCM Temperatures

data=lapply(pred_temp,function(x) x[,c(1,3)])
data=Reduce(function(...) merge(...,by="year", all=T), data)
colnames(data)=c("year",unlist(lapply(names(pred_temp),function(x) x[1])))
data=pivot_longer(data=data,cols=!year,names_to = "chain",values_to = "val")
data$val=data$val-273.15
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))

#warning missing data okay, it's HaDGEM
plt=ggplot(data)+
  geom_line(aes(x=year,y=val,col=rcp,group=chain),size=1.2)+
  xlab("")+
  ylab("Température France (°C)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_color_discrete("",type = as.vector(col_3rcp),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  ggtitle("Température France pour les différents RCP/GCM/RCM")
save.plot(plt,Filename = "global_tas",Folder = path_fig,Format = "jpeg")

##############################################
## Plot emergence dates

data=lapply(pred_temp,function(x) x[,c(1,4)])
data=Reduce(function(...) merge(...,by="year", all=T), data)
colnames(data)=c("year",unlist(lapply(names(pred_temp),function(x) x[1])))
data=pivot_longer(data=data,cols=!year,names_to = "chain",values_to = "val")
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))
data$val=T_coef[1]*data$val+T_coef[2]

idx=vector(mode = "list")
temp_ref=c(1,1.5,2,3,4,5)
data=pivot_wider(data[,c("year","chain","val")],names_from = chain,values_from = val)
for(temp in temp_ref){
  idx[[as.character(temp)]]=apply(data[,-1],MARGIN=2,function(x) min(which(x>=temp)))
}

idx=data.frame(do.call(rbind, idx))
idx[idx==Inf]=NA
years=data$year
idx=data.frame(apply(idx,MARGIN=2,function(x) years[x]))
idx$temp=temp_ref
data=pivot_longer(data=idx,cols=!temp,names_to = "chain",values_to = "val")
data$rcp=unlist(lapply(strsplit(data$chain,"_"),function(x) x[1]))


plt=ggplot(data)+
  geom_boxplot(aes(x=val,y=factor(temp),fill=rcp),alpha=0.7,outlier.shape = NA)+
  geom_point(aes(x=val,y=factor(temp),fill=rcp), position = position_jitterdodge(jitter.width = 0.15),size=2,alpha=0.5)+
  xlab("")+
  ylab("Changement de température planétaire (°C)")+
  theme_bw(base_size = 18)+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  theme(plot.title = element_text( face="bold",  size=20,hjust=0.5))+
  scale_x_continuous(breaks=seq(2000,2100,20))+
  scale_fill_discrete("",type = as.vector( col_3rcp),labels=c("RCP 2.6","RCP 4.5","RCP 8.5"))+
  guides(fill=guide_legend(reverse=TRUE))+
  ggtitle("Première année de franchissement des seuils\nde niveau de réchauffement planétaire\npour les différents RCPs (et GCMs/RCMs)")
save.plot(plt,Filename = "dat_threshold_temp",Folder = path_fig,Format = "jpeg")


