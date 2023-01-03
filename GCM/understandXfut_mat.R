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
path_fig="C:/Users/reverdya/Documents/Docs/3_figures/fictiveT/"


######
#MAIN#
######

paths=list.files(paste0(path_data,"raw/Global_temp/"),pattern=glob2rx("global_tas*"),full.names = T)
tas_glob_full=vector(length=length(paths),mode="list")
for ( i in 1:length(paths)){
  tas_glob=read.csv(paths[i],skip=3,sep="",header=F)
  if(grepl("HadGEM2-ES",paths[i],fixed=T)){# no values in 1859
    tas_glob=tas_glob[-1,]
  }
  tas_glob=data.frame(year=tas_glob[,1],tas=apply(tas_glob[,-1],MARGIN = 1,mean))# mean of 12 months
  colnames(tas_glob)[2]=paste0(strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][5],"_",strsplit(strsplit(paths[i],"/")[[1]][9],"_")[[1]][4])#rcp_gcm name
  tas_glob_full[[i]]=tas_glob[tas_glob$year<=2100&tas_glob$year>=1861,]
}
mat_Globaltas=lapply(tas_glob_full,function(x) cbind(x[,1],smooth.spline(x=x[,1],y=x[,2],spar = 1)$y))
mat_Globaltas=lapply(mat_Globaltas,function(x) x[x[,1]>=1861&x[,1]<=2100,2])# here 1861 because we wanna show GFDL
indic_local=lapply(tas_glob_full,function(x) x[x[,1]>=1861&x[,1]<=2100,2])# here 1861 because we wanna show GFDL
mat_Globaltas_gcm=data.frame(do.call(cbind,mat_Globaltas))
indic_local=data.frame(do.call(cbind,indic_local))
for (i in 1:ncol(mat_Globaltas_gcm)){
  colnames(mat_Globaltas_gcm)[i]=colnames(tas_glob_full[[i]])[2]
}
colnames(indic_local)=colnames(mat_Globaltas_gcm)
years=seq(1861,2100)



df=data.frame(time=years,x1=mat_Globaltas_gcm[,24],x2=mat_Globaltas_gcm[,18],y1=indic_local[,15],y2=indic_local[,24])
df[,"y1"]=log10(df[,"y1"]-283)*2.5+1 #fake local indicator from global temperature
df[,"y2"]=log10(df[,"y2"]-280)*2.4
df[df$time<1970,c("y1","y2")]=NA

Xref=c(df$x1[df$time==1990],df$x2[df$time==1990])

plt1=ggplot(df)+
  geom_line(aes(x=time,y=x1),color="red",size=1.2)+
  geom_line(aes(x=time,y=x2),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  geom_hline(yintercept=Xref[1],color="red",lty="dotted",size=1.2)+
  geom_hline(yintercept=Xref[2],color="blue",lty="dotted",size=1.2)+
  scale_x_continuous(limits=c(1861,2105),expand=c(0,0))+
  scale_y_continuous(limits=c(286,292),expand=c(0,0))+
  xlab("time")+
  ylab("X")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Température planétaire en fonction\ndu temps")+
  annotate("text", x = 1990, y = 291.5, label = "1990",fontface = "bold",size=3)+
  annotate("text", x = 1880, y = Xref[1], label = "Xref1",fontface = "bold",size=3,color="red")+
  annotate("text", x = 1880, y = Xref[2], label = "Xref2",fontface = "bold",size=3,color="blue")
plt1

df$Dx1=df$x1-Xref[1]
df$Dx2=df$x2-Xref[2]

plt2=ggplot(df)+
  geom_line(aes(x=time,y=Dx1),color="red",size=1.2)+
  geom_line(aes(x=time,y=Dx2),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  scale_x_continuous(limits=c(1861,2105),expand=c(0,0))+
  scale_y_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("time")+
  ylab("DX")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Différence de température planétaire\nen fonction du temps")+
  annotate("text", x = 1990, y = 4.5, label = "1990",fontface = "bold",size=3)
plt2

X_1990_obs=0.6
df$Dx1_PI=df$Dx1+X_1990_obs
df$Dx2_PI=df$Dx2+X_1990_obs

plt3=ggplot(df)+
  geom_line(aes(x=time,y=Dx1_PI),color="red",size=1.2)+
  geom_line(aes(x=time,y=Dx2_PI),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  scale_x_continuous(limits=c(1861,2105),expand=c(0,0))+
  scale_y_continuous(limits=c(-0.5,6),expand=c(0,0))+
  xlab("time")+
  ylab("DX_PI")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Réchauffement planétaire\nen fonction du temps")+
  annotate("text", x = 1990, y = 4.5, label = "1990",fontface = "bold",size=3)
plt3

plt4=ggplot(df)+
  geom_line(aes(x=time,y=y1),color="red",size=1.2)+
  geom_line(aes(x=time,y=y2),color="blue",size=1.2)+
  geom_vline(xintercept=1990,color="black",lty="dotted",size=1.2)+
  scale_x_continuous(limits=c(1861,2105),expand=c(0,0))+
  scale_y_continuous(limits=c(1.9,3.5),expand=c(0,0))+
  xlab("time")+
  ylab("Y")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Indicateur en fonction\ndu temps")+
  annotate("text", x = 1990, y = 3.4, label = "1990",fontface = "bold",size=3)
plt4

plt5=ggplot(df)+
  geom_line(aes(x=Dx1_PI,y=y1),color="red",size=1.2)+
  geom_line(aes(x=Dx2_PI,y=y2),color="blue",size=1.2)+
  scale_y_continuous(limits=c(1.9,3.5),expand=c(0,0))+
  scale_x_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("DX_PI")+
  ylab("Y")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Indicateur en fonction du\nréchauffement planétaire")
plt5


df$y1_spline=df$y1
df$y2_spline=df$y2
spline1=smooth.spline(x=df$Dx1_PI[df$time>=1970],y=df$y1[df$time>=1970],spar = 1)
spline2=smooth.spline(x=df$Dx2_PI[df$time>=1970],y=df$y2[df$time>=1970],spar = 1)
df$y1_spline[df$time>=1970]=spline1$y
df$y2_spline[df$time>=1970]=spline2$y
Yref=c(predict(spline1,X_1990_obs)$y,predict(spline2,X_1990_obs)$y)

plt6=ggplot(df)+
  geom_line(aes(x=Dx1_PI,y=y1),color="red",size=0.8)+
  geom_line(aes(x=Dx2_PI,y=y2),color="blue",size=0.8)+
  geom_line(aes(x=Dx1_PI,y=y1_spline),color="red",size=1.2)+
  geom_line(aes(x=Dx2_PI,y=y2_spline),color="blue",size=1.2)+
  geom_vline(xintercept=X_1990_obs,color="black",lty="dotted",size=1.2)+
  geom_hline(yintercept=Yref[1],color="red",lty="dotted",size=1.2)+
  geom_hline(yintercept=Yref[2],color="blue",lty="dotted",size=1.2)+
  scale_y_continuous(limits=c(1.9,3.5),expand=c(0,0))+
  scale_x_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("DX_PI")+
  ylab("Y")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Indicateur en fonction du\nréchauffement planétaire\navec spline")+
  annotate("text", x = -0.5, y = Yref[1], label = "Yref1",fontface = "bold",size=3,color="red")+
  annotate("text", x = -0.5, y = Yref[2], label = "Yref2",fontface = "bold",size=3,color="blue")+
  annotate("text", x = X_1990_obs, y = 3.25, label = "1990",fontface = "bold",size=3)
plt6

df$Dy1_spline=df$y1_spline-Yref[1]
df$Dy2_spline=df$y2_spline-Yref[2]

plt7=ggplot(df)+
  geom_line(aes(x=Dx1_PI,y=Dy1_spline),color="red",size=1.2)+
  geom_line(aes(x=Dx2_PI,y=Dy2_spline),color="blue",size=1.2)+
  scale_y_continuous(limits=c(-0.125,0.95),expand=c(0,0))+
  scale_x_continuous(limits=c(-1,5),expand=c(0,0))+
  xlab("DX_PI")+
  ylab("DY")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text( face="bold", size=12,hjust=0.5))+
  theme( axis.line = element_line(colour = "black"),panel.border = element_blank())+
  ggtitle("Différence d'indicateur en fonction du\n réchauffement planétaire")
plt7

plt=ggarrange(plt1,plt2,plt3,plt4,plt5,plt6,plt7,ncol=3,nrow=3,align="v")
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



