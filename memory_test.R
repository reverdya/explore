####################
##Proof of concept##
####################


##Libraries
library(rstudioapi)
library(ggplot2)

##create a references objects that should stay in memory
a=b=1
gc()
memory.size()/1024

##overwrite a huge dataframe in a loop
df=matrix(data=0,nrow = 1e5,ncol=1e4)
print(object.size(df),units = "auto")
# for(i in 1:1000){
#   df=matrix(data=i,nrow = 1e5,ncol=1e4)
#   if(i%%100==0){memory.size()/1024}
# }
## remove and clean
memory.size()/1024
rm(df)
memory.size()/1024
gc()
memory.size()/1024

##Restart R and Check memory.size
restartSession(command="memory.size()/1024")#command as a string


################################################
##Next need to be manually run step by step

##Restart R and Check variables are still loaded 
restartSession(command="print(a+b)")

##Restart R and Check library is still loaded 
restartSession(command="?geom_point")



