###############################################
##Self-calling a new iteration after restart##
###############################################

rm(list=ls())
gc()
library(rstudioapi)

n_iter=10

toto=function(){
  df=matrix(data=0,nrow = 1e5,ncol=1e4)
  rm(df)
  gc()
  print(memory.size()/1024)
}

restart_loop=function(fct,cpt){
  fct #the heavy function to be ran 
  print(cpt)
  assign("CPT",cpt+1,envir = globalenv())
  if((cpt)!=n_iter){
    restartSession(command="restart_loop(fct=toto(),cpt=CPT)")
  }
}

restart_loop(fct=toto(),cpt=1)
