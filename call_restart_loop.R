###############################################
##Self-calling a new iteration after restart##
###############################################

rm(list=ls())
gc()
library(rstudioapi)

FCT=toto=function(CPT=1){#need to name an exemplar of the function FCT and to have only one parameter with default CPT=1
  df=matrix(data=CPT,nrow = 1e5,ncol=1e4)
  print(memory.size()/1024)
}

restart_loop=function(fct,cpt=1,n_iter,step){
  fct #the heavy function to be ran 
  print(cpt)
  if(cpt==1){
    assign("STEP",step,envir = globalenv())
    assign("N_ITER",n_iter,envir = globalenv())
    
  }
  assign("CPT",cpt+1,envir = globalenv())
  if((cpt)!=n_iter){
    if(cpt%%step==0){
      restartSession(command="restart_loop(fct=FCT(CPT),cpt=CPT,n_iter=N_ITER,step=STEP)")#using only stuff in global environment
    }else{
      restart_loop(fct=FCT(CPT),cpt=CPT,n_iter=N_ITER,step=STEP)
    }
    
  }
}

restart_loop(fct=toto(),n_iter=10000,step=2)
