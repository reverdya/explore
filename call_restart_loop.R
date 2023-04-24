##############################################
##Self-calling a new iteration after restart##
##############################################
# Alix Reverdy
# 24/04/2023

rm(list=ls())
gc()
library(rstudioapi)#works only in Rstudio

## the heavy function to be ran (here an example with a heavy dataframe to be overwritten)
toto=function(CPT){#need to have a function that will loop on a counter
  df=matrix(data=CPT,nrow = 1e5,ncol=1e4)# watch out this is 7Go !!!!
  print(memory.size()/1024)
}

#######################################
## The looping and restart function
## fct the heavy function
## cpt the first iteration (do not change default=1)
## last the last iteration
## step the step to which have the restart launched

restart_loop=function(fct,cpt=1,last,step){
  fct(cpt) 
  print(cpt)
  if(cpt==1){
    assign("STEP",step,envir = globalenv())
    assign("LAST",last,envir = globalenv())
    assign("FCT",fct,envir = globalenv())
  }
  assign("CPT",cpt+1,envir = globalenv())
  if((cpt)!=last){
    if(cpt%%step==0){
      restartSession(command="restart_loop(fct=FCT,cpt=CPT,last=LAST,step=STEP)")#using only stuff in global environment
    }else{
      restart_loop(fct=FCT,cpt=CPT,last=LAST,step=STEP)
    }
  }
}

restart_loop(fct=toto,last=10000,step=2)
