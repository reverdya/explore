###############################################
##Self-calling a new iteration after restart##
###############################################

###############################################################################
## To manually copy paste in console to load global variables and initiate
print("Manually copy/paste/run to console what's inside the function and add your global variables")
manual_run=function(){
        i=1
        #global variables
}
###############################################################################


##########
## "Loop"
# I wanted to add a while loop to add a condition to run the restart one out of two times but kept crashing the session
  
#...
library(rstudioapi)
pth_rscript_original="C:/Users/reverdya/Documents/Docs/1_code/explore/memory_test2.R"
last=10




i=i+1
print(i-1)
if((i-1)!=last){
  restartSession(command="source(pth_rscript_original)")
}



