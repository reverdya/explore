################################
##Self-calling a new iteration##
################################

## Following for callr (deprecated)
#required to install Rtools through exe installer for processx
#https://cran.r-project.org/bin/windows/Rtools/rtools40.html

# if(nchar(system.file(package='processx'))==0){
#   install.packages("C:/Users/reverdya/Downloads/softwares/processx_3.8.1.tar.gz", repos = NULL, type = "source")#needed more recent version
# }
# if(nchar(system.file(package='callr'))==0){
#   library(remotes)
#   install_github("r-lib/callr")
# }


library(stringr)
pth_rscript_original="C:/Users/reverdya/Documents/Docs/1_code/explore/memory_test2.R"





###############################################################################
## To manually copy paste in console
print("Manually copy/paste/run to console what's inside the function (with the right path)")
manual_run=function(){
        rm(list=ls())
        gc()
        library(stringr)
        pth_rscript_original="C:/Users/reverdya/Documents/Docs/1_code/explore/memory_test2.R"
        i=1
        save(i,file=str_replace(pth_rscript_original,".R",".Rdata"))
}
###############################################################################







###########################
## Load global environment (did not find a way to not have to do it each time, apart from putting it in the above function)
# library(callr)
library(rstudioapi)
last=10
load(file=str_replace(pth_rscript_original,".R",".Rdata"))

#...



##########
## "Loop"
# I wanted to add a while loop to add a condition to run the restart one out of two times but kept crashing the session
  
#...

i=i+1
save(i,file=str_replace(pth_rscript_original,".R",".Rdata"))
if((i-1)!=last){
  print(i-1)
  restartSession(command="source(pth_rscript_original)")
}else{
  file.remove(str_replace(pth_rscript_original,".R",".Rdata"))
}



