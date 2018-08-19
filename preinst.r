### README: for the 1st-time installation only. No necessarily for upgrade for previous ver.
### Please place/copy this file under the working directory. Use 'getwd()' to find out
### where the directory is. And then type 'source("preinst.r")' (no quote) under R console
### and select package you want to install. You need Administrator privilege to run this script. 
### Thanks.  --YJ

options(warn=-1)
graphics.off()
pick<-NULL;pick<-6
   
  while(pick!=0){
  cat("\n*** v1.4 (released on May. 04, 2016)***\n\n")
  file.menu <- c("bear","ivivc","PKfit","stab","tdm")
  pick <- menu(file.menu, title = " This R script will update all your currently installed R packages\n and install required packages for (0 to exit!):-")
  if (pick == 1){
     update.packages(ask=F);cat("\n\n")
     cat("  Please wait...\n\n")
     if(!is.element("reshape", installed.packages()[,1])) install.packages("reshape")
     if(!is.element("nlme", installed.packages()[,1])) install.packages("nlme")
     if(!is.element("gdata", installed.packages()[,1])) install.packages("gdata")
     if(!is.element("sciplot", installed.packages()[,1])) install.packages("sciplot")
     if(!is.element("plotrix", installed.packages()[,1])) install.packages("plotrix")
     if(!is.element("ggplot2", installed.packages()[,1])) install.packages("ggplot2")
     if(!is.element("png", installed.packages()[,1])) install.packages("png")
     if(!is.element("grid", installed.packages()[,1])) install.packages("grid")
     if(!is.element("plyr", installed.packages()[,1])) install.packages("plyr")
     if(!is.element("ICSNP", installed.packages()[,1])) install.packages("ICSNP")
     cat(" \n\n If the following package installations fail, please browse\n")
     cat(" http://forum.bebac.at/mix_entry.php?id=15786 for more info.\n")
     cat(" Press Enter to continue. ");pause<-readline();cat("\n\n")
     if(!is.element("randomizeBE", installed.packages()[,1])) install.packages("randomizeBE")
     if(!is.element("gWidgets", installed.packages()[,1])) install.packages("gWidgets")
     if(!is.element("gWidgetsRGtk2", installed.packages()[,1])) install.packages("gWidgetsRGtk2")
     if(.Platform$OS.type=="windows"){
     YesOrNo<-readline("--> To install bear from local zip/binary file (Y/n)? ")
     if(substr(YesOrNo, 1, 1)!="n" && substr(YesOrNo, 1, 1)!="N"){
     utils:::menuInstallLocal();cat("\n\n Type library(bear) to start running bear.\n\n")}
     else{cat("\n\n OK, Done.\n\n")}}
     else{
     cat("\n\n--> Done. Now please install bear from a binary file.\n\n")}}
  if (pick == 2){
     update.packages(ask=F);cat("\n\n")
     cat("  Please wait...\n\n")
     if(!is.element("reshape", installed.packages()[,1])) install.packages("reshape")
     if(!is.element("optimx", installed.packages()[,1])) install.packages("optimx")
     if(!is.element("sciplot", installed.packages()[,1])) install.packages("sciplot")
     if(!is.element("png", installed.packages()[,1])) install.packages("png")
     if(!is.element("grid", installed.packages()[,1])) install.packages("grid")
     if(!is.element("deSolve", installed.packages()[,1])) install.packages("deSolve")
     if(!is.element("minpack.lm", installed.packages()[,1])) install.packages("minpack.lm")
     if(.Platform$OS.type=="windows"){
     YesOrNo<-readline("\n\n--> To install ivivc from local zip/binary file (Y/n)? ")
     if(substr(YesOrNo, 1, 1)!="n" && substr(YesOrNo, 1, 1)!="N"){
     utils:::menuInstallLocal();cat("\n\n Type library(ivivc) to start running ivivc.\n\n")}
     else{cat("\n\n OK, Done.\n\n")}}
     else{
     cat("\n\n--> Done. Now please install ivivc from a binary file.\n\n")}}
  if (pick == 3){
     update.packages(ask=F);cat("\n\n")
     cat("  Please wait...\n\n")
     if(!is.element("optimx", installed.packages()[,1])) install.packages("optimx")
     if(!is.element("deSolve", installed.packages()[,1])) install.packages("deSolve")
     if(!is.element("minpack.lm", installed.packages()[,1])) install.packages("minpack.lm")
     if(!is.element("nls2", installed.packages()[,1])) install.packages("nls2")
     if(.Platform$OS.type=="windows"){
     YesOrNo<-readline("\n\n--> To install PKfit from local zip/binary file (Y/n)? ")
     if(substr(YesOrNo, 1, 1)!="n" && substr(YesOrNo, 1, 1)!="N"){
     utils:::menuInstallLocal();cat("\n\n Type library(PKfit) to start running PKfit.\n\n")}
     else{cat("\n\n OK, Done.\n\n")}}
     else{
     cat("\n\n--> Done. Now please install PKfit from a binary file.\n\n")}}
  if (pick == 4){
     update.packages(ask=F);cat("\n\n")
     cat("  Please wait...\n\n")
     if(!is.element("reshape", installed.packages()[,1])) install.packages("reshape")
     if(.Platform$OS.type=="windows"){
     YesOrNo<-readline("\n\n--> To install stab from local zip/binary file (Y/n)? ")
     if(substr(YesOrNo, 1, 1)!="n" && substr(YesOrNo, 1, 1)!="N"){
     utils:::menuInstallLocal();cat("\n\n Type library(stab) to start running stab.\n\n")}
     else{cat("\n\n OK, Done.\n\n")}}
     else{
     cat("\n\n--> Done. Now please install stab from a binary file.\n\n")}}
  if (pick == 5){
     readline("\n\n Please make sure that JAGS has been intalled before this step.\n Press Enter to continue.\n\n")
     update.packages(ask=F);cat("\n\n")
     cat("  Please wait...\n\n")
     if(!is.element("rjags", installed.packages()[,1])) install.packages("rjags")
     if(!is.element("coda", installed.packages()[,1])) install.packages("coda")
     if(!is.element("png", installed.packages()[,1])) install.packages("png")
     if(!is.element("grid", installed.packages()[,1])) install.packages("grid")
     if(!is.element("deSolve", installed.packages()[,1])) install.packages("deSolve")
     if(.Platform$OS.type=="windows"){
     YesOrNo<-readline("\n\n--> To install tdm from local zip/binary file (Y/n)? ")
     if(substr(YesOrNo, 1, 1)!="n" && substr(YesOrNo, 1, 1)!="N"){
     utils:::menuInstallLocal();cat("\n\n Type library(tdm) to start running tdm.\n\n")}
     else{cat("\n\n OK, Done.\n\n")}}
     else{
     cat("\n\n--> Done. Now please install tdm from a binary file.\n\n")}}}
  if (pick == 0) cat("\n\n Bye now. \n\n");pick<-NULL
