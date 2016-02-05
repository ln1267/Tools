name_dir<-dir(path=".",include.dirs=TRUE,all.files =TRUE,pattern = "ANN",full.names = TRUE)
length(name_dir)
filenames<-NA

for (i in seq(1,length(name_dir),by=2)){
  filenames<-c(filenames,name_dir[i])
}

filenames<-filenames[2:(length(name_dir)/2+1)]
RESULT_MJ <- lapply(filenames, read.ENVI)


filenames<-name_dir[c(1,2,5,6,7,8,9,10)]
RESULT_MJ <-  lapply(filenames, read.delim,header = TRUE, sep = ",")
names(RESULT_MJ) <- substr(filenames, 3, 30)
save(RESULT_MJ,file = "RESULT_MJ.RData")
load("RESULT_MJ_LCMerge.RData")
load("Carbon_ann_MJ.RData")
load("Carbon_ann_MJ_LCmerge.RData")
Carbon_ann_LC_merge<-Carbon_ann
