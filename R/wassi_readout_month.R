#---------------------------
rm(list =ls()) # this is for clear all the data in memory of R

# install and libarary "caTools" which is mainly for ENVI data process, eg: read.ENVI, write.ENVI
# "plyr" is mainly used for sort data and other founctions

#install.packages("caTools") 
#install.packages("plyr")

library(caTools)

data_directory="M:/CLIMATE_RESULTS/C7/"
name_ann<-"ANNUALFLOW.TXT"
name_month<-"MONTHFLOW.TXT"

num_blocks<-1

nrows<-260
ncols<-182


num_ID<-nrows*ncols/num_blocks
name_ID<-c(1:num_ID)
year_start<-2000
year_end<-2012
num_year<-year_end-year_start+1

#num_block<-1

for (num_block in 1:num_blocks) {
  input_name_ann<-paste(data_directory,name_ann,sep='')
  input_name_month<-paste(data_directory,name_month,sep='')
  
  num_line<-num_ID*num_year
  num_line_mon<-num_ID*num_year*12
  

  if (num_block==1) {
    num_line_pre<-num_ID*num_year*(num_block-1) 
    num_line_pre_mon<-num_ID*num_year*(num_block-1)*12 
    
    data_ann<-read.delim(input_name_ann, header = TRUE, sep = ",",nrows=num_line,skip =num_line_pre )
    print("FINISH READDING ANNUAL DATA")
    data_month<-read.delim(input_name_month, header = TRUE, sep = ",",nrows=num_line_mon,skip =num_line_pre_mon )
    print("FINISH READDING MONTHLY DATA")
  } else {
    num_line_pre<-num_ID*num_year*(num_block-1)+1
    data_ann<-read.csv(input_name_ann, header = FALSE, sep = ",", nrows=num_line,skip =num_line_pre )
    print("FINISH READDING ANNUAL DATA")
    data_month<-read.csv(input_name_month, header = FALSE, sep = ",",nrows=num_line_mon,skip =num_line_pre_mon )
    print("FINISH READDING MONTHLY DATA")
  }
  #-------------------new method
  num_vars_ann<-12
  num_vars_mon<-12
  data_vars_ann<-array(0,c(nrows,ncols,num_year,num_vars_ann))
  data_vars_mon<-array(0,c(nrows,ncols,num_year*12,num_vars_mon))
  
  for (var in 1:num_vars_ann){
    
      for (j in 1:ncols){
    
        for (i in 1:nrows){
      
          num_start_ann<-((j-1)*nrows+i-1)*num_year+1
          num_end_ann<-((j-1)*nrows+i)*num_year
          data_vars_ann[i,j,,var]<-data_ann[num_start_ann:num_end_ann,var]
#          print(num_start_ann)
 #         print(num_end_ann)
         # print(num_start_ann)
         num_start_mon<-((j-1)*nrows+i-1)*num_year*12+1
          num_end_mon<-((j-1)*nrows+i)*num_year*12
          data_vars_mon[i,j,,var]<-data_month[num_start_mon:num_end_mon,var]  
      
        }
    
      }
    print(var)
  }
    
#----------------------------------------------------
    for (n_var in 1:num_vars_ann ){
    output_name_ann<-paste(data_directory,colnames(data_ann)[n_var],"_ANN","_",as.character(num_block),sep='')
    write.ENVI(data_vars_ann[,,,n_var],output_name_ann)
    output_name_mon<-paste(data_directory,colnames(data_month)[n_var],"_MON","_",as.character(num_block),sep='')
    write.ENVI(data_vars_mon[,,,n_var],output_name_mon)
  }
  print("FINISH OUTPUTTING  DATA")
}  
#----------------------------------------------------
  
name_tar<-"M:/CLIMATE_RESULTS/ENVI/data_tar"

data_tar<-read.ENVI(name_tar)
nrows<-nrow(data_tar)
ncols<-ncol(data_tar)
num_brands_ann<-num_year
num_brands_mon<-num_year*12
name_row_ann<-c(year_start:year_end)
name_row_mon_ann<-rep(name_row_ann,each=12)
name_row_mon_mon<-rep(1:12,num_year)


n_var<-7
n_var_mon<-11
#------------only for several vegs
N_vegs<-16
data_sta_ann_vegs<-matrix(0,nrow=num_brands_ann,ncol=N_vegs)
data_sta_mon_vegs<-matrix(0,nrow=num_brands_mon,ncol=N_vegs)
for (m in 1:N_vegs){
  
  data_linshi<-data_vars_ann[,,,n_var]  # data_linshi is used for transfering origin array for getting data by each basin
  data_linshi[data_tar[,,4]!=m]<-NA
  data_sta_ann_vegs[,m]<-apply(data_linshi,3, mean,na.rm=TRUE)
  
  data_linshi<-data_vars_mon[,,,n_var_mon]  # data_linshi is used for transfering origin array for getting data by each basin
  data_linshi[data_tar[,,4]!=m]<-NA
  data_sta_mon_vegs[,m]<-apply(data_linshi,3, mean,na.rm=TRUE)
  #  print(m)
}

out_path<-"M:/CLIMATE_RESULTS/C7/C7_VEG.csv"
write.table(data.frame(name_row_ann,data_sta_ann_vegs),file = out_path, col.names = c("Year",paste("VEG_",c(1:N_vegs),sep = "")),row.names=FALSE,sep = ",")
out_path<-"M:/CLIMATE_RESULTS/C7/C7_VEG_MON.csv"
write.table(data.frame(name_row_mon_ann,name_row_mon_mon,data_sta_mon_vegs),file = out_path, col.names = c("Year","MONTH",paste("VEG_",c(1:N_vegs),sep = "")),row.names=FALSE,sep = ",")

#------------Only for several basins
N_basins<-13

data_sta_ann_basins<-matrix(0,nrow=num_brands_ann,ncol=N_basins)
data_sta_mon_basins<-matrix(0,nrow=num_brands_mon,ncol=N_basins)
for (m in 1:N_basins){
  
  data_linshi<-data_vars_ann[,,,n_var]  # data_linshi is used for transfering origin array for getting data by each basin
  data_linshi[data_tar[,,1]!=m]<-NA
  data_sta_ann_basins[,m]<-apply(data_linshi,3, mean,na.rm=TRUE)
  
  data_linshi<-data_vars_mon[,,,n_var_mon]  # data_linshi is used for transfering origin array for getting data by each basin
  data_linshi[data_tar[,,1]!=m]<-NA
  data_sta_mon_basins[,m]<-apply(data_linshi,3, mean,na.rm=TRUE)
  
  # print(m)
}

out_path<-"M:/CLIMATE_RESULTS/C7/C7_BASIN_ann.csv"
write.table(data.frame(name_row_ann,data_sta_ann_basins),file = out_path, col.names = c("Year",paste("BASIN_",c(1:N_basins),sep = "")),row.names=FALSE,sep = ",")
out_path<-"M:/CLIMATE_RESULTS/C7/C7_BASIN_mon.csv"
write.table(data.frame(name_row_mon_ann,name_row_mon_mon,data_sta_mon_basins),file = out_path, col.names =c("Year","MONTH",paste("Basin_",c(1:N_basins),sep = "")),row.names=FALSE,sep = ",")



# #------------ for several basins and vegs
# data_sta_basin_vegs<-array(0,c(num_brands,N_basins,N_vegs))
# 
# for (m in 1:N_basins){
#   for (n in 1:N_vegs){
#     data_linshi<-data_ENVI  # data_linshi is used for transfering origin array for getting data by each basin
#     data_linshi[data_tar[,,1]!=m]<-NA
#     data_linshi[data_tar[,,4]!=n]<-NA
#     data_sta_basin_vegs[,m,n]<-apply(data_linshi,3, mean,na.rm=TRUE)
#     
#     # print(n)
#   } 
#   out_path<-paste("M:/CLIMATE_RESULTS/ENVI/C7_BASIN_",m,".csv",sep='')
#   write.table(data_sta_basin_vegs[,m,],file = out_path, col.names = TRUE,row.names=name_row,sep = ",")
#   # print(m)
# }

