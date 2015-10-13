批量读取nc格式的数据（针对不同日期的数据）示例：
for year=2003:2012
     for month=1:12
           mon=num2str(month+100);
          strmon=mon(2:3);
         % To Judge How many days in a month
          if (month==1)||(month==3)||(month==5)||(month==7)||(month==8)||(month==10)||(month==12)
              day=31;
          elseif (month==4)||(month==6)||(month==9)||(month==11)
              day=30;
          elseif (mod(year,400)==0||mod(year,4)==0 && mod(year,100)~=0) && (month==2)
              day=29;
          else
              day=28;
          end
        for id=1:day
            mday=num2str(id+100);
            strday=mday(2:3);
            name=[F:\**,num2str(year),num2str(strmon),num2str(strday),'.nc'];  %按时间变化的不同数据
            out=netcdf.open(name,'nc_nowrite');
            temp=netcdf.getvar(out,要导入的变量);
            lat=netcdf.getvar(out,纬度);
            lon=netcdf.getvar(out,经度);
            ......(以下自己设计吧，如作图什么的)
         end
      end
end

-----------------------------------------------------------------------------------------------------------------------------------------------------

批量读取hdf格式的数据示例：
clc;  %清屏
clear; %清空
datadir='D:\data\降水数据\CPC Unified Gauge-Based Analysis of Daily Precipitation over CONUS\'; %指定批量数据所在的文件夹
filelist=dir([datadir,'*.nc']); %指定批量数据的类型
a=filelist(1).name; %查看你要读取的文件的编号。filelist(1).name在window下为第一个标号数据
b=filelist(2).name; %查看你要读取的文件的编号。filelist(2).name在window下为第二个标号数据
k=length(filelist);
for s=1:k
  filename=[datadir,filelist(s).name];
  ncid=netcdf.open(filename,'NC_NOWRITE');
  ncdisp('D:\data\降水数据\CPC Unified Gauge-Based Analysis of Daily Precipitation over CONUS\precip.V1.0.1948.nc'); %在命令窗中显示nc文件的变量
  %任意取其中一个来看数据中所包含的变量特征，以为下面读取数据变量做铺垫
  % ncid = netcdf.open('D:\data\降水数据\CPC Unified Gauge-Based Analysis of Daily Precipitation over CONUS\precip.V1.0.1948.nc','NOWRITE'); %打开nc文件
  % ncdisp('D:\data\降水数据\CPC Unified Gauge-Based Analysis of Daily Precipitation over CONUS\precip.V1.0.1948.nc'); %在命令窗中显示nc文件的变量
  PrecipData  = ncread(filename,'precip'); %读入变量precip
  TimeData  = ncread(filename,'time'); %读入变量time
  LonData  = ncread(filename,'lon'); %读入变量lon
  LatData  = ncread(filename,'lat'); %读入变量lat
  netcdf.close(ncid);   % 关闭文件
end;

