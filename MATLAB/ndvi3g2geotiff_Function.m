% Created by Yuke Zhou
% Version 1.0
% 12 Feb, 2015

% Description:
% Convert Raw Data(GIMMS NDVI3g) to
% GeoTIFF and project to GeoCoodSystem 'WGS84?
% Output:
% A NDVI file with its real value
% A flag file indicating the piexl source

% Thanks to Li Xu. His blog http://bbs.sciencenet.cn/home.php?mod=space&uid=1148346&view=lixujeremy
% Thanks to Prof. Nan(giscn@msn.com) for inspiring.
% His code: http://nanzt.info/2797.html

% run as: ndvi3g2geotiff_Function('D:\gisData\GIMSS_NDVI\3g\1980s\', '*VI3g', 'D:\gisData\GIMSS_NDVI\3g\NDVI_Flag\')
function [] = ndvi3g2geotiff_Function( inFilePath, inFilePattern, outFilepath )

files = dir( [inFilePath, inFilePattern] );

num_files = length(files);

for i = 1:num_files
ndvi_FileName = files(i).name;
ndvi3g = [inFilePath, ndvi_FileName];
%%ndvi3g='geo82dec15a.n07-VI3g';

% Image info
col = 4320;
row = 2160;

% Read Raw Data
fid = fopen(ndvi3g,'r','b');
A=fread(fid,col*row, '*int16');
fclose(fid);
A=reshape(A,row,col);
A = A';
A = rot90(A,3);
A = fliplr(double(A));

% Define the Lat/Long range
%LonRange=[-180+1/24:1/12:180-1/24];
%LatRange=[-90+1/24:1/12:90-1/24];
% Write a Data Referenced to Geographic Coordinates
R=georasterref('RasterSize', [row, col], 'Latlim', [-90+1/24, 90-1/24],'Lonlim', [-180+1/24, 180-1/24], 'ColumnsStartFrom', 'north');

% Ouput NDVI.tif
disp('…Writing out NDVI file…');
ndvi=floor(A./10)./1000;
ndvipath=[outFilepath, ndvi_FileName, '_NDVI.tif'];
geotiffwrite(ndvipath, ndvi, R);
fprintf('%s done!\n', ndvipath);

% Output Flag.tif
disp('…Writing out Flag file…');
flag=A-floor(A./10) .* 10 +1;
flagpath=[outFilepath, ndvi_FileName, '_Flag.tif'];
geotiffwrite(flagpath, flag, R);
fprintf('%s done!\n', flagpath);

disp('END');
end
end