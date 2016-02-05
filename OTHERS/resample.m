%内存容易溢出（用命令查看内存状态feature('memstats')），解决方法：
% 1.修改虚拟内存（一般物理+虚拟需要达到2.5G以上），重启
% 2.不启动java虚拟机，matlab -nojvm
% 3.采用3GB switch(部分机子不支持)：右键我的电脑-->属性-->高级-->启动和故障修复-->设置-->编辑-->
%  最后一行multi(0)disk(0)rdisk(0)partition(1)\WINDOWS="Microsoft Windows XP Professional" 复制，
%  在下一行粘贴，并将粘贴一行改为： multi(0)disk(0)rdisk(0)partition(1)\WINDOWS="Microsoft Windows XP Professional with 3GB switche" /3gb 
%  -->存储-->确定-->重新启动计算机-->在显示选择操作系统时，选择3GB switche项-->打开matlab。(重启计算机，选择3GB switche若出错则该机不支持)

%内存大泽可以考虑使用A=mat2cell(str2num(cell2mat(textread('E:\Climatefuture\pre205012.txt','%s','delimiter','\n','whitespace','','bufsize',100000,'headerlines',6))))
%改法读取文件时，先以文本读入，然后将文本转为数字，不经过reshape（数字格式读入则成为一列数据，需要经过reshape转变为矩阵）

%将文件放在任意文件夹下，但是文件和文件路径不能包含中文字符，调用命令："run resample"
%首先必须在移动硬盘的路径下建立文件夹tm：H:\convert\tm
%默认为计算参数tm，如果要改变参数，则将tm替代为其它变量（包括路径也需要修改）
for i=1961:2100;
for j=1:9;
filename=['H:\rcm-1km\tm\tm',num2str(i),'0',num2str(j),'.txt']; %设定输入数据的路径文件夹
filename2=['H:\convert\tm\tm',num2str(i),'0',num2str(j),'.txt'];
data=reshape(textread(filename,'%6.2f','headerlines',6),7321,4357)';
fid = fopen(filename2,'wt');%‘a’：在打开的文件末端添加数据。文件不存在则创建。%‘a+’：打开文件后，先读入数据再添加数据。文件不存在则创建。
 a=(data(1:5:4355,1:5:7320)+data(2:5:4355,2:5:7320)+data(3:5:4355,3:5:7320)+data(4:5:4355,4:5:7320)+data(5:5:4355,5:5:7320))/5;
%自动运算矩阵行列数采样终结点，则用下式自动运算
%a=(data(1:5:(size(data,1)-mod(size(data,1),5)),1:5:(size(data,2)-mod(size(data,2),5)))+data(2:5:(size(data,1)-mod(size(data,1),5)),2:5:(size(data,2)-mod(size(data,2),5)))+data(3:5:(size(data,1)-mod(size(data,1),5)),3:5:(size(data,2)-mod(size(data,2),5)))+data(4:5:(size(data,1)-mod(size(data,1),5)),4:5:(size(data,2)-mod(size(data,2),5)))+data(5:5:(size(data,1)-mod(size(data,1),5)),5:5:(size(data,2)-mod(size(data,2),5))))/5;
clear data
fprintf(fid, '%s', 'ncols      1464');
fprintf(fid,'\n');
fprintf(fid, '%s', 'nrows      871');
fprintf(fid,'\n');
fprintf(fid, '%s', 'xllcorner  73.497955');
fprintf(fid,'\n');
fprintf(fid, '%s', 'yllcorner  17.067215');
fprintf(fid,'\n');
fprintf(fid, '%s', 'cellsize   0.042105');
fprintf(fid,'\n');
fprintf(fid, '%s', 'NODATA_value  -99.00');
fprintf(fid,'\n');
for k=1:871;
   fprintf(fid, '%6.2f ',a(k,:));
   fprintf(fid,'\n');
end;
fclose(fid);
clear a ans fid k;
end;
end;
clear ;

for i=1961:2100;
for j=10:12
filename=['H:\rcm-1km\tm\tm',num2str(i),num2str(j),'.txt'];  %设定输入数据的路径文件夹
filename2=['H:\convert\tm\tm',num2str(i),num2str(j),'.txt'];
data=reshape(textread(filename,'%6.2f','headerlines',6),7321,4357)';
fid = fopen(filename2,'wt');%‘a’：在打开的文件末端添加数据。文件不存在则创建。%‘a+’：打开文件后，先读入数据再添加数据。文件不存在则创建。
 a=(data(1:5:4355,1:5:7320)+data(2:5:4355,2:5:7320)+data(3:5:4355,3:5:7320)+data(4:5:4355,4:5:7320)+data(5:5:4355,5:5:7320))/5;
clear data
fprintf(fid, '%s', 'ncols      1464');
fprintf(fid,'\n');
fprintf(fid, '%s', 'nrows      871');
fprintf(fid,'\n');
fprintf(fid, '%s', 'xllcorner  73.497955');
fprintf(fid,'\n');
fprintf(fid, '%s', 'yllcorner  17.067215');
fprintf(fid,'\n');
fprintf(fid, '%s', 'cellsize   0.042105');
fprintf(fid,'\n');
fprintf(fid, '%s', 'NODATA_value  -99.00');
fprintf(fid,'\n');
for k=1:871;
   fprintf(fid, '%6.2f ',a(k,:));
   fprintf(fid,'\n');
end;
fclose(fid);
clear a ans fid k;
end;
end;
clear ;