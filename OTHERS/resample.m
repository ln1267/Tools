%�ڴ����������������鿴�ڴ�״̬feature('memstats')�������������
% 1.�޸������ڴ棨һ������+������Ҫ�ﵽ2.5G���ϣ�������
% 2.������java�������matlab -nojvm
% 3.����3GB switch(���ֻ��Ӳ�֧��)���Ҽ��ҵĵ���-->����-->�߼�-->�����͹����޸�-->����-->�༭-->
%  ���һ��multi(0)disk(0)rdisk(0)partition(1)\WINDOWS="Microsoft Windows XP Professional" ���ƣ�
%  ����һ��ճ��������ճ��һ�и�Ϊ�� multi(0)disk(0)rdisk(0)partition(1)\WINDOWS="Microsoft Windows XP Professional with 3GB switche" /3gb 
%  -->�洢-->ȷ��-->�������������-->����ʾѡ�����ϵͳʱ��ѡ��3GB switche��-->��matlab��(�����������ѡ��3GB switche��������û���֧��)

%�ڴ������Կ���ʹ��A=mat2cell(str2num(cell2mat(textread('E:\Climatefuture\pre205012.txt','%s','delimiter','\n','whitespace','','bufsize',100000,'headerlines',6))))
%�ķ���ȡ�ļ�ʱ�������ı����룬Ȼ���ı�תΪ���֣�������reshape�����ָ�ʽ�������Ϊһ�����ݣ���Ҫ����reshapeת��Ϊ����

%���ļ����������ļ����£������ļ����ļ�·�����ܰ��������ַ����������"run resample"
%���ȱ������ƶ�Ӳ�̵�·���½����ļ���tm��H:\convert\tm
%Ĭ��Ϊ�������tm�����Ҫ�ı��������tm���Ϊ��������������·��Ҳ��Ҫ�޸ģ�
for i=1961:2100;
for j=1:9;
filename=['H:\rcm-1km\tm\tm',num2str(i),'0',num2str(j),'.txt']; %�趨�������ݵ�·���ļ���
filename2=['H:\convert\tm\tm',num2str(i),'0',num2str(j),'.txt'];
data=reshape(textread(filename,'%6.2f','headerlines',6),7321,4357)';
fid = fopen(filename2,'wt');%��a�����ڴ򿪵��ļ�ĩ��������ݡ��ļ��������򴴽���%��a+�������ļ����ȶ���������������ݡ��ļ��������򴴽���
 a=(data(1:5:4355,1:5:7320)+data(2:5:4355,2:5:7320)+data(3:5:4355,3:5:7320)+data(4:5:4355,4:5:7320)+data(5:5:4355,5:5:7320))/5;
%�Զ�������������������ս�㣬������ʽ�Զ�����
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
filename=['H:\rcm-1km\tm\tm',num2str(i),num2str(j),'.txt'];  %�趨�������ݵ�·���ļ���
filename2=['H:\convert\tm\tm',num2str(i),num2str(j),'.txt'];
data=reshape(textread(filename,'%6.2f','headerlines',6),7321,4357)';
fid = fopen(filename2,'wt');%��a�����ڴ򿪵��ļ�ĩ��������ݡ��ļ��������򴴽���%��a+�������ļ����ȶ���������������ݡ��ļ��������򴴽���
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