%function [image,p,t]=freadenvi('I:\AU\GAC\annual');

% freadenvi           - read envi image (V. Guissard, Apr 29 2004)
%
%     Reads an image of ENVI standard type
%    to a [col x line x band] MATLAB array
% image=freadenvi(fname)
% [image,p]=freadenvi(fname)
% [image,p,t]=freadenvi(fname)
%
% INPUT :
%
% fname string giving the full pathname of the ENVI image to read.
%
% OUTPUT :
%
% image----------- c by l by b array containing the ENVI image values organised in
%    c : cols, l : lines and b : bands.
% p  1 by 3 vector that contains (1) the nb of cols, (2) the number.
%    of lines and (3) the number of bands of the opened image.
%
% t  string describing the image data type string in MATLAB conventions.
%
% NOTE :    freadenvi needs the corresponding image header file generated
%    automatically by ENVI. The ENVI header file must have the same name
%    as the ENVI image file + the '.hdf' exention.
%
%%%%%%%%%%%%%
clear
fname='F:\NDVI\NDVI-11-4-N1'
fnamex='F:\RAIN\RAIN-11-4-N'

% Parameters initialization
elements={'samples ' 'lines   ' 'bands   ' 'data type '};
d={'bit8' 'int16' 'int32' 'float32' 'float64' 'uint16' 'uint32' 'int64' 'uint64'};
% Check user input
if ~ischar(fnamey)
    error('fname should be a char string');
end

if ~ischar(fnamex)
    error('fname should be a char string');
end

% Open ENVI header file to retreive s, l, b & d variables

rfidy = fopen(strcat(fnamey,'.hdr'),'r');
rfidx = fopen(strcat(fnamex,'.hdr'),'r');

% Check if the header file is correctely open
if rfidy == -1
    error('Input header file does not exist');
end;

if rfidx == -1
    error('Input header file does not exist');
end;

% Read ENVI image header file and get p(1) : nb samples,
% p(2) : nb lines, p(3) : nb bands and t : data type
while 1
    tline = fgetl(rfidx);
    if ~ischar(tline), break, end
    [first,second]=strtok(tline,'=');
   
    switch first
        case elements(1)
            [f,s]=strtok(second);
            py(1)=str2num(s);
        case elements(2)
            [f,s]=strtok(second);
            py(2)=str2num(s);
        case elements(3)
            [f,s]=strtok(second);
            py(3)=str2num(s);
        case elements(4)
            [f,s]=strtok(second);
            ty=str2num(s);
            switch ty
                case 1
                    ty=d(1);
                case 2
                    ty=d(2);
                case 3
                    ty=d(3);
                case 4
                    ty=d(4);
                case 5
                    ty=d(5);
                case 12
                    ty=d(6);
                case 13
                    ty=d(7);
                case 14
                    ty=d(8);
                case 15
                    ty=d(9);
                otherwise
                    error('Unknown image data type');
            end
    end
end

while 1
    tline = fgetl(rfidx);
    if ~ischar(tline), break, end
    [firstx,secondy]=strtok(tline,'=');
   
    switch firstx
        case elements(1)
            [f,s]=strtok(second);
            px(1)=str2num(s);
        case elements(2)
            [f,s]=strtok(second);
            px(2)=str2num(s);
        case elements(3)
            [f,s]=strtok(second);
            px(3)=str2num(s);
        case elements(4)
            [f,s]=strtok(second);
            tx=str2num(s);
            switch tx
                case 1
                    tx=d(1);
                case 2
                    tx=d(2);
                case 3
                    tx=d(3);
                case 4
                    tx=d(4);
                case 5
                    tx=d(5);
                case 12
                    tx=d(6);
                case 13
                    tx=d(7);
                case 14
                    tx=d(8);
                case 15
                    tx=d(9);
                otherwise
                    error('Unknown image data type');
            end
    end
end

fclose(rfidy);
fclose(rfidx);

ty=ty{1,1};
tx=tx{1,1};
% Open the ENVI image and store it in the 'image' MATLAB array

disp([('Opening '),(num2str(py(1))),('cols x '),(num2str(py(2))),('lines x '),(num2str(py(3))),('bands')]);
disp([('of type '), (ty), (' image...')]);
fidy=fopen(fnamey);
imagey=fread(fidy,ty);
imagey=reshape(imagey,[py(1),py(2),py(3)]);
fclose(fidy);

disp([('Opening '),(num2str(px(1))),('cols x '),(num2str(px(2))),('lines x '),(num2str(px(3))),('bands')]);
disp([('of type '), (tx), (' image...')]);
fidx=fopen(fnamex);
imagex=fread(fidx,tx);
imagex=reshape(imagex,[px(1),px(2),px(3)]);
fclose(fidx);
% % zhuanzhi
% for a=1:p(1)
% for b=1:p(2)
% for c=1:p(3)
% 
% image0(a,b,p(3)-c+1)=image(a,b,c);
% 
% end
% end
% end



by=zeros(p(3),1);
bx=zeros(p(3)),1;
br=zeros(p(1),p(2));
bp=zeros(p(1),p(2));

for m=1:p(1)
    for n=1:p(2)
        for c=1:p(3)
           by(c)=imagey(m,n,c);
           bx(c)=imagex(m,n,c);
        end
		xx=[ones(size(bx)) bx]
        [b,bint,r,rint,stats] = regress(by,xx,alpha);  
        br(m,n)=stats(1);
        bp(m,n)=stats(3);
    end
end

% for c=1:10
% b2(:,:,c)=b2(:,:,c)';
% end
disp([('finish')]);

% save f:\NDVI\n-11-4-1500.dat b2 -ascii
load chirp
wavplay(y,Fs)