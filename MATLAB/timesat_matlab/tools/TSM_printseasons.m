function TSM_printseasons(varargin)
% MATLAB program to read and print phenology
% data from TIMESAT v2
% 
% written by Per J\"onsson, Malm\"o University
% Lars Eklundh, Lund University

%clear all
format short
clc
disp('------------------------------------------------------------------------')
disp('  TSM_printseasons                                   ')
disp('  Reads the output file from the TIMESAT program')
disp('  and prints seasonality data')
TS_version_inc

message = 'Go to the command window to complete the dialogue';
msgbox(message,'Notice','warn','modal')

fs = filesep; % file separator character

%Check if data file is parsed to the command
if nargin == 1
    filename = varargin{1};
elseif nargin > 1
    disp('Too many input arguments');
    disp(' ')
    disp('Leaving TSM_printseasons')
    return
else
    filename = input('  Give name of seasonality file:  ','s');
end

% Check if file exists

if exist(filename,'file') < 2 %if the file does not exist
   message = 'Input seasonality file not found';
   msgbox(message,'Error executing command','error')
   disp(' ')
   disp('Leaving TSM_printseasons')
   return
end

%% Check if ndx-file exists, otherwise call TSM_index
[pathstr, fname, ext] = fileparts(filename);
if ~isempty(pathstr)
    ndxfile = [pathstr fs fname '.ndx'];
else
    ndxfile = [fname '.ndx'];
end

%get time of data file
s=dir(filename);
filetime=s.datenum;

if exist(ndxfile,'file') == 2 %if the file exists
    s=dir(ndxfile); %Check its time
    ndxtime=s.datenum;
    if ndxtime < filetime  % if the index file is older than the data file
        TSM_index(filename)
    end
else % if the index file does not exist
   TSM_index(filename)
end

%% open file for reading and display header information
fp = fopen(filename,'r');    
n = fread(fp,[1,6],'int32');

fprintf('  Data window in file \n')
fprintf('  ------------------- \n')
fprintf('  Rows    : %u - %u \n',n(3),n(4))
fprintf('  Columns : %u - %u \n',n(5),n(6))
disp(' ')
disp('  Now enter the window you wish to display data for')

rcbound(1) = input('  First row: ');
rcbound(2) = input('  Last row: ');
rcbound(3) = input('  First column: ');
rcbound(4) = input('  Last column: ');

%rcbound = input('  Ouput parameters for pixels between row1, row2, col1 and col2 \n');
disp(' ')
%Get output file; check if output is to screen or file; 
outfile = input('  Name of output text file (hit Enter to print to screen): ','s');
if isempty(outfile)
    printtofile = 0;
else
    printtofile = 1;
    fido = fopen(outfile, 'w');
    if fido < 0
        message=['File ' outfile ' could not be opened - leaving TSM_printseasons'];
        disp('message')
        return
    end
end

%Locate byte position for pixels to be processed
indexlist_tpa = locate(filename, rcbound);

n = length(indexlist_tpa);

if printtofile== 1
    fprintf(fido,'  Row   Col.  Seas.  Beg.    End.   Length   Base   Mid-x   Max.    Amp.    L-der.  R-der.  L-integ.  S-integ. \n');
end

for i = 1:n
  fseek(fp,indexlist_tpa(i)-1,-1);  
  rc = fread(fp,[1,3],'int32');
  for i = 1:rc(3)
    phenology(i,:) = fread(fp,[1,11],'real*4');
  end
  
  if printtofile == 1
     for i = 1:rc(3)
       fprintf(fido,'%5u %5u %5u %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %9.1f %9.1f \n', ...
       rc(1),rc(2), i, ...
       phenology(i,1),phenology(i,2),phenology(i,3),phenology(i,4),phenology(i,5),phenology(i,6), ...
       phenology(i,7),phenology(i,8),phenology(i,9),phenology(i,10),phenology(i,11));
     end

  else
     disp(' ')
     fprintf('  Row, Column:  %u %u \n',rc(1),rc(2))
     for i = 1:rc(3)
       fprintf('Data for season %u \n',i)
       fprintf('   Beg.    End.   Length   Base   Mid-x   Max.    Amp.    L-der.  R-der.  L-integ.  S-integ. \n')
       fprintf('%7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %7.1f %9.1f %9.1f \n', ...
       phenology(i,1),phenology(i,2),phenology(i,3),phenology(i,4),phenology(i,5),phenology(i,6), ...
       phenology(i,7),phenology(i,8),phenology(i,9),phenology(i,10),phenology(i,11))
     end
     disp('Hit enter to continue')
     pause
  end
end

fclose(fp);
if printtofile == 1
   fclose(fido);
end
disp(' ')
disp('Leaving TSM_printseasons')


function indexlist_tpa = locate(tpafile, rcbound);
%finds first and last byte number for data within rcbound window

fs = filesep; % file separator character

[pathstr, filename, ext] = fileparts(tpafile);
if ~isempty(pathstr)
    ndxfile = [pathstr fs filename '.ndx'];
else
    ndxfile = [filename '.ndx'];
end

%open index file
fid = fopen(ndxfile,'r');
%read index data into A (col row n index)
A=fread(fid,[4,inf],'int64')';
indx = find(A(:,1)>=rcbound(1) & A(:,1)<=rcbound(2) & ...
    A(:,2)>=rcbound(3) & A(:,2)<=rcbound(4));
indexlist_tpa=A(indx,4);

fclose(fid);
return
