 function TSM_viewfits
% function TSM_viewfits(varargin)
% MATLAB program to view fits from TIMESAT 
% 
% written by Per J\"onsson, Malm\"o University
% Lars Eklundh, Lund University

% open file for reading

%clear all
clc
figH=[999];
disp('------------------------------------------------------------------------')
disp('  TSM_viewfits                                       ')
disp('  Reads the output function files from the TIMESAT program')
disp('  and displays fitted functions and original time-series')
TS_version_inc

message = 'Go to the command window to complete the dialogue';
msgbox(message,'Notice','warn','modal')

  
  df = input('  Display fitted functions?  (1/0)?  ');
  dr = input('  Display original time-series?  (1/0)?  ');
  if df == 0 && dr == 0
      disp('  Nothing to do... leaving TSM_viewfits')
      return
  end
  if df == 1
    filenamef = input('  Give name of input fitted data file:  ','s');
    % Check if file exists
    if exist(filenamef,'file') < 2 %if the file does not exist
      message = 'Input fitted data file not found';
      msgbox(message,'Error executing command','error','modal')
      if ishandle(figH)
         delete(figH)
      end
      disp(' ')
      disp('  Leaving TSM_viewfits')
      return
    end
  end
  if dr == 1
    filenamer = input('  Give name of input raw data file:  ','s');
    if exist(filenamer,'file') < 2 %if the file does not exist
      message = 'Input raw data file not found';
      msgbox(message,'Error executing command','error','modal')
      if ishandle(figH)
        delete(figH)
      end
      disp(' ')
      disp('  Leaving TSM_viewfits')
      return
    end
  end
%end

disp(' ')
if df == 1
  
  %Check if ndx-file exists, otherwise call TSM_index
  [pathstr, fname, ext] = fileparts(filenamef);
  if ~isempty(pathstr)
     ndxfile = [pathstr fs fname '.ndx'];
  else
     ndxfile = [fname '.ndx'];
  end
 
  %get time of data file
    s=dir(filenamef);
    filetime=s.datenum;

    if exist(ndxfile,'file') == 2 %if the file exists
        s=dir(ndxfile); %Check its time
        ndxtime=s.datenum;
        if ndxtime < filetime  % if the index file is older than the data file
            TSM_index(filenamef)
        end
    else % if the index file does not exist
       TSM_index(filenamef)
    end
  
  % Open fitted function data file    
  fpf = fopen(filenamef,'r');
  %read header
  nf = fread(fpf,[1,6],'int32');  
  nptf = nf(1)*nf(2);
  n = nf;
  npt = nptf;
end

if dr == 1
  %Check if ndx-file exists, otherwise call TSM_index
  [pathstr, fname, ext] = fileparts(filenamer);
  if ~isempty(pathstr)
     ndxfile = [pathstr fs fname '.ndx'];
  else
     ndxfile = [fname '.ndx'];
  end
  
  %get time of data file
    s=dir(filenamer);
    filetime=s.datenum;

    if exist(ndxfile,'file') == 2 %if the file exists
        s=dir(ndxfile); %Check its time
        ndxtime=s.datenum;
        if ndxtime < filetime  % if the index file is older than the data file
            TSM_index(filenamer)
        end
    else % if the index file does not exist
       TSM_index(filenamer)
    end
  
%  open raw data file
  fpr = fopen(filenamer,'r');
  %read header
  nr = fread(fpr,[1,6],'int32');  
  nptr = nr(1)*nr(2);
  n = nr;
  npt = nptr;
end
if df == 1 & dr == 1
  if nf ~= nr
    message = 'Files not compatible'
    return
  end
end

fprintf(' \n')
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
%rcbound = input('  View fits between row1, row2, col1 and col2 \n');
%read data

figH = figure('Tag','Viewfits','Name','TSM_viewfits','NumberTitle','off');
%Define a handle to a function that operates when figure is closed; parse
%   figH
set(figH,'CloseRequestFcn',{@my_closefcn,figH})

%Find byte locations data pixels within window
if df == 1
   indexlistdf = locate(filenamef, rcbound);
   n=length(indexlistdf);
end
if dr == 1
   indexlistdr = locate(filenamer, rcbound);
   n=length(indexlistdr);
end 

if dr == 1 && dr ==1
    if indexlistdr ~= indexlistdf
      message = 'Raw and fitted files not equal';
      msgbox(message,'Error executing command','error','modal')
      if ishandle(figH)
        delete(figH)
      end
      disp(' ')
      disp('  Leaving TSM_viewfits')
      return
    end
end

for i = 1:n    %For all the pixels found
  figure(figH)
  set(figH,'Tag','Viewfits','Name','TSM_viewfits','NumberTitle','off');
  %Define a handle to a function that operates when figure is closed; parse
  %   figH
  set(figH,'CloseRequestFcn',{@my_closefcn,figH})
  clf
      
  if df == 1
      fseek(fpf,indexlistdf(i)-1,-1);
      rc = fread(fpf,[1,2],'int32');
      y1 = fread(fpf,[1,npt],'real*4');  
      fprintf('  Displaying fitted function for row: %u column %u \n',rc(1),rc(2))
      %Plot the data
      plot(1:npt,y1,'r'), hold on
      xlabel('time'), ylabel('data')
      title(sprintf('Time-series for row %u, column %u ',rc(1),rc(2)))
      xlim([1 npt])
  end
  
  if dr == 1
      fseek(fpr,indexlistdr(i)-1,-1);
      rc = fread(fpr,[1,2],'int32');
      y1 = fread(fpr,[1,npt],'real*4');
      fprintf('  Displaying original time-series for row: %u column %u \n',rc(1),rc(2))
      %Plot the data
      plot(1:npt,y1,'b'), hold on
      xlabel('time'), ylabel('data')
      title(sprintf('Time-series for row %u, column %u ',rc(1),rc(2)))
      xlim([1 npt])
  end  
  disp('  Hit enter to continue')
  pause
end

if df == 1
   fclose(fpf);
end
if dr == 1
   fclose(fpr);
end
disp(' ')
disp('  Leaving TSM_viewfits')
delete(figH)

function my_closefcn(src,evnt,figH)
message = 'TSM_viewfits might still be running - please continue or abort with ctrl-C';
msgbox(message,'Warning','warn','modal')
if ishandle(figH)
   delete(figH)
end
return

function indexlist = locate(ttsfile, rcbound);
%finds first and last byte number for data within rcbound window

fs = filesep; % file separator character

[pathstr, filename, ext] = fileparts(ttsfile);
if ~isempty(pathstr)
    ndxfile = [pathstr fs filename '.ndx'];
else
    ndxfile = [filename '.ndx'];
end

%open index file
fid = fopen(ndxfile,'r');
%read index data into A (col row index)
A=fread(fid,[3,inf],'int64')';
indx = find(A(:,1)>=rcbound(1) & A(:,1)<=rcbound(2) & ...
    A(:,2)>=rcbound(3) & A(:,2)<=rcbound(4));
indexlist=A(indx,3);

fclose(fid);
return

