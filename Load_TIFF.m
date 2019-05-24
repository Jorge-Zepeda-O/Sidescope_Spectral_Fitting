function [data] = Load_TIFF(filepath, framerange, beginend)
%	Loads all frames from a *.tif or *.mptiff file and stores into a data matrix.
% 	-----------------------------------------------------------------------------	
% 	Arugment Definitions:
% 	> filepath:		"x" The path of the file to be read
% 
% 	> framerange:	[#] The frames (in order) to look at
% 					(i,j) The beginning and end frames (requires beginend = true)
%					(~) The default will take all available frames, ascending
% 
% 	> beginend:		(T/F) if framerange has only two elements, interprets those two 
%		elements as the beginning and end of the framerange (or if improperly 
%		ordered, as the end and beginning, respectively.)
%					(~) The default will only select the frames given
% 	-----------------------------------------------------------------------------	 
% 	Outputs:
% 	< data:			[x,y,f] The frame data stored in filepath.
	
	%% Argument Defaults %%
	if(nargin < 2), framerange = 0;	end		% By default, get all the frames %
	if(nargin < 3), beginend = false; end	% By default, only select frames given %

	%% Initialize %%
	% Determine the number of available frames to read from %
	info = imfinfo(filepath);
	numframes = numel(info);
	
	% Make the frng (framerange) array %
	if(framerange == 0)
		% Obtain all frames from the image %
		frng = 1:numframes;
	elseif(length(framerange) == 2 && beginend)
		% The frames specified are beginning & end %
		frng = framerange(1):framerange(2);
		
		% Check to make sure that they're within the bounds %
		if(any(framerange > numframes) || any(framerange < 1))
			% Truncate to the ends of the file %
			disp(['Selected frames are out of bounds for this file.  ', ...
				'Truncating to the ends of the file...']);
			
			frng = max(min(frng), 1) : min(max(frng), numframes);
		end
		
		% Determine if the frames are to be read backwards or not. If so, flip frng %
		if(framerange(1) > framerange(2)), frng = flip(frng); end
	else
		% Pick the specific frames listed, in that order, even if only one %
		frng = framerange;
		
		% Check if there's anything out of bounds and remove them %
		frng(any(frng > numframes) || any(frng < 1)) = [];
	end
	
	% Allocate the appropriate memory for the data %
	data = zeros(info(1).Height, info(1).Width, length(frng));
	
	%% Extract the image frame by frame %%
	for f = 1:length(frng)
		% We have to put it in a loop because MATLAB can't read .tiff files from a
		% vector array of frame numbers.  There is only a minor difference between
		% calling Load_TIFF with multiple single-frame calls versus a batch call
		data(:,:,f) = imread(filepath, frng(f), 'Info', info);
	end
end