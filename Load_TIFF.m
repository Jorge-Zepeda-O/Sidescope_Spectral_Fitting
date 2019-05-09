function [data] = LoadTIFF(filepath, framerange, beginend)
%	Loads all frames from a *.tif or *.mptiff file and stores into a data matrix.
% 	-----------------------------------------------------------------------------	
% 	Arugment Definitions:
% 	> filepath:		"x" The path of the file to be read
% 
% 	> framerange:	[#] The frames (in order) to look at
% 					(i,j) The beginning and end frames (requires beginend = true)
% 
% 	> beginend:		(T/F) (optional) if framerange has only two elements,
% 		interprets those two elements as the beginning and end of the framerange
% 		(or if improperly ordered, as the end and beginning, respectively.)
% 	-----------------------------------------------------------------------------	 
% 	Outputs:
% 	< data:			[x,y,f] The frame data stored in filepath.
	
	%% Argument Defaults %%
	if(nargin < 3), beginend = false; end	% By default, only select frames given %

	%% Initialize %%
	% Make the frng (framerange) array %
	if(length(framerange) == 2 && beginend)
		% The frames specified are beginning & end %
		
		% Determine if the frames are to be read backwards or not %
		if(min(framerange) == framerange(1))
			% Properly ordered %
			frng = framerange(1):framerange(2); 
		else
			% Read in reverse %
			frng = framerange(2):-1:framerange(1);
		end
	else
		% Pick the specific frames listed, even if only one %
		frng = framerange;
	end
	
	% Determine the number of available frames to read from %
	info = imfinfo(filepath);
	numframes = numel(info);
	
	% Check to make sure that no frame in frng is out of bounds %
	if(any(frng > numframes) || any(frng < 1))
		error('Selected frames are out of bounds for this file');
	end
	
	% Allocate the appropriate memory for the data %
	data = zeros(info(1).Height, info(1).Width, length(frng));
	
	%% Extract the image frame by frame %%
	for f = 1:length(frng)
		data(:,:,f) = imread(filepath, frng(f), 'Info', info);
	end
end