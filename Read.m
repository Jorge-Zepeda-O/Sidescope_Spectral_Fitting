classdef Read < handle
% A static class that contains functions for reading in images or movies

methods(Static)
	function Image(parent, frames)	
	% A function that prompts the user for an image to load, then loads it in and
	% displays on the parent figure
	
		%% Argument Defaults %%
		if(nargin < 2), frames = 1; end
		
		%% Initialize %%
		% Prompt the user to locate a file %
		[filename, filefolder] = uigetfile({'*.tif', '*.mptiff'});
			
		% Concatenate the file name and folder into the appropriate path %
		filepath = [filefolder, filename];

		% Read the .tif / .mptiff file %
		img = Load_TIFF(filepath, frames);
		
		%% Pre-processing %%
		% Anything that's below the mean (which should be low) is a flat 0 %
		img = img .* (img > mean(img(:)));
		
		% Clear out the previous fields %
		parent.UserData.Particles = [];
		parent.UserData.use_evs = false;
		parent.UserData.specplt = false;
		
		%% Display %%
		% Obtain the axes where the image should go in the parent figure %
		ax_img = parent.UserData.ax_img;
		
		% Plot the image to these axes %
		Show.Image(ax_img, img, "Original Image");
		
		% Update the appropriate fields in the parent figure %
		parent.UserData.img = img;
		parent.UserData.ax_img = ax_img;
		
		%% MainWin Update %%
		% Make the 'Zoom Image', 'Pan Image', & 'Select Particles' buttons enabled %
		but_zoomimg = findobj(parent.Children, 'flat', 'Tag', 'Zoom Image');
		but_zoomimg.Enable = 'on';
		
		but_panimg = findobj(parent.Children, 'flat', 'Tag', 'Pan Image');
		but_panimg.Enable = 'on';
		
		but_selpart = findobj(parent.Children, 'flat', 'Tag', 'Select Particles');
		but_selpart.Enable = 'on';
		
		% Make the 'Fit Selected Particles' button and 'Found Particles' listbox 
		% disabled, and clear them while you're at it.
		but_fitpart = findobj(parent.Children, 'flat', 'Tag', 'Fit Selected Particles');
		but_fitpart.Enable = 'off';
		
		lbx_fndpart = findobj(parent.Children, 'flat', 'Tag', 'lbx: Found Particles');
		lbx_fndpart.Enable = 'off';
		lbx_fndpart.String = {};
	end
end
	
end