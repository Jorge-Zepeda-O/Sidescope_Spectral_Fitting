classdef Frame < handle
% A class for things dealing with frames in a .tiff image 

%% STATIC VARIABLES %%
%	> actidx:	[#] Array of indicies of the currently active/selected frames.  This 
%		formalism allows for multiple frames to be selected at once.
%
%	> winval:	[#] Array of numbers corresponding to the values of the window
%		parameter sliders in MainWin (win_rad, win_sig)
methods(Static)
	function [value] = actidx(val, write)	% Active Index %
		persistent actidx;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)					% Spit out the entire array %
			value = actidx;
		elseif(nargin < 2 || ~write)	% Read out the specific index given by val % 
			value = actidx(val);
		elseif(write)					% Write to the value
			actidx = val;
		end
	end
	function [value] = winval(val, write)	% Window Values %
		persistent winval;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)				% Spit out the entire array %
			value = winval;
		elseif(nargin == 1)			% Read out the specific index given by val % 
			value = winval(val);
		else
			% Check if 'write' is a boolean %
			if(islogical(write))	% Write to the whole array %
				winval = val;
			else					% Write to just the indices given in 'write' %
				prev = Frame.winval;
				prev(write) = val;
				winval = prev;
			end
		end
	end
	
	%% REFRESH %%
	function REFRESH()
	% Clears all static variables of their values and prepares the class for use
	
		%% Refresh Indicies %%
		Frame.actidx([], true);		% Active indices %
	
		%% Refresh Values %%
		Frame.winval([], true);		% Window Values %
	end
end

properties
	isActive	% (bool)	Determines if this Frame is currently selected or not %
	
	%% File Handling %%
	file_folder	% ("str")	The location of the file %
	file_name	% ("str")	The name of the file (including extension) %
	
	%% Thresholds %%
	thr_det		% (#)	The detection threshold %
	thr_sig		% (#)	The signal threshold %
	
	%% Images %%
	img			% [[x,y]] The image to analyze %
	img_det		% [[x,y]] The image to analyze, but everything below 'thr_det' = 0 %
	img_sig		% [[x,y]] The image to analyze, but everything below 'thr_sig' = 0 %
	
	%% Children %%
	Particles	% [Particle]	An array of Particles identified in 'img' %
	actPar		% [#]	An array of indicides corresponding to the active Particles
end

%% DYNAMIC METHODS %%
methods
	%% CONSTRUCTOR %%
	function [obj] = Frame(file_folder, file_name, frame_num)
	% Creates an instance of the 'Frame' class.  This loads the .tiff file given by 
	% the file path as an image.  'Particles' is not initialized until the user
	% selects a region of interest, found in 'Frame.Select'.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> file_folder:	("str") The location on the computer of this Frame
	%
	%	> file_name:	("str") The file name (and extension)
	%
	%	> frame_num:	(#) The particular frame of the .tiff file to load
	%					(~) The default will take only the first frame 
	%	------------------------------------------------------------------------
	%	Outputs:
	%	< obj:	(Frame)	 The 'Frame' object constructed
		
		%% Argument Defaults %%
		if(nargin < 3), frame_num = 1; end	% By default, just read the first frame %
	
		%% Argument Pass-ins %%
		obj.file_folder = file_folder;
		obj.file_name	= file_name;
		
		%% Initialization %%
		obj.isActive = false;	% Start every Frame as inactive %
		
		%% Loading %%
		% Load the frame given by 'frame_num' into memory %
		obj.img = Load_TIFF([file_folder, file_name], frame_num);
		
		%% Pre-Processing - Detection Limit %%
		% The detection threshold ('thr_det') is defined by the non-signal values, as
		% such, a trimmean (which removes the x% highest and lowest values) is
		% appropriate.  The signal that we are interested in is considered an
		% intensity outlier compared the background.
		obj.thr_det = trimmean(obj.img(:), 10);
		
		% Once we have the detection threshold, determine what parts of the image
		% were actually detected, and aren't just noise.  This should give us the
		% parts of the image in the slit regions - zero out the noise.
		detected = obj.img > obj.thr_det;
		obj.img_det = obj.img .* detected;
		
		%% Pre-Processing - Signal Limit %%
		% Now, determine the threshold for that which isn't noise, but is in
		% the slit region, and therefore could be considered background.  We will
		% look for the 95%+ intensity regime, due to the fact that we reject the null
		% hypothesis for p < 0.05, and hence go with the hypothesis that there is a
		% particle there.  We will use the arithmetic mean for this test.
		obj.thr_sig = mean(obj.img(detected)) + 2*std(obj.img(detected));
		
		% Once we have the signal threshold, determine what parts are signal and zero
		% out the rest.
		issignal = obj.img > obj.thr_sig;
		obj.img_sig = obj.img(issignal);
	end
	
	%% METHODS %%
	function SelectROI(this)
	% This function prompts the user to select a Region of Interest (ROI) of the
	% Frame.  This ROI is then analyzed for particles, and a series of Particle
	% objects are created and attributed to this Frame.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
	%		storing and keeping track of many different instance variables.
	
		%% Refresh %%
		% Refresh the Particles field with an empty Particle array %
		this.Particles = Particle.empty;
	
		%% User-Input %%
		% Prompt the user to select a rectangular Region of Interest (ROI) %
		roi = round(getrect(UI.axs(1)));
		
		% Locate the ROI in the stored image %
		roi_x = roi(1) + (1:roi(3));	% x pos + width %
		roi_y = roi(2) + (1:roi(4));	% y pos + height %
		roi_img = this.img(roi_y, roi_x);	% Blame MATLAB %
		
		%% Smooth ROI (SNR Boost) %%
		% Smooth the ROI image using a Gaussian filter, which is a suitable
		% approximation to the PSF of a single emitter.  We wish to fit three
		% standard deviations of our filter in the window radius.
		rad_win = Frame.winval(1);		% Chosen window radius %
		
		grid = -rad_win:rad_win;		% Create the grid for the filter %
		[xx, yy] = meshgrid(grid, grid);
		
		filt = exp(-(xx.^2 + yy.^2)/(2*(rad_win/3)^2));	% Fit three std-devs %
		filt = filt / sum(filt(:));						% Normalize %
		
		% Filter the ROI image.  We use conv2 instead of fft2/ifft2 for two reasons:
		% > The size of the filter is small
		%		This means that the process of zero-padding the filter to the size of
		%		the ROI, then taking the fft2, and ifft2 of the elementwise product
		%		is more computationally expensive than dealing with the convolution.
		%
		% > We don't have any aliasing artifacts from using the circular convolution
		%		Since the fft/ifft can be represented by a circular convolution with
		%		the DFT matrix, we will see elements from one side of the image
		%		appearing on the other.  This doesn't happen with a truncated linear
		%		convlution, which is what we will be performing.
		roi_filt = conv2(roi_img, filt, 'same');
		
		%% Particle Localization %%
		% Develop a maximum filter in x and y using forward and backward positive
		% derivatives.  Essentially, this checks that at the point in question, the
		% derivative has increased to get there, and it will decrease once it leaves,
		% hence finding a maximum.  We have to smooth the signal before we get to
		% this point, else all the noise will be picked up as maxima.
		img_dx = diff(roi_filt, 1, 1) > 0;					% Derivative filters %
		img_dy = diff(roi_filt, 1, 2) > 0;
		
		pad_dx = zeros(size(roi_filt(1,:)));				% Zero padding %
		pad_dy = zeros(size(roi_filt(:,1)));
		
		filt_dx = [pad_dx; img_dx] & [1-img_dx; pad_dx];	% Forward and Backward %
		filt_dy = [pad_dy, img_dy] & [1-img_dy, pad_dy];
		
		% We've already established what is a signal and what isn't in this.img_sig,
		% so when we select the peaks, make sure that we do it over the signal image.
		roi_sig = this.img_sig(roi_y, roi_x) .* (filt_dx & filt_dy);
		
		% Now we just have to find out where roi_sig > 0, as the filters have taken
		% care of non-peaks, and the img_sig has taken care of any points below the
		% signal threshold.  These are our peak positions.
		[peak_pos(:,2), peak_pos(:,1)] = ind2sub(size(roi_sig), find(roi_sig));
		
		% Then simply translate the peak positions to the image coordinates %
		peak_pos = peak_pos + roi(1:2);		% Once again, blame MATLAB %
		
		%% Particle Creation %%
		% This can be a time-intensive process; create a waitbar %
		wb = waitbar(0, "Finding Particles...");
		
		% For each found peak, create a child Particle %
		for p = 1:size(peak_pos, 1)
			% Update the waitbar %
			waitbar(p/size(peak_pos,1), wb, "Finding Particles...");
			
			% Slice a region of this image close to this particle.  We select pixels
			% in an entire range of y so everything related to spectrum processing 
			% can be contained in the Particle class.
			peak_yrng = (-rad_win:rad_win) + peak_pos(p,2);
			
			% Pass in the particle's peak position and the image slice %
			part = Particle(peak_pos(p,:), this.img(peak_yrng, :));
			lbx_str{p} = part.str;	% For the Listbox %
			
			% Append this particle to the Frame's particle list %
			this.Particles(p) = part;
		end
		% Close the waitbar %
		close(wb);
		
		%% MainWin Update %%
		% Activate the first particle found %
		this.actPar = 1;
		
		% Listbox updates %
		lbx = UI.FindObject(UI.ctrls, "lbx: Found Particles");	% Get the Listbox %
		lbx.String = lbx_str;		% Makes the Listbox have an item per Particle %
		lbx.Value = 1;				% Sets the current item to the first one %
		lbx.UserData.preVal = 1;	% Marks the previously selected Particle %
	end
	
	%% VISUALIZATION %%
	function DispImg(this, ax)
	% Displays the image contained in this Frame on the axes specified by 'ax'.
	%
	% Additional considerations taken:
	% + Aspect ratio is 1:1
	% + Title is ["Original Image (", this.file_name, ")"]
	% + Labeled colorbar with "Intensity (arb.)"
	% + Axes labels
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Original Image' axes 
	
		%% Argument Defaults %%
		if(nargin < 2), ax = UI.axs(1); end	% Default to the OriginalImage axes %
		
		%% Refresh %%
		cla(ax, 'reset');
		
		%% Plotting %%
		imagesc(ax, this.img);						% Plot the image %
		
		xlim(ax, [1/4, 3/4]*size(this.img, 1));		% Constrain the viewing region %
		axis(ax, 'image')							% 1:1 Aspect Ratio
		
		%% Labeling %%
		title(ax, join(["Original Image (", this.file_name, ")"]));	% Title %
		
		c = colorbar(ax);							% Colorbar %
		c.Label.String = "Intensity (arb.)";
		
		xlabel(ax, "X position (px)");				% Axes labels %
		ylabel(ax, "Y position (px)");
	end
end


end