classdef Frame < handle
% A class for things dealing with frames in a .tiff image 

%% CONSTANT VARIABLES %%
properties(Constant)
	MAGNIFICATION = 40
    PIXEL_SIZE = 11     % um
    SBAR_PX = 10 / (Frame.PIXEL_SIZE / Frame.MAGNIFICATION)
end

%% STATIC VARIABLES %%
%	> actidx:	[#] Array of indicies of the currently active/selected frames.  This 
%		formalism allows for multiple frames to be selected at once.
%
%	> winval:	[#] Array of numbers corresponding to the values of the window
%		parameter sliders in MainWin (win_rad, win_sig)
methods(Static)
	function [value] = opmode(val, write)	% Operation Mode %
		persistent opmode;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)				% Spit out the entire array %
			value = opmode;
		elseif(nargin == 1)			% Read out the specific index given by val % 
			value = opmode(val);
		else
			% Check if 'write' is a boolean %
			if(islogical(write))	% Write to the whole array %
				opmode = val;
			else					% Write to just the indices given in 'write' %
				prev = Frame.opmode;
				prev(write) = val;
				opmode = prev;
			end
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
	
	%% REFRESH %%
	function REFRESH()
	% Clears all static variables of their values and prepares the class for use
	
		%% Refresh Options %%
		Frame.opmode([], true);		% Operation Mode %
	
		%% Refresh Values %%
		Frame.winval([], true);		% Window Values %
		Frame.actidx([], true);		% Active Index %
	end
end

%% DYNAMIC VARIABLES %%
properties
	%% File Handling %%
	file_folder	% ("str")	The location of the file
	file_name	% ("str")	The name of the file (including extension)
	
	%% Thresholds %%
	thr_det		% (#)	The detection threshold
	thr_sig		% (#)	The signal threshold
	
	%% Images %%
	img			% [[x,y]] The image to analyze %
	img_det		% [[x,y]] The image to analyze, but everything below 'thr_det' = 0
	img_sig		% [[x,y]] The image to analyze, but everything below 'thr_sig' = 0
	
	%% Spectrum %%
	spec_bg		% [#]	An array of values corresponding to the background spectrum
	
	%% Children %%
	Particles	% [Particle]	An array of Particles identified in the ROI
	actPar		% [#]	An array of indicides corresponding to the active Particles
	lbx_str		% {"str"}
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
		obj.img_sig = obj.img .* issignal;
	end
	
	%% METHODS %%
	function GetSpectrumBG(this, xrng, pow_noise)
	% Calculate the Background in the spectrum as a function of pixel.  We do
	% this by selecting several small chunks throughout the image and looking at
	% their standard deviation.  If it's too high, then it likely contains a
	% peak, and we need to ignore it.  Once we have determined which chunks are
	% viable, we then take the mean of those over the x-position to get our
	% background signal.
		
		%% Slice up the Spectrum Images %%
		% For the slice width, just use the window size.  That's what we're
		% integrating over anyways, and it saves using another parameter / slider
		slice_rad = floor(Frame.winval(1) / 2);
		
		% Compute the number of slices we need to take.  They will overlap with each
		% other by half, and this is intentional
		slices = size(this.img, 2) / slice_rad - 1;
		
		% Get the middle slice positions to sprawl out from (forget the top and
		% bottom slices, they're usually just blank anyways)
		slice_pos = (2:slices-1)*slice_rad;
	
		% Get the slices ranges %
		slice_rng = (-slice_rad:slice_rad)' + slice_pos;
		
		% Set up the valid slices %
		slice_valid = [];
		
		for s = 1:slices - 2
			% Get the slice specified %
			slice = this.img(slice_rng(:,s), xrng);
			
			% Compute the variance along y %
			slice_var = mean(var(slice, 0, 2));
			
			% Check if it's small enough to not be a peak, but large enough to be
			% something
			if(slice_var < 50*pow_noise && slice_var > 10*pow_noise)
				% Append the mean to the valid slices - the mean of means is the mean
				% of the whole, so don't worry. << CHECK THIS 
				slice_valid(:,end+1) = mean(slice, 1);
			end
		end
		
		% Compute the mean of means %
		this.spec_bg = mean(slice_valid, 2);
	end
	function PickSpectrumBG(this, xrng, filt_wgt)
		this.spec_bg = zeros([length(xrng), length(this.Particles)]);
		
		lower = -round(2*Frame.winval(1)+3*Frame.winval(2));
		upper =  round(2*Frame.winval(1)+3*Frame.winval(2));
		
		for p = 1:length(this.Particles)
			%axes(UI.axs(4));
			axes(UI.axs(4));
			yrng = this.Particles(p).peak_pos(2) + this.Particles(p).spec_off + (lower:upper);
			imagesc(this.img(yrng, xrng));
			% Guides %
			hold on;
			fill([0,0,1,1]*217, 3*Frame.winval(2)*[0,1,1,0], ...
				'r', 'facealpha', 0.3);
			fill([0,0,1,1]*217, 3*Frame.winval(2)*[-1,1,1,-1]+upper, ...
				'r', 'facealpha', 0.3);
			fill([0,0,1,1]*217, 3*Frame.winval(2)*[-1,0,0,-1]+(2*upper+1), ...
				'r', 'facealpha', 0.3);
			hold off;
			title(sprintf("Please select background for particle %d", p));
			
			%Particle.S_DispSpec(this.Particles(p))
			bg_pos = round(ginput(1));
			bg_pos = bg_pos(2);
			bgrng = this.Particles(p).peak_pos(2) - (upper+1) + bg_pos + ...
				(-Frame.winval(1):Frame.winval(1));
			this.spec_bg(:,p) = this.img(bgrng, xrng)' * filt_wgt(:,1);
			
			this.Particles(p).bg_pos = bg_pos;
			this.Particles(p).bg_img = this.img(bgrng, xrng);
			
			%figure()
			%subplot(3,1,[1,2])
			%imagesc(this.img(yrng, xrng));
			%line([1, range(xrng)], bg_pos*[1,1], 'color', 'r')
			%line([1, range(xrng)], bg_pos*[1,1]+2*Frame.winval(2), 'color', 'r', 'linestyle', '--')
			%line([1, range(xrng)], bg_pos*[1,1]-2*Frame.winval(2), 'color', 'r', 'linestyle', '--')
			%line([1, range(xrng)], bg_pos*[1,1]+Frame.winval(1), 'color', 'r', 'linestyle', ':')
			%line([1, range(xrng)], bg_pos*[1,1]-Frame.winval(1), 'color', 'r', 'linestyle', ':')
			
			%line([1, range(xrng)], upper*[1,1]+1, 'color', 'k')
			%line([1, range(xrng)], upper*[1,1]+1+2*Frame.winval(2), 'color', 'k', 'linestyle', '--')
			%line([1, range(xrng)], upper*[1,1]+1-2*Frame.winval(2), 'color', 'k', 'linestyle', '--')
			
			%subplot(3,1,3)
			%imagesc(this.Particles(p).bg_img);
			
			%figure()
			%imagesc(this.img(yrng, xrng))
			%line(ax, [xrng(1),xrng(end)]-xrng(1), bg_pos(2)*[1,1]-Frame.winval(2), 'color', 'k', 'linewidth', 4);
			%line(ax, [xrng(1),xrng(end)]-xrng(1), bg_pos(2)*[1,1], 'color',  'g', 'linewidth', 4);
			%line(ax, [xrng(1),xrng(end)]-xrng(1), bg_pos(2)*[1,1]+Frame.winval(2), 'color',  'k', 'linewidth', 4);
			if(p ~= length(this.Particles)), Particle.S_DispSpec(this.Particles(p)); end
		end
		Particle.S_DispSpec(this.Particles(1));
	end
	
	function SelectROI(this, roi)
	% This function prompts the user to select a Region of Interest (ROI) of the
	% Frame.  This ROI is then analyzed for particles, and a series of Particle
	% objects are created and attributed to this Frame.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
	%		storing and keeping track of many different instance variables.
	%
		%% Argument Defaults %%
		if(nargin < 2), roi = 0; end
	
		%% Refresh %%
		% If we're not appending to the current particle array, or its empty
		if(isempty(this.Particles))
			% Refresh the Particles field with an empty Particle array %
			this.Particles = Particle.empty;
		end
	
		%% User-Input %%
		if(isscalar(roi))
			% Prompt the user to select a rectangular Region of Interest (ROI) %
			roi = round(getrect(UI.axs(1)));
		end
		
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
		peak_pos = sortrows(peak_pos, 2);
		
		% Then simply translate the peak positions to the image coordinates %
		peak_pos = peak_pos + roi(1:2);		% Once again, blame MATLAB %
		
		%% Particle Creation %%
		% This can be a time-intensive process; create a waitbar %
		wb = waitbar(0, "Finding Particles...");
		
		pnum = length(this.Particles);
		this.lbx_str = cell([pnum, 1]);
		for p = 1:pnum
			this.lbx_str{p} = join(["Particle #", p, "@", this.Particles(p).str_loc]);
		end
		
		% For each found peak, create a child Particle %
		for p = 1:size(peak_pos, 1)
			% Update the waitbar %
			waitbar(p/size(peak_pos,1), wb, "Finding Particles...");
			
			% Slice a region of this image close to this particle.  We select pixels
			% in an entire range of y so everything related to spectrum processing 
			% can be contained in the Particle class.
			peak_yrng = (-rad_win:rad_win) + peak_pos(p,2);
			
			% Pass in the particle's peak position and the image slice %
			part = Particle(peak_pos(p,:), this.img, peak_yrng);
			
			% Drop the particle if it's not the maximum in its own box %
			if(max(part.peak_img(:)) ~= max(max(part.peak_img( ...
					Frame.winval(1) + 1 + (-2:2), Frame.winval(1) + 1 + (-2:2)))))
				continue; 
			end

			% Check to see if this particle already exists %
			if(any(any(part.peak_pos(1) == [this.Particles(:).peak_pos])) && ...
				any(any(part.peak_pos(2) == [this.Particles(:).peak_pos])))
				% Don't add it in, it's already here %
				continue;
			end
			
			pnum = pnum + 1;
			this.lbx_str{pnum} = join(["Particle #", pnum, "@", part.str_loc]);

			% Append this particle to the Frame's particle list %
			this.Particles(pnum) = part;
		end
		% Close the waitbar %
		close(wb);
		
		%% MainWin Update %%
		% Activate the first particle found %
		this.actPar = 1;
	end
	function FitROI(this, wb)
	% Function that fits all particles in the ROI with multiple peaks.  In the
	% process
		%% Create Filters %%
		% Make the selection, background, and total filters %
		ygrid = (-Frame.winval(1):Frame.winval(1))';	% Grid along the y-axis %
		yoff = 0;

		if(Frame.opmode(5))
			filt_wgt = exp(- (ygrid-yoff).^2 / (2*Frame.winval(2)^2)); % Gaussian Filter %
		else
			filt_wgt = abs(ygrid) <= 2*Frame.winval(2);	% 95% confidence %
		end
		%filt_oth = 1 - filt_sel;							% Inverse-Gaussian Filter %
		
		%filt_wgt = [filt_sel, filt_oth];	% Combine together for simplicity %

		% Normalize the Filters %
		filt_wgt = filt_wgt ./ sum(filt_wgt);
	
		%% Image Analysis %%		
		% Get the noise power, which is just its variance in what we didn't detect %
		pow_noise = var(this.img(this.img < this.thr_det));
		
		% Get the range over which we can calculate the spectrum background.  This
		% begins failing if the slit is too wide, however, so be careful!
		part_pos = [this.Particles.peak_pos];
		xrng = round(Particle.BND_PX(1) + min(part_pos(1,:))) : ...
			round(Particle.BND_PX(2) + max(part_pos(1,:)));
		
		% Calculate the spectrum background.  This is complicated, so look at the
		% relevant function.
		if(this.opmode(2))
			this.GetSpectrumBG(xrng, pow_noise);
			this.spec_bg = kron(this.spec_bg, ones([1,length(this.Particles)]));
		else
			this.PickSpectrumBG(xrng, filt_wgt);
		end
		
		%% Fit each Spectrum %%
		for p = 1:length(this.Particles)	
			% Call the spectrum fitting function for this particle %
			this.Particles(p).Fit_Spectrum(filt_wgt, xrng, this.spec_bg(:,p), pow_noise);
			
			% Update the waitbar %
			waitbar(wb.UserData(2)/wb.UserData(3), wb, ...
				join(["Fitting Spectra ( Frame", wb.UserData(1), ":", ...
				p, "/", length(this.Particles), ")"]));
			
			% Increment the particle fit counter %
			wb.UserData(2) = wb.UserData(2) + 1;
		end
		% Increment the frame number counter %
		wb.UserData(1) = wb.UserData(1) + 1;
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
	% + Scale Bar
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Original Image' axes 
	
		%% Argument Defaults %%
		if(nargin < 2), ax = UI.axs(1); end	% Default to the OriginalImage axes %
		
		%% Refresh %%
		% Wipe off all the boxes previously there %
		children = ax.Children;
		for c = 1:length(children)-3
			children(c).Visible = 'off';
		end
		%cla(ax, 'reset');
		
		%% Plotting %%
		if(isempty(children))						
			imagesc(ax, this.img);					% Plot the image the first time %
		else
			children(end).CData = this.img;			% Swap out the previous image %
		end
		
		axis(ax, 'image')							% 1:1 Aspect Ratio
		xlim(ax, [1/6, 5/6]*size(this.img, 1));		% Constrain the viewing region %
		
		%% Labeling %%
		t = title(ax, join(["Original Image (", this.file_name, ")"]));	% Title %
		t.Interpreter = 'none';	% Remove the TeX interpreter %
		
		c = colorbar(ax);							% Colorbar %
		c.Label.String = "Intensity (arb.)";
		
		xlabel(ax, "X position (px)");				% Axes labels %
		ylabel(ax, "Y position (px)");
        
        %% Scale Bar %%
		if(Particle.visopt(1))
			line(ax, size(this.img, 1)*(0.20) + [0,Frame.SBAR_PX], ...
				size(this.img, 1)*(1-0.06) + [0,0], ...
				'Color', 'w', 'LineWidth', 3);
			text(ax, size(this.img, 1)*(0.20) + Frame.SBAR_PX/2, ...
				size(this.img, 1)*(1-0.08), "10\mum", ...
				'Interpreter', 'tex', 'Color', 'w', 'HorizontalAlignment', 'center');
		end
	end
	function DispBox(this, ax)
	% Draws boxes around all particles selected in this frame
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Original Image' axes 
	
		%% Argument Defaults %%
		if(nargin < 2), ax = UI.axs(1); end	% Default to the OriginalImage axes %
		
		%% Refresh %%
		% Gather all the children %
		children = ax.Children;
		
		% Make invisible any previous boxes %
		for c = 1:length(children)-3
			children(c).Visible = 'off';
		end
		
		%% Draw Boxes %%
		% Run through all particles in this frame and draw the appropriately colored
		% box around each one.  If a particle that previously had a box is selected
		% again, the box will become visible once more.
		for p = 1:length(this.Particles)
			% If p is an active particle, then it will draw a green box %
			this.Particles(p).DispBox(ax, any(p == this.actPar));
		end
	end
end
end