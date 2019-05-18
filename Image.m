classdef Image < handle
properties
	filename
	
	img
	
	img_thr
	img_sig
	img_noise

	detect_threshold
	signal_threshold
end

methods
	function [obj] = Image(data, filename)
		%% Load this frame into memory %%
		obj.img = data;
		
		%% Pre-Processing %%
		% Anything out of the detection region should be lower than the mean, so 
		% let's zero it out.  However, it should be noted that what we're throwing 
		% out can be used as a good metric for the noise power - so we should be 
		% careful.
		obj.detect_threshold = mean(obj.img(:));
		detected = obj.img > obj.detect_threshold;
		
		obj.img_thr = obj.img .* (detected);
		obj.img_noise = obj.img .* (~detected);
		
		% Additionally, determine the threshold for that which isn't noise, but is in
		% the slit region, and therefore could be considered background.  We will
		% look for the 95%+ intensity regime, due to the fact that we reject the null
		% hypothesis for p < 0.05, and hence go with the hypothesis that there is a
		% particle there.
		obj.signal_threshold = mean(obj.img(detected)) + 2*std(obj.img(detected));
		issignal = obj.img > obj.signal_threshold;
		
		obj.img_sig = obj.img .* (issignal);
		
		%% Additional things %%
		obj.filename = filename;
	end
	
	function Select(this, parent)
		%% Initialization %%
		% Get the rectangular region of interest (ROI) specified by the user %
		roi = round(getrect(parent.UserData.axs{1}));
		
		% Acquire the roi-image %
		roi_x = roi(2) + (1:roi(4));
		roi_y = roi(1) + (1:roi(3));
		roi_img = this.img(roi_x, roi_y);
		
		%% Smooth the ROI (SNR Boost) %%
		% Smooth the roi-image using a Gaussian Filter - it's a suitable
		% approximation to the PSFs in the image.
		winrad = parent.UserData.Fitting.params(1); % The chosen window radius %
		semiwinrad = floor(winrad/2);
		grid = -semiwinrad:semiwinrad;		% Make the filter half the size %
		[xx, yy] = meshgrid(grid, grid);
		gfilt = exp(-(xx.^2 + yy.^2)/(2*(semiwinrad/3)^2));	 % Fit over 3 sigmas %
		gfilt = gfilt / sum(gfilt(:));		% Normalize %
		
		% Since the size of the filter is small compared to the image we're
		% filtering, it's actually quicker to do the 2D linear convolution than to
		% use the FFT/IFFT.  Consider [2 N^2 log(N)] versus [N M^2] where M << N.
		% This also prevents artifacts from the circular convolution!
		filt_img = conv2(roi_img, gfilt, 'same');
		
		%% Localize Particles by Finding Peaks %%
		% Develop a maximum filter in x and y using forward and backward positive
		% derivatives.  Essentially, this checks that at the point in question, the
		% derivative has increased to get there, and it will decrease once it leaves,
		% hence finding a maximum.  We have to smooth the signal before we get to
		% this point, else all the noise will be picked up as maxima.
		dx = diff(filt_img, 1, 1) > 0;
		dy = diff(filt_img, 1, 2) > 0;
		
		dx_pad = zeros(size(filt_img(1,:)));	% Zero padding %
		dy_pad = zeros(size(filt_img(:,1)));
		
		filt_dx = [dx_pad; dx] & [1-dx; dx_pad];	% Forward and Backward %
		filt_dy = [dy_pad, dy] & [1-dy, dy_pad];
		
		% For the region of interest, threshold based on img_sig %
		roi_sig = this.img_sig(roi_x, roi_y) .* (filt_dx & filt_dy);
		
		% Now it's as simple as finding where roi_sig > 0 and getting the coords %
		[peak_pos(:,2), peak_pos(:,1)] = ind2sub(size(roi_sig), find(roi_sig));
		
		% We need to transform these to the image coordinates... %
		peak_pos = peak_pos + roi(1:2);
		
		%% Create a Particle for each Peak %
		listbox_str = {};
		listbox = findobj(parent.Children, 'flat', 'Tag', "lbx: Found Particles");
		listbox.String = listbox_str;
		
		for p = 1:size(peak_pos, 1)			
			% Pass in a slice of the image as well; it's easier to work with and only
			% passes the relevant information into each particle.
			peak_rng = (-winrad:winrad) + peak_pos(p,2);
			
			% Append it to the particle list %
			parent.UserData.Particles{p} = Particle(peak_pos(p,:), this.img(peak_rng,:));
			
			% And put the particle in the listbox %
			listbox_str{p} = parent.UserData.Particles{p}.str_lbox;
			listbox.String = listbox_str;
			
			% Plop a box around the peak and spectrum %
			this.DispBox(parent.UserData.axs{1}, parent.UserData.Particles{p});
		end
		% Display the first particle found, if applicable %
		listbox.Value = 1;
		listbox.UserData.preVal = 1;	% Set the previous value to 1 as well %
		
		%% Visualization %%
		% Plot the first particle found in the proper axes %
		if(p > 0)
			parent.UserData.Particles{1}.DispImg(parent.UserData.axs{2}, parent.UserData.axs{3});
		else
			% Let the user know that nothing was found %
			cla(parent.UserData.axs{2}, 'reset');
			text(parent.UserData.axs{2}, 0.32, 0.5, "No particles found...");
			
			cla(parent.UserData.axs{3}, 'reset');
			text(parent.UserData.axs{3}, 0.41, 0.5, "No particles found...");
		end
		
		% Debug purposes %
		if(parent.UserData.DEBUG)
			figure(2);
			subplot(2,2,1);
			surf(roi_img); shading interp;
			title("The region of interest");
			
			subplot(2,2,2);
			surf(filt_img); shading interp;
			title("The smoothed region of interest");
			
			subplot(2,2,3);
			imagesc(roi_sig);
			title("The thresholded region of interest, showing the peaks");
			set(gca, 'YDir', 'normal');
		end
		
	end
	
	function DispImg(this, ax)
	% Function displaying this image's 'img' on axes 'ax' and having title 'ttl'.
	% Additionally adjusts the aspect ratio to have 1:1 correspondence, labels the
	% colorbar with "Intensity (arb.)", and sets the y-direction to 'normal'.

		% Reset the axes %
		cla(ax, 'reset');
		
		% Plot the new data %
		imagesc(ax, this.img);
		
		% Apply all the fancy labeling things %
		title(ax, join(["Original Image (", this.filename, ")"]));	% Set the title %
		axis(ax, 'image')	% Set the aspect ratio %
		c = colorbar(ax);	% Makes the colorbar visible %
		c.Label.String = "Intensity (arb.)";	% Labels the colorbar %
		
		xlabel(ax, "X position (px)");
		ylabel(ax, "Y position (px)");
	end
	function DispBox(this, ax, part, sel)
	% Draws a colored box around the particle given in 'part'.  If 'sel' is true, the
	% box is green (indicating that is selected particle) and if false, it is red.
		
	end
end
	
end