classdef Particle < handle
% Class that contains methods and infomration about each particle in the image, used
% primarily as an object for modular spectrum fitting.

properties(Constant)
	% Selection boxes %
	FILT_RAD = 10	% Selection window radius (x & y) %
	SPEC_RAD = 107	% Spectrum window radius (x) - uses WIN_RAD for (y) %
	SPEC_OFF = 330	% 350 px for 1200x1200, 713 px for 2048x2048 %
	
	% Spectral calibration %
	CAL_SLOPE = 1.927	% Calibration slope (nm) %
	CAL_INTER = -1.7313 % Calibration intercept (nm) %
end
properties
	pos		% Position of the particle %
	spos	% Position of the center of the particle's spectrum %
	
	spec	% Spectrum struct of plots (selection, background, signal, sum(fit)) %
	fits	% Individual spectrum fits %
	params	% Individual spectrum fit parameters [A, x_0, Gamma] %
	grids	% The eV and nm grids for this fit %
	
	best_fit % The x_0 of the fit that best aligns with what it should? %
	best_params
end

methods(Static)
	function Select(parent)
		%% Initialization %%
		% Get the rectangular region of interest specified by the user %
		roi = round(getrect(parent.UserData.ax_img));

		% Find the region of interest in the image %
		xrng = roi(2) + (1:roi(4));
		yrng = roi(1) + (1:roi(3));
		roi_img = parent.UserData.img(xrng, yrng);
		
		% Build a Gaussian filter %
		filt_rad = 6;	% Filter Radius %
		filt_sig = 2;	% Gaussian standard deviation %
		[xx, yy] = meshgrid(-filt_rad:filt_rad, -filt_rad:filt_rad);
		filt = exp(-(xx.^2 + yy.^2)/(2*filt_sig^2));	% Build a Gaussian %
		filt = filt ./ sum(filt(:));					% Normalize the filter %
		Filt = fft2(filt, size(roi_img,1), size(roi_img,2));	% Zero-pad the fft %
		
		% Smooth the image with our filter - this denoises it enough to perform
		% elementary peakfinding - which is where our particles are located!
		filt_roi = ifft2(fft2(roi_img) .* Filt);
		
		%% Peakfinding %%
		% Build the derivative filters in x and y and apply them forward & backward %
		dx = diff(filt_roi, 1, 1) > 0;
		dy = diff(filt_roi, 1, 2) > 0;
		
		dx_0 = zeros(size(filt_roi(1,:)));	% Zero padding for size reasons %
		dy_0 = zeros(size(filt_roi(:,1)));
		
		roi_dx = [dx_0; dx] & [1-dx; dx_0];	% Forward & Backward %
		roi_dy = [dy_0, dy] & [1-dy, dy_0];
		
		% Threshold based on two standard deviations above the nonzero value mean -
		% unfortunately this means that we will need to take a larger sample around
		% the particle of interest (if there is only one) in order to drive down the
		% mean and standard deviation.  Remember, the particle has to be in the > 95%
		% regime!
		thresh = mean(filt_roi(filt_roi > 0)) + 2*std(filt_roi(filt_roi > 0));
		
		% Check if this threshold is too close to the maximum %
		if(thresh > 0.75*max(filt_roi(:)))	% > 75% picked arbitrarily %
			% We probably picked up only noise, throw everything away %
			thresh = max(filt_roi(:));
		end
		roi_th = (filt_roi > thresh);
		
		% Combine all of our filtering together! %
		peak_roi = filt_roi .* (roi_dx & roi_dy & roi_th);
		
		% Find the appropriate peaks and their (x,y) coordinates in the selection %
		peaks = find(peak_roi);
		[px, py] = ind2sub(size(peak_roi), peaks);
		
		%% Visualization and Data storage %%
		% Clear the boxes from before, if there were any - also tell the user how
		% many particles we've found, if any...
		Show.Image(parent.UserData.ax_img, parent.UserData.img, ...
			join(["Original Image | Particles Located:", length(peaks)]));
		
		% Store all the new particles in the region of interest into the UserData %
		parent.UserData.fp_str = cell(1, length(peaks));
		
		for p = 1:length(peaks)
			% Make a new Particle %
			part = Particle;
			
			% Pass in the particle and spectrum positions %
			part.pos =	[py(p) + roi(1), px(p) + roi(2)] - filt_rad;
			part.spos = part.pos + [part.SPEC_OFF, 0];		% Don't forget to shift %
			% The reason we subtract filt_rad from both coordinates in part.pos is
			% because when we convolve with a filter, the result shifts right by the 
			% ~ half the length of the filter.  We correct for this to put the peak
			% in the center.
			
			% Append the particle to the Particles list %
			if(~isfield(parent.UserData, 'Particles') || p == 1)
				% Create a new particle list %
				parent.UserData.Particles = part;
			else
				parent.UserData.Particles(end+1) = part;
			end
			
			% Draw a box around the particle and its spectrum %
			Show.Box(parent.UserData.ax_img, part);
			
			% Update the listbox in MainWin with the found particle %
			parent.UserData.fp_str{p} = join(["Particle", p, "- (     )"]);
			
			% Start creating list entries in MainWin %
			lbx_fndpart = findobj(parent.Children, 'flat', 'Tag', "lbx: Found Particles");
			lbx_fndpart.Enable = 'on';
			lbx_fndpart.String = parent.UserData.fp_str;
			lbx_fndpart.Value = 1;
			drawnow limitrate;
		end
		
		%% MainWin Update %%
		% Make the 'Fit Selected Particles' button enabled %
		but_fitpart = findobj(parent.Children, 'flat', 'Tag', 'Fit Selected Particles');
		but_fitpart.Enable = 'on';
		
		% Display the first spectrum %
		img = parent.UserData.img;
		part = parent.UserData.Particles(1);
		spec_img = img(part.spos(2) + (-Particle.FILT_RAD:Particle.FILT_RAD), ...
					   part.spos(1) + (-Particle.SPEC_RAD:Particle.SPEC_RAD));
		
		Show.Image(parent.UserData.ax_spec, spec_img, ...
			join(["Spectrum Image for Particle", 1]));
		axis(parent.UserData.ax_spec, 'normal');
		
		% Change 'Spec Plot' to false %
		tog_specplt = findobj(parent.Children, 'flat', 'Tag', "Spec Plot");
		tog_specplt.Value = false;
		tog_specplt.UserData.prev_idx = 1;
		parent.UserData.specplt = false;
	end
	
	function FitSelected(parent)
		%% CONSTANTS %%
		HC = (6.626E-34 * 2.998E8) * (1E9/1.602E-19);	% For hw (eV) = hc / lambda (nm) %
		EV_RNG = [1.5, 3.0];			% in eV %
		NM_RNG = HC ./ flip(EV_RNG);	% in nm - flipped so (1) < (2) %
		
		%% Initialization %%
		% Copy in the image from MainWin %
		img = parent.UserData.img;
		
		% Other parameters to pull from MainWin: %
		% sel_sig: Standard deviation of selection filter %
		% sel_thr: Threshold of maximum to determine if something is bad %
		% fit_lor: # of Lorentzians to fit %
		% fit_itr: # of iterations to perform when correcting fits %
		[sel_sig, sel_thr, fit_lor, fit_itr] = parent.UserData.FitParams{:};
		
		% Remember that sel_thr is in percent! %
		sel_thr = sel_thr / 100;
		
		% Prepare the selection and background windows %
		xgrid = -Particle.SPEC_RAD:Particle.SPEC_RAD;	% Grids %
		ygrid = -Particle.FILT_RAD:Particle.FILT_RAD;
		
		sel_filt = exp(-(ygrid.^2)/(2*sel_sig^2));	% Constrcut a Gaussian filter %
		bg_filt = 1 - sel_filt;
		
		sel_filt = sel_filt / sum(sel_filt);		% Normalize %
		bg_filt = bg_filt / sum(bg_filt);
		
		% Prepare the plotting ranges %
		nm_off = 382;	% ??? %
		
		nm_rng = (NM_RNG - Particle.CAL_INTER) / Particle.CAL_SLOPE;
		nm_grid = (floor(nm_rng(1)):floor(nm_rng(2))) + nm_off;
		
		ev_grid = Particle.CAL_SLOPE * (nm_grid - nm_off) + Particle.CAL_INTER;
		ev_grid = HC ./ ev_grid;	% Don't forget! %

		%% Iteration %%
		% Refresh the listbox %
		lbx_fndpart = findobj(parent.Children, 'flat', 'Tag', "lbx: Found Particles");
		lbx_fndpart.Enable = 'on';
		for p = 1:length(parent.UserData.Particles)
			parent.UserData.fp_str{p} = join(["Particle", p, "- (     )"]);
		end
		lbx_fndpart.String = parent.UserData.fp_str;
		
		% For each found particle... %
		for p = 1:length(parent.UserData.Particles)
			%% Get the Selection and Background plots %%
			% Get the image of its spectrum %
			spec_xrng = parent.UserData.Particles(p).spos(1) + xgrid;
			spec_yrng = parent.UserData.Particles(p).spos(2) + ygrid;
			
			spec_img = img(spec_yrng, spec_xrng);
			
			% Truncate the image where the detector is sure to have signal %
			[spec_trun, bnds] = Particle.TruncateImage(spec_img);
			
			% Remember to also truncate the grids as well %
			this_nm_grid = nm_grid(bnds(1):bnds(2));
			this_ev_grid = ev_grid(bnds(1):bnds(2));

			% Get the selection plot and the background plots - matrix multiplication
			% will handle the weighted sum very well.
			sel_plot = sel_filt * spec_trun;
			bg_plot = bg_filt * spec_trun;
			
			% Get the signal we're interested in using a soft threshold %
			sig_plot = max(sel_plot - bg_plot, 0);
			
			%% Lorentzian Fitting %%
			fit = zeros([fit_lor, length(sig_plot)]);
			params = zeros([fit_lor, 3]); % [A, x_0, Gamma] %
			
			% Iteration for accuracy %
			for i = 1:fit_itr
				for l = 1:fit_lor
					% Determine if the total fit (except this one) has an issue %
					fit_prev = sum(fit((1:fit_lor) ~= l,:), 1);
					[issue, err_plot] = Particle.FitIssue(sig_plot, fit_prev, sel_thr);
					if(issue)
						% Re-fit this Lorentzian to the extra data %
						[fit(l,:), params(l,:)] = Particle.FitLorentzian(err_plot, this_nm_grid);
					else
						break;
					end
					
					% Stop if we have a good enough fit with fewer Lorentzians! %
					if(i > 1)
						[issue, ~] = Particle.FitIssue(sig_plot, sum(fit(1:l,:), 1), 2*sel_thr);
						if(~issue)
							% Set the useless ones to zero so we can discard them %
							fit(l+1:end,:) = 0;
							params(l+1:end,1) = 0;
							break;
						end
					end
				end
				
				% Sort the fits based on their peak intensities %
				[~, sortidx] = sort(params(:,1), 'descend');
				fit = fit(sortidx, :);
				params = params(sortidx, :);
			end
			
			% Remove the unnecessary Lorentzians %
			useless_fit = (params(:,1) < 100); % Arbitrary number %
			fit = fit(~useless_fit, :);
			params = params(~useless_fit, :);
			
			% Find and write down the supposed "best peak" - for this, we'll be
			% looking at the slice down the middle and seeing where its peak is
			%mid_spec = spec_img(ceil(end/2), :);
			peakidxs = zeros([size(fit, 1), 1]);
			for f = 1:length(peakidxs)
				[~, peakidxs(f)] = find(abs(params(f,2) - nm_grid) < 0.5, 1);
			end
			[~, bestidx] = min(abs(peakidxs - length(nm_grid)/2) .* params(:,1).^(-0.9));
			best_fit = fit(bestidx, :);
			best_params = params(bestidx, :);
			
			% Copy data into the associated particle in MainWin %
			parent.UserData.Particles(p).spec = [sel_plot; bg_plot; sig_plot; fit]';
			parent.UserData.Particles(p).fits = fit';
			parent.UserData.Particles(p).params = params';
			parent.UserData.Particles(p).grids = [this_ev_grid; this_nm_grid]';
			
			parent.UserData.Particles(p).best_fit = best_fit';
			parent.UserData.Particles(p).best_params = best_params';

			% Update the found particles string cell for the listbox %
			parent.UserData.fp_str{p} = join(["Particle", p, "- (", char(10003), ")"]);
			
			% Start creating list entries in MainWin %
			%lbx_fndpart = findobj(parent.Children, 'flat', 'Tag', "lbx: Found Particles");
			%lbx_fndpart.Enable = 'on';
			lbx_fndpart.String = parent.UserData.fp_str;
			drawnow limitrate;
			%{
			figure(2);
			subplot(3,4,1:3);
			imagesc(spec_img);
			line([bnds(1), bnds(1)], [0, 22], 'Color', 'r', 'LineWidth', 2);
			line([bnds(2), bnds(2)], [0, 22], 'Color', 'r', 'LineWidth', 2);
			
			subplot(3,4,4);
			plot([sel_win; bg_win], yy);
			legend("Selection", "Background");
			xlabel("Weight");
			ylabel("Pixel");
			grid on;
			
			subplot(3,4,5:6)
			sel_img = kron(sel_win', ones([1, size(spec_trun,2)])) .* spec_trun;
			imagesc(sel_img);
			title("Selection Image");
			caxis([0, max(sel_img(:))]);
			
			subplot(3,4,7:8)
			bg_img = kron(bg_win', ones([1, size(spec_trun,2)])) .* spec_trun;
			imagesc(bg_img);
			title("Background Image");
			caxis([0, max(sel_img(:))]);
			
			subplot(3,4,9:10)
			plot(sel_plot);
			title("Selection Spectrum");
			axis tight
			ylim([0, max(sel_plot)]);
			
			subplot(3,4,11:12)
			plot(bg_plot);
			title("Background Spectrum");
			axis tight
			ylim([0, max(sel_plot)]);
			%}
		end
		
		%% MainWin Update %%
		% Plot the first particle in the spectrum axes %
		sel_part = lbx_fndpart.Value;
		Show.SpecPlot(parent.UserData.ax_spec, parent.UserData.Particles(sel_part), ...
					sel_part, parent.UserData.use_evs);
		
		% Change 'Spec Plot' to true %
		tog_specplt = findobj(parent.Children, 'flat', 'Tag', "Spec Plot");
		tog_specplt.Value = true;
		parent.UserData.specplt = true;
	end
	function [img_trun, bnds] = TruncateImage(img)
	% Truncates the image where "the detector is valid".  Essentially, if any pixel
	% in a column is zero (hence "just noise" becuase there shouldn't be any signal
	% there) then the following (or leading) columns of the image are no longer
	% valid, and must be removed.  This leaves just the columns that are completely
	% nonzero as 'img_trun'.  The bounds are also output so we know the left bound's
	% offset when making an axis to fit along later.
	
		%% Initialize %%
		% Sizes %
		[~, X] = size(img);
		
		% Bounds %
		bnds = [1, X];
		
		% Minimum of each column %
		haszero = min(img, [], 1) == 0;
		
		% Starting zero flag %
		flg = haszero(1);
		
		%% Iterate through the image's columns %%
		for col = 1:X
			% Check if this column has a zero %
			if(haszero(col))
				% Check if the flag has been set and we're not in the right 10% %
				if(flg && col < 0.8*X)
					% Then we need to move the left bound over to here %
					bnds(1) = col + 1;
					
				% Else check if the flag was lowered and we're not in the left 10% %
				elseif(~flg && col > 0.2*X)
					% Then we need to move the right bound here and break %
					bnds(2) = col - 1;
					break;
					
				else
					% There was a zero, but we've already passed the left bound, so
					% start lowering the flag
					flg = col < 0.2*X;
				end
			end
		end
		
		% Return the truncated image %
		img_trun = img(:, bnds(1):bnds(2));
	end
	function [fit, params] = FitLorentzian(data, grid)
	% The Lorentzian function is given by:
	%
	%	L(x) = 2/pi * Gamma/[ 4(x - x_0)^2 + Gamma^2 ]
	%		 = 2/pi / Gamma [ ( 2(x-x_0)/Gamma )^2 + 1]
	%
	%	It has the special property that the FWHM is Gamma
	%
	%	However, for broad and fitting appeal, we need to introduce an amplitude and
	%	y-intercept term: A, and b respectively.  We can absorb the 2/pi into A:
	%
	%	L(x) = A / [ (2(x-x_0)/Gamma )^2 + 1] + b
	%
	%	We constrain such that b = 0, as the background should be subtracted
	
		%% Initialization %%
		% Fitting parameters %
		tol = 1E-6;		% Fitting tolerance %
		
		% Sizes %
		len = length(data);
		
		% Initalize the nonlinear least squares optimization options %
		lsqopt = optimset('lsqnonlin');
		lsqopt.TolFun = tol;
		lsqopt.TolX = tol;
		lsqopt.Display = 'off';
		
		% Establish the bounds of the nonlinear fit (params = [A, x_0, Gamma]) %
		bnd_lo = [0E4, 0 + grid(1), 0];
		bnd_hi = [1E4, len + grid(1), len];
		
		% The initial value of the nonlinear fit parameters %
		p_0 = [1E3, len/2 + grid(1), 20];
		
		%% Optimization %%
		% Let MATLAB take care of this fit! %
		params = lsqnonlin(@(p) LorentzianFxn(p, grid, data), p_0, bnd_lo, bnd_hi, lsqopt);
		
		% Also return the fit itself %
		fit = LorentzianFxn(params, grid);
		
		function [res] = LorentzianFxn(p, x, y)
			% Compute the value of the Lorentzian at each x given parameters p %
			vals = p(1) ./ ((2*(x-p(2))/p(3)).^2 + 1);
			
			% Return the residual with the data given in y %
			if(nargin < 3)
				res = vals;
			else
				res = vals - y;
			end
		end
	end
	function [issue, err] = FitIssue(data, fit, thresh)
		prob = (data - fit) > thresh * max(data);
		issue = sum(prob) > 0.10*length(prob);
		err = (data - fit) .* prob;
	end
end

methods
	
end

end