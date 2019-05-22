classdef Particle < handle
% A dynamic class for handling identified particles.
%
% Static variables:
%	> pltopt:	[bool] Array of boolean values to determine various plotting options
%		(use_evs, show_bg, show_sig)
%
%	> fitval:	[#]	Array of numbers corresponding to the values of the fitting
%		parameter sliders in MainWin (flt_sig, fit_snr, fit_lor, fit_itr)
%
%	> imgdim:	[#] Array of numbers corresponding to the sizes of the various images
%		associated with this Particle (peak_x, peak_y, spec_x, spec_y)

properties(Constant)
	% Physical Constants %
	HC = (6.626E-34 * 2.998E8) * (1E9/1.602E-19);
	
	% Spectral ranges %
	EV_RNG = [1.5, 3.0]						% Bounds in eV %
	NM_RNG = HC ./ flip(Particle.EV_RNG)	% Bounds in nm [flipped so (1) < (2)] %
	
	% Spectral calibration %
	CAL_SLOPE = 1.927	% Calibration slope (nm) %
	CAL_INTER = -1.7313 % Calibration intercept (nm) %
end

properties
	isActive	% (bool)	Determines if this Particle is currently selected %
	
	%% Peak %%
	peak_pos	% (x, y) Peak position %
	peak_rng
	peak_img	% [x, y] Image at the peak with a small surrounding window %

	%% Spectrum %%
	spec_pos	% (x, y) Spectrum position %
	spec_rng	% (-, +) Spectrum range (in px) %
	spec_img	% [x, y] Image at the spectrum with a small surrounding window %
	
	spec_plot	% [sel, bg, sig, fit(s)] Various parts of the integrated spectrum % 
	spec_fits
	
	%% Display %%
	peak_lines	% [line] Array of lines that define the box around the peak image %
	spec_lines	% [line] Array of lines that define the box around the spectrum img %
	
	str
end
	
%% DYNAMIC METHODS %%
methods
	%%%%%% STILL NEEDS WORK %%%%
	%% CONSTRUCTOR %%
	function [obj] = Particle(peak_pos, img)		
		obj.peak_pos = peak_pos;
		
		% The window size is encoded in the dimensions of img %
		filt_rad = size(img, 1);
		
		% Make the peak range %
		obj.peak_rng = (-floor(filt_rad/2):floor(filt_rad/2)) + obj.peak_pos(1);
		
		% Then determine how big the spectrum range needs to be and make it %
		spec_rngpx = round(Particle.nm2px(Particle.NM_RNG, obj.peak_pos(1)));
		obj.spec_rng = spec_rngpx(1):spec_rngpx(2);
		% Get the image chunks %
		obj.peak_img = img(:,obj.peak_rng);
		obj.spec_img = img(:,obj.spec_rng);
		
		% Write up the listbox string %
		obj.str = join(["Particle @ (", obj.peak_pos(1), ",", ...
			obj.peak_pos(2), ")"]);
	end
	
	%% METHODS %%
	function FitSpec(this, parent, param, flt)
		%% Integrate the spectrum image with the various filters %%
		% Obtain the selection and background spectra %
		plt.sel = (flt.sel * this.spec_img)';
		plt.bg = (flt.bg * this.spec_img)';
		
		% Get the "signal" by subtracting the background and soft thresholding %
		plt.sig = max(plt.sel - plt.bg, 1);
		
		% Calculate the signal to noise ratio by smoothing the integrated signal %
		plt.img = (flt.img * this.spec_img)';
		
		plt.sm = conv(sum(this.spec_img, 1)'/size(this.spec_img,1), flt.sm, 'same');
		noise = sum(this.spec_img, 1)'/size(this.spec_img,1) - plt.sm;
		
		nstd = std(noise(length(flt.sm):end-length(flt.sm)));

		%% Multiple Lorentzian fitting %%
		fit.fits = zeros([length(plt.sig), param.lorentz]);
		fit.para = zeros([3, param.lorentz]);
		
		% Repeat this process a number of times... %
		for i = 1:param.iter
			% For each Lorentzian... %
			for l = 1:param.lorentz
				fit_other = sum(fit.fits(:,(1:param.lorentz) ~= l), 2);
				[issue, err] = Particle.FitIssue(plt.sig, fit_other, ...
					sqrt(param.thresh)*nstd);
				if(~issue)
					break;
				end
				
				[fit.fits(:,l), fit.para(:,l)] = ...
					Particle.FitLorentzian((1:length(err))', err);
				
				if(fit.para(1,l) < 2*sqrt(param.thresh)*nstd)
					fit.fits(:,l) = 0;
					fit.para(:,l) = 0;
				end
			end
		end
		
		% Weed out the useless Lorentzians %
		useless = (fit.para(1,:) == 0);
		fit.fits = fit.fits(:,~useless);
		fit.para = fit.para(:,~useless);
		
		% Adjust the wavelength-dependent parameters %
		fit.para(2,:) = Particle.px2nm(fit.para(2,:) + this.spec_rng(1), this.peak_pos(1));	% x_0 %
		fit.para(3,:) = Particle.px2nm(fit.para(3,:));	% Gamma has no offset %
		
		%% Uhh %%
		this.spec_plot = plt;
		this.spec_fits = fit;
		
		%% Visualization %%
		% Debug purposes %
		if(parent.UserData.DEBUG)
			figure(3);
			plot([sum(this.spec_img, 1)'/size(this.spec_img,1), plt.sm, noise]);
		end
	end
	
	%% VISUALIZATION %%
	function DispBox(this, ax, sel)
		%% Arugment Defaults %%
		if(nargin < 2), ax = UI.axs(1); end		% Default to the Original Img axes %
		if(nargin < 3), sel = false; end		% Default to not selected %
		
		%% Color Selection %%
		if(sel)
			color = [0, 0.8, 0];	% Green for the selected particle %
		else
			color = [1, 0, 0];		% Red for a non-selected particle %
		end
		
		%% Draw the box %%
		% Check if it doesn't exist yet, and if so, draw it! %
		if(isempty(this.peak_lines))
			% Figure out the dimensions %
			[filt_sz(2), filt_sz(1)] = size(this.peak_img);
			spec_edges = [this.spec_rng(1), this.spec_rng(end)];

			% For brevity: (1,1) = left, (2,1) = right, (1,2) = bottom, (2,2) = top %
			peak_edges = (this.peak_pos + [-1,1]'/2 * filt_sz);

			%% Peak Box %%
			% Sides %
			this.peak_lines{1} = line(ax, peak_edges(1)*[1,1], peak_edges(3:4));
			this.peak_lines{2} = line(ax, peak_edges(2)*[1,1], peak_edges(3:4));

			% Top/Bottom %
			this.peak_lines{3} = line(ax, peak_edges(1:2), peak_edges(3)*[1,1]);
			this.peak_lines{4} = line(ax, peak_edges(1:2), peak_edges(4)*[1,1]);

			%% Spectrum Box %%
			% Sides %
			this.spec_lines{1} = line(ax, spec_edges(1)*[1,1], peak_edges(3:4));
			this.spec_lines{2} = line(ax, spec_edges(2)*[1,1], peak_edges(3:4));

			% Top/Bottom %
			this.spec_lines{3} = line(ax, spec_edges(1:2), peak_edges(3)*[1,1]);
			this.spec_lines{4} = line(ax, spec_edges(1:2), peak_edges(4)*[1,1]);
		end
		
		% Color the boxes %
		for e = 1:4
			this.peak_lines{e}.Color = color;
			this.spec_lines{e}.Color = color;
		end
	end
	
	function DispPeak(this, ax)
	% Displays the image of the peak of this particle on the axes specified by 'ax'.
	%
	% To plot in a separate figure, use:
	% > figure;
	% > [Particle Instance].DispSpec(gca);
	%
	% Additional considerations taken:
	% + Aspect ratio is 1:1
	% + Grid
	% + Title is "Peak Image"
	% + Labeled colorbar with "Intensity (arb.)"
	% + Axes labels
	% + Tick marks are shifted to the (x,y) coordinates of the peak
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Peak Image' axes 
	
		%% Argument Defaults %%
		if(nargin < 2), ax = UI.axs(2); end	% Default to the PeakImg axes %
		
		%% Preliminaries %%
		% Create the tick range %
		peak_rad = floor( size(this.peak_img, 1)/2 );
		tick_rng = 1:5:2*peak_rad;
		
		%% Refresh %%
		cla(ax, 'reset');
		
		%% Plotting %%
		imagesc(ax, this.peak_img);				% Plot the image %
		
		axis(ax, 'image');						% 1:1 aspect ratio %
		grid(ax, 'on');							% Show grid %
		
		%% Labelling %%
		title(ax, "Selected Peak Image");		% Title %
		
		cb = colorbar(ax);						% Colorbar %
		cb.Label.String = "Intensity (arb.)";
		
		xlabel(ax, "X position (px)");			% Axis labels %
		ylabel(ax, "Y position (px)");
		
		xticks(ax, tick_rng);					% Tick marks %
		yticks(ax, tick_rng);

		%tick_lbl = zeros([length(tick_rng), 2]);	% Tick labels %
		tick_lbl = tick_rng + this.peak_pos - peak_rad - 1;
% 		for tk = 1:length(tick_rng)
% 			tick_lbl(tk,:) = tick_rng(tk) + this.peak_pos - peak_rad - 1;
% 		end
		xticklabels(ax, {tick_lbl(:,1)});
		yticklabels(ax, {flip(tick_lbl(:,2))});
	end
	function DispSpec(this, ax)
	% Displays the image of the spectrum of this particle on the axes specified by 
	% 'ax'. 
	%
	% To plot in a separate figure, use:
	% > figure;
	% > [Particle Instance].DispSpec(gca);
	%
	% Additional considerations taken:
	% + Grid
	% + Title is "Spectrum Image"
	% + Labeled colorbar with "Intensity (arb.)"
	% + Axes labels
	% + Tick marks are shifted to the (x,y) coordinates of the spectrum
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Spectrum Image' axes 
	
		%% Argument Defaults %%
		if(nargin < 2), ax = UI.axs(3); end	% Default to the SpecImg axes %
		
		%% Preliminaries %%
		% Create the tick range %
		peak_rad = floor( size(this.peak_img, 1)/2 );
		ptk_rng = 1:5:2*peak_rad;
		stk_rng = 1:10:spec_sz(1);
		
		%% Refresh %%
		cla(ax, 'reset');
		
		%% Plotting %%
		imagesc(ax, this.spec_img);				% Plot the image %
		
												% Don't set aspect ratio! %
		grid(ax, 'on');							% Show grid %
		
		%% Labelling %%
		title(ax, "Selected Spectrum Image");	% Title %
		
		cb = colorbar(ax);						% Colorbar %
		cb.Label.String = "Intensity (arb.)";
		
		xlabel(ax, "X position (px)");			% Axis labels %
		ylabel(ax, "Y position (px)");
		
		xticks(ax, stk_rng);						% Set tick marks %
		yticks(ax, ftk_rng);

		tick_lbl = zeros([length(ptk_rng), 2]);	% Tick labels %
		for tk = 1:length(ptk_rng)
			tick_lbl(tk,:) = ptk_rng(tk) + this.peak_pos - peak_rad - 1;
		end
		
% 		ptk_lbl = zeros([length(ptk_rng), 1]);
% 		stk_lbl = zeros([length(stk_rng), 1]);		% Tick labels %
		
		ptk_lbl = ptk_rng + this.peak_pos - peak_rad - 1;
		stk_lbl = stk_rng + this.spec_rng(1) - 1;
		
% 		for tk = 1:length(stk_rng)
% 			ptk_lbl(tk) = ptk_rng(tk) + this.peak_pos - peak_rad - 1;
% 			stk_lbl(tk) = stk_rng + this.spec_rng(1) - 1;
% 		end
		xticklabels(ax, {stk_lbl});
		yticklabels(ax, {flip(ptk_lbl)});
		
	end
	function DispPlot(this, ax)
	% Displays a plot of the selection, background, signal, and fit of the spectrum
	% of this particle on axes provided by 'ax'.
	%
	% To plot in a separate figure, use:
	% > figure;
	% > [Particle Instance].DispSpec(gca);
	%
	% Additional considerations taken:
	% + Title is "Selected Spectrum"
	% + Axes limits tight to the data
	% + Grid
	% + Axes labels
	% + Wavelength / Energy plotting along the x-axis
	% + Dynamic legend
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Spectrum Plot' axes 
	
		%% Argument Defaults %%
		if(nargin < 2), ax = UI.axs(4); end		% Default to the Spec Plot axes %
		
		%% Refresh %%
		cla(ax, 'reset');
		
		%% Initialization %%
		% Physical constant: hw (eV) = hc / lambda (nm) %
		hc = (6.626E-34 * 2.998E8) * (1E9/1.602E-19);
		
		% Check if we should even plot anything %
		if(isempty(this.spec_plot))
			text(ax, 0.35, 0.5, "Please fit the selected spectra");
			return;
		end
		
		% Make the appropriate xgrid %
		xgrid = Particle.px2nm(this.spec_rng, this.peak_pos(1));
		if(Particle.plotting(1)), xgrid = hc ./ xgrid; end		% Use eVs %
		
		% Initialize the legend %
		leg = {};
		
		%% Plotting %%
		% Plot the data on the xgrid %
		hold(ax, 'on');
		if(plt_params{2})	% Show Selection & Background %
			plot(ax, xgrid, [this.spec_plot.sel, this.spec_plot.bg], '--');
			leg{1} = "Selection";
			leg{2} = "Background";
		end
		if(plt_params{3})	% Show Signal %
			plot(ax, xgrid, this.spec_plot.sig, '--', 'Color', [0.5,0,0.5]);
			leg{end+1} = "Signal";
		end
		plot(ax, xgrid, this.spec_fits.fits, 'k-', 'LineWidth', 1);
		leg{end+1} = "Lorenztian Fit";
		
		hold(ax, 'off');
		
		axis(ax, 'tight');							% Tight axes limits %
		grid(ax, 'on');								% Grid %
		
		% Add in the bells and whistles %
		title(ax, "Spectrum Plot");					% Title %
		
		if(plt_params{1})	% Use eVs %				% Axis labels %
			xlabel(ax, "Energy (eV)");
		else
			xlabel(ax, "Wavelength (nm)");			
		end
		ylabel(ax, "Intensity (arb.)");
			
		legend(ax, leg);							% Legend %
	end
end

%% STATIC METHODS %%
methods(Static)
	function [px] = nm2px(nm, px_0)
		if(nargin < 2), px_0 = 0; end
		px = (nm - Particle.CAL_INTER) / Particle.CAL_SLOPE + px_0;
	end
	function [nm] = px2nm(px, px_0)
		if(nargin < 2), px_0 = 0; end
		nm = Particle.CAL_SLOPE * (px - px_0) + Particle.CAL_INTER;
	end
	
	function [fit, params] = FitLorentzian(grid, data)
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
		bnd_lo = [0E4, 0+grid(1), 0];
		bnd_hi = [1E4, len+grid(1), len];
		
		% The initial value of the nonlinear fit parameters %
		p_0 = [1E3, len/2+grid(1), 20];
		
		%% Optimization %%
		% Let MATLAB take care of this fit! %
		params = lsqnonlin(@(p) LorentzianFxn(p, grid, data), p_0, ...
			bnd_lo, bnd_hi, lsqopt);
		
		% Also return the fit itself %
		fit = LorentzianFxn(params, grid);
		
		function [res] = LorentzianFxn(p, x, y)
			% Compute the value of the Lorentzian at each x given parameters p %
			vals = p(1) ./ ((2*(x-p(2))/p(3)).^2 + 1);
			
			% Return the residual with the data given in y %
			if(nargin < 3)
				res = vals;
			else
				res = (vals - y);
			end
		end
	end
	function [issue, err] = FitIssue(data, fit, thr)
		fit_err = data - fit;
		fit_issue = abs(fit_err) > thr;
		
		issue = sum(fit_issue) > 0.10 * length(data);
		err = fit_err .* fit_issue;
	end
end

%% PERSISTENT VARIABLE METHODS %%
methods(Static)
	% Plotting Options %
	function [value] = pltopt(val, write)
		persistent pltopt;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)				% Spit out the entire array %
			value = pltopt;
		elseif(nargin == 1)			% Read out the specific index given by val % 
			value = pltopt(val);
		else
			% Check if 'write' is a boolean %
			if(islogical(write))	% Write to the whole array %
				pltopt = val;
			else					% Write to just the indices given in 'write' %
				prev = Particle.pltopt;
				prev(write) = val;
				pltopt = prev;
			end
		end
	end
	
	% Fitting Options %
	function [value] = fitval(val, write)
		persistent fitval;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)				% Spit out the entire array %
			value = fitval;
		elseif(nargin == 1)			% Read out the specific index given by val % 
			value = fitval(val);
		else
			% Check if 'write' is a boolean %
			if(islogical(write))	% Write to the whole array %
				fitval = val;
			else					% Write to just the indices given in 'write' %
				prev = Particle.fitval;
				prev(write) = val;
				fitval = prev;
			end
		end
	end
	
	% Image Dimensions %
	function [value] = imgdim(val, write)
		persistent imgdim;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)				% Spit out the entire array %
			value = imgdim;
		elseif(nargin == 1)			% Read out the specific index given by val % 
			value = imgdim(val);
		else
			% Check if 'write' is a boolean %
			if(islogical(write))	% Write to the whole array %
				imgdim = val;
			else					% Write to just the indices given in 'write' %
				prev = Particle.imgdim;
				prev(write) = val;
				imgdim = prev;
			end
		end
	end
end
end