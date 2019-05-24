classdef Particle < handle
% A dynamic class for handling identified particles.

%% CONSTANT VARIABLES %%
properties(Constant)
	% Physical Constants %
	HC = (6.626E-34 * 2.998E8) * (1E9/1.602E-19);	% hw (ev) = hc / lambda (nm) %
	
	% Spectral calibration %
	CAL_SLOPE = 1.927	% Calibration slope (nm) %
	CAL_INTER = -1.7313 % Calibration intercept (nm) %
	
	% Spectral ranges %
	BND_EV = [1.5, 3.0]								% Bounds in eV %
	BND_NM = Particle.HC ./ flip(Particle.BND_EV)	% Bounds in nm %
end

%% STATIC VARIABLES %%
%	> pltopt:	[bool]	Array of boolean values to determine various plotting options
%		(show_ev, show_bg, show_sig)
%
%	> fitval:	[#]		Array of numbers corresponding to the values of the fitting
%		parameter sliders in MainWin (fit_snr, fit_lor, fit_itr)
%
%	> imgdim:	[#]		Array of numbers corresponding to the sizes of the various 
%		images associated with this Particle (peak_x, peak_y, spec_x, spec_y)
methods(Static)
	function [value] = pltopt(val, write)	% Plotting Options %
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
	function [value] = fitval(val, write)	% Fitting Options %
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
	function [value] = imgdim(val, write)	% Image Dimensions %
		persistent imgdim;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)					% Spit out the entire array %
			value = imgdim;
		elseif(nargin < 2 || ~write)	% Read out the specific index given by val % 
			value = imgdim(val);
		elseif(write)					% Write to the value
			imgdim = val;
		end
	end
	
	%% REFRESH %%
	function REFRESH()
	% Clears all static variables of their values and prepares the class for use
	
		%% Refresh Values %%
		Particle.imgdim([], true);		% Image Dimensions %
		
		%% Refresh Options %%
		Particle.pltopt([], true);		% Plotting Options %
		Particle.fitval([], true);		% Fitting Options %
	end
end

%% DYNAMIC VARIABLES %%
properties
	isActive	% (bool)	Determines if this Particle is currently selected %
	
	%% Peak %%
	peak_pos	% (x, y)	Peak position %
	peak_rng
	peak_img	% [x, y]	Image at the peak with a small surrounding window %

	%% Spectrum %%
	spec_pos	% (x, y)	Spectrum position %
	spec_rng	% (-, +)	Spectrum range (in px) %
	spec_img	% [x, y]	Image at the spectrum with a small surrounding window %
	
	spec_domain % [nm, ev]	Domain
	spec_plots	% [sel, bg, sig] Various parts of the integrated spectrum % 
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
		spec_rngpx = round(Particle.nm2px(Particle.BND_NM, obj.peak_pos(1)));
		obj.spec_rng = spec_rngpx(1):spec_rngpx(2);
		% Get the image chunks %
		obj.peak_img = img(:,obj.peak_rng);
		obj.spec_img = img(:,obj.spec_rng);
		
		% Write up the listbox string %
		obj.str = join(["Particle @ (", obj.peak_pos(1), ",", ...
			obj.peak_pos(2), ")"]);
	end
	
	%% METHODS %%
	function Fit_Spectrum(this, filt_wgt, pow_noise)
	% Derives the spectrum plot of this particle by comparing a selection and
	% background filter applied to the spectrum image using a soft threshold.  It
	% then calculates the SNR of the spectrum before fitting multiple Lorentzians to
	% the spectrum plot.  It attempts to use the minimal number of Lorentzians to
	% accomplish this, and will correct previously fit Lorentizans in an effort to
	% maximize accuracy.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> filt_wgt: [filt_sel, filt_bg] A pair of filters (Gaussian and inverse 
	%		Gaussian) that are applied to the spectrum image in order to perform
	%		weighted integration.  The first selects regions closer to the middle, 
	%		while the second selects regions away from the middle.
	%		
	%		Yes, I'm very aware that I can subtract the filters and do a single
	%		operation, however this way we get to see what each of the filters does
	%		
	%	> snr:		(#) The Signal to Noise ratio of the observed spectrum.  The SNR
	%		is calculated by: SNR = E[S^2] / var(N), where S is the background-
	%		subtracted signal and N is the additive Gaussian noise present in the 
	%		entire image.  Here we are taking the expected value over the y 
	%		dimension, thereby giving us a wavelength dependent SNR
	%
	%	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	%	Implicit Parameters:
	%	

		%% Filter the Spectrum Images %%
		% Obtain the two integrations at once using linear algebra %
		spec_wgt = this.spec_img' * filt_wgt;	% (plot, filter #) %
		
		% The signal we're interested in is the soft-threshold between the selected
		% spectrum and the background spectrum, hereby dubbed the signal spectrum
		spec_sig = max(spec_wgt(:,1) - spec_wgt(:,2), 0);
		
		% Pass these in to the spec_plots %
		this.spec_plots = [spec_wgt, spec_sig];
		
		%% Multiple Lorentzian Fitting %%
		% Establish the domain now so we don't have to again later %
		this.spec_domain = Particle.px2nm(this.spec_rng, this.peak_pos(1))';
		this.spec_domain(:,2) = Particle.HC ./ this.spec_domain(:,1);
		
		% Initialize the vector arrays for the fits and the parameters %
		this.spec_fits = Fit.empty;
		for f = 1:Particle.fitval(2)
			this.spec_fits(f) = Fit(this.spec_domain(:,1), 3);
		end
		
		% Repeat for the specified number of iterations... %
		for i = 1:Particle.fitval(3)
			% Repeat for each Lorentzian involved... %
			for l = 1:Particle.fitval(2)
				% Get the sum-fit for all other Lorentizans %
				fit_other = this.spec_fits(1:length(this.spec_fits) ~= l);
				sum_other = sum([fit_other.curve], 2);
				%fit_other = sum(fit_fits(:, (1:Particle.fitval(2)) ~= l), 2);
				
				% Get the error of fit_other with the raw data %				
				[issue, fit_err] = Particle.FitIssue(spec_sig, sum_other, 50);
				if(~issue)
					break;
				end
				
				% Use the fit error to re-fit this Lorentzian %
				this.spec_fits(l).Fit_Lorentzian(fit_err);
				
				% If this fit doesn't really matter too much, nuke it %
				if(this.spec_fits(l).param(1) < 100)
					this.spec_fits(l).isActive = false;
					this.spec_fits(l).Reset();
				else
					this.spec_fits(l).isActive = true;
				end
			end
		end
		
		% Weed out the useless Lorenztians %
		this.spec_fits = this.spec_fits([this.spec_fits.isActive]);
		return;		
		%% %% %% %% %%
		% Use the raw background-subtracted signal for the Wavelength-Dependent SNR %
		%snr = (spec_wgt(:,1)/std(spec_wgt(:,1)) + spec_wgt(:,2)/std(spec_wgt(:,2))).^2 ...
		%	./ (pow_noise);
		
		spec_sel = this.spec_img .* (filt_wgt(:,1) * ones([1, size(this.spec_img, 2)]));
		spec_bg = this.spec_img .* (filt_wgt(:,2) * ones([1, size(this.spec_img, 2)]));
		
		max_sel = max(spec_sel, [], 2);
		max_bg = max(spec_bg, [], 2);
		std_img = std(this.spec_img, 0, 2);
		
		%snr = (max_sel - max_bg)./std_img;
		snr = (spec_wgt(:,1) - spec_wgt(:,2)).^2 / var(spec_wgt(:,1) - spec_wgt(:,2));
		%snr2 = (spec_wgt(:,1) + spec_wgt(:,2)).^2 ./ (pow_noise);
		%snrthr = (spec_wgt(:,2).^2) ./ (pow_noise);
		%snr = (spec_wgt(:,1).^2) ./ (pow_noise);
		%snr3 = (spec_sig).^2 / (pow_noise + var(spec_wgt(:,1) - spec_wgt(:,2)));
		
		
		figure(2);
		subplot(3,1,1:2);
		plot(spec_sig);
		xlim([0, 200]);
		
		subplot(3,1,3);
		plot(10*log10(snr));
		%line([0, 200], [1,1]*10*log10(snrthr), 'Color', 'r', 'LineStyle', '--');
		xlim([0, 200]);
		ylim([-20, 20]);
	end
	
	%% VISUALIZATION %%
	function DispBox(this, ax, sel, adj)
		%% Arugment Defaults %%
		if(nargin < 2), ax = UI.axs(1); end		% Default to the Original Img axes %
		if(nargin < 3), sel = false; end		% Default to not selected %
		if(nargin < 4), adj = false; end		% Default to no adjustments %
		
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
		tick_rng = (0:5:2*peak_rad) + 1;
		
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
		tick_lbl = tick_rng' + this.peak_pos - peak_rad - 1;
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
	% To plot in a separate figure (or in your own axes), use:
	% > figure;
	% > [Particle Instance].DispSpec([Axes]);
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
		ptk_rng = (0:5:2*peak_rad) + 1;
		
		spec_sz = length(this.spec_rng);
		stk_rng = 1:10:spec_sz;
		
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
		yticks(ax, ptk_rng);
		
% 		ptk_lbl = zeros([length(ptk_rng), 1]);
% 		stk_lbl = zeros([length(stk_rng), 1]);		% Tick labels %
		
		ptk_lbl = ptk_rng + this.peak_pos(2) - peak_rad - 1;
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
		% Check if we should even plot anything %
		if(isempty(this.spec_plots))
			text(ax, 0.40, 0.5, "Please fit spectra");
			return;
		end
		
		% Make the appropriate xgrid %
		xgrid = this.spec_domain(:, 1+Particle.pltopt(1));	% Use eVs %	
		
		% Initialize the legend %
		leg = {};
		
		%% Plotting %%
		% Plot the data on the xgrid %
		hold(ax, 'on');
		if(Particle.pltopt(2))	% Show Selection & Background %
			plot(ax, xgrid, this.spec_plots(:,1:2), '--');
			leg{1} = "Selection";
			leg{2} = "Background";
		end
		if(Particle.pltopt(3))	% Show Signal %
			plot(ax, xgrid, this.spec_plots(:,3), '--', 'Color', [0.5,0,0.5]);
			leg{end+1} = "Signal";
		end
		plot(ax, xgrid, [this.spec_fits.curve], 'k-', 'LineWidth', 1);
		leg{end+1} = "Lorenztian Fit";
		
		hold(ax, 'off');
		
		axis(ax, 'tight');							% Tight axes limits %
		grid(ax, 'on');								% Grid %
		
		% Add in the bells and whistles %
		title(ax, "Spectrum Plot");					% Title %
		
		if(Particle.pltopt(1))	% Use eVs %				% Axis labels %
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
	
	function [issue, err] = FitIssue(data, fit, thr)
		fit_err = data - fit;
		fit_issue = abs(fit_err) > thr;
		
		issue = sum(fit_issue) > 0.10 * length(data);
		err = fit_err .* fit_issue;
	end
end


end