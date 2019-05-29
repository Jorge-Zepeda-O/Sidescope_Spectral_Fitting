classdef Particle < handle
% A dynamic class for handling identified particles.

%% CONSTANT VARIABLES %%
properties(Constant)
	% Physical Constants %
	HC = (6.626E-34 * 2.998E8) * (1E9/1.602E-19);	% hw (ev) = hc / lambda (nm) %
	
	% Spectral calibration %
	CAL_SLOPE = 1.927	% Calibration slope (nm) %
	CAL_INTER = -1.7313 % Calibration intercept (nm) %
	
	% Spectral bounds %
	BND_EV = [1.5, 3.0]								% Bounds in eV %
	BND_NM = Particle.HC ./ flip(Particle.BND_EV)	% Bounds in nm %
	BND_PX = Particle.S_NM2PX(Particle.BND_NM);		% Bounds in px (not offset) %
	
	% Spectral ranges %
	RNG_PX = round(Particle.BND_PX(1)):round(Particle.BND_PX(2));
	RNG_NM = Particle.S_PX2NM(Particle.RNG_PX);
	RNG_EV = Particle.HC ./ Particle.RNG_NM;
end

%% STATIC VARIABLES %%
%	> pltopt:	[bool]	Array of boolean values to determine various plotting options
%		(show_ev, show_bg, show_sig)
%
%	> fitval:	[#]		Array of numbers corresponding to the values of the fitting
%		parameter sliders in MainWin (fit_snr, fit_tol, fit_lor, fit_itr)
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

	%% REFRESH %%
	function REFRESH()
	% Clears all static variables of their values and prepares the class for use
		
		%% Refresh Options %%
		Particle.pltopt([], true);		% Plotting Options %
		Particle.fitval([], true);		% Fitting Options %
	end
end

%% DYNAMIC VARIABLES %%
properties
	%% Peak %%
	peak_pos	% (x; y)	Peak position %
	peak_img	% [x, y]	Image at the peak within a small surrounding window %

	%% Spectrum %%
	spec_img	% [x, y]	Image at the spectrum with a small surrounding window %
	
	spec_plots	% [sel, oth, sig]	Various parts of the integrated spectrum % 
	spec_fits	% [Fit]		Array of Fit objects corresponding to Lorentzians %
	
	pow_noise	% (#)	Value of the noise power
	
	%% Display %%
	peak_lines	% [line] Array of lines that define the box around the peak image %
	spec_lines	% [line] Array of lines that define the box around the spectrum img %
	filt_lines	% [line] Array of lines that show where the selection box is %
end

%% DEPENDENT VARIABLES %%
properties(Dependent)
	%% Strings %%
	str_loc	% "(x, y)"
end
methods
	function [value] = get.str_loc(this)
		value = join(["(", this.peak_pos(1), ",", this.peak_pos(2), ")"]);
	end
end

%% DYNAMIC METHODS %%
methods
	%%%%%% STILL NEEDS WORK %%%%
	%% CONSTRUCTOR %%
	function [obj] = Particle(peak_pos, img_slice)
	% Constructs a Particle object centered at position 'peak_pos' and with a slice
	% of the Frame image 'img' around this peak position.
	%	------------------------------------------------------------------------
	%	Argument Definitions
	%	> peak_pos:	  (x, y)	Location in the Frame of this particle's peak
	%	> img_slice: [[x, y]]	Horizontal slice of the Frame including this peak
	%
	%	------------------------------------------------------------------------
	%	Outputs:
	%	< obj:	(Particle)	The newly created Particle object
	
		%% Argument Passing %%
		% Copy in the peak position given %
		obj.peak_pos = peak_pos';

		%% Parse the Frame Slice %%
		% Make the peak range - use the window radius option %
		peak_rng = -Frame.winval(1):Frame.winval(1);

		% Get the image chunks %
		obj.peak_img = img_slice(:, peak_rng + obj.peak_pos(1));
		obj.spec_img = img_slice(:, Particle.RNG_PX + obj.peak_pos(1));
	end
	
	%% METHODS %%
	function Fit_Spectrum(this, filt_wgt, xrng, spec_bg, pow_noise)
	% Derives the spectrum plot of this particle by comparing a selection and
	% background filter applied to the spectrum image using a soft threshold.  It
	% then calculates the SNR of the spectrum before fitting multiple Lorentzians to
	% the spectrum plot.  It attempts to use the minimal number of Lorentzians to
	% accomplish this, and will correct previously fit Lorentizans in an effort to
	% maximize accuracy.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> filt_wgt: [filt_sel, filt_oth] A pair of filters (Gaussian and inverse 
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

		%% Argument Passing %%
		% Copy in the noise power from the Frame %
		this.pow_noise = pow_noise;
	
		%% Filter the Spectrum Images %%
		% Find out where the background is here %
		bg_loc = sum(xrng == this.RNG_PX' + this.peak_pos(1), 1);

		% Subtract out the background %
		spec_sub = this.spec_img - spec_bg(logical(bg_loc))';
		
		% Obtain the two integrations at once using linear algebra %
		spec_wgt = spec_sub' * filt_wgt;	% (plot, filter #) %
		
		% The signal we're interested in is the soft-threshold between the selected
		% spectrum and the background spectrum, hereby dubbed the signal spectrum
		%spec_sig = max(spec_wgt(:,1) - spec_wgt(:,2), 0);
		spec_sig = spec_wgt(:,1) - spec_wgt(:,2);
		
		% Pass these in to the spec_plots %
		this.spec_plots = [spec_wgt, spec_sig];
		
		%% Multiple Lorentzian Fitting %%		
		% Initialize the vector arrays for the fits and the parameters.  Note that
		% the domain is over the nm range.
		numfits = Particle.fitval(3);
		this.spec_fits = Fit.empty;
		for f = 1:numfits
			this.spec_fits(f) = Fit(Particle.RNG_NM, 3, f == 1);
		end

		% Check if we're only attempting to fit one peak %
		if(numfits == 1)
			% Fit one and done %
			this.spec_fits.Fit_Lorentzian(spec_sig);
		else
			% Repeat for the specified number of iterations... %
			for i = 1:Particle.fitval(4)
				% Repeat for each Lorentzian involved... %
				for l = 1:numfits
					% Get the sum-fit for all *other* Lorentizans %
					fit_other = sum([this.spec_fits([1:numfits] ~= l).curve], 2);

					% Get the error of fit_other with the raw data %				
					[issue, fit_err] = Particle.FitIssue(spec_sig, fit_other, ...
						sqrt(pow_noise) / Particle.fitval(2));
					if(~issue)
						if(~this.spec_fits(l).isActive)
							continue;
						else
							break;
						end
					end

					% Use the fit error to re-fit this Lorentzian %
					this.spec_fits(l).Fit_Lorentzian(fit_err);

					% If this fit doesn't really matter too much, nuke it %
					if(this.spec_fits(l).param(3)^2 < Particle.fitval(1)*pow_noise)
						this.spec_fits(l).isActive = false;
						this.spec_fits(l).Refresh();
					else
						this.spec_fits(l).isActive = true;
					end
				end
				
				% If there's only one Lorentzian left, we've already done our best
				% fit, so break out.
				if(sum([this.spec_fits.isActive]) == 1), break; end
			end
			
			% Weed out the useless Lorenztians %
			this.spec_fits = this.spec_fits([this.spec_fits.isActive]);
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
			spec_edges = this.BND_PX + this.peak_pos(1);

			% For brevity: (1,1) = left, (2,1) = right, (1,2) = bottom, (2,2) = top %
			peak_edges = (this.peak_pos' + [-1,1]'/2 * filt_sz);

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
		
		% Color the boxes and make them visible %
		for e = 1:4
			% Peak box %
			this.peak_lines{e}.Color = color;
			this.peak_lines{e}.Visible = 'on';
			
			% Spectrum box %
			this.spec_lines{e}.Color = color;
			this.spec_lines{e}.Visible = 'on';
		end
	end
end

%% STATIC METHODS %%
methods(Static)
	%% CONVERSIONS %%
	function [px] = S_NM2PX(nm, px_0, ord)
	% Converts a given wavelength (in nm) to the corresponding pixel value based on
	% the calibration.  The pixel offset 'px_0' represents the location of the zeroth
	% order diffraction, and is optional.  Options to include the nth order
	% diffraction position are also included in 'ord'
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> nm:	[#] A vector of values to find the pixel location of the nth order
	%		diffraction of
	%
	%	~ px_0:	(#)	A value that determines how far offset the pixels should be from
	%		the zeroth order diffraction.
	%			(~) Defaults to 0
	%
	%	~ ord:	(#) The order of diffraction to locate
	%			(~)	Defaults to 1
	%	
	%	------------------------------------------------------------------------
	%	Outputs:
	%	< px:	[#] A vector of values corresponding to the pixel locations of the
	%		nth order diffraction
	
		%% Arugment Defaults %%
		if(nargin < 2), px_0 = 0; end	% Default to centered zeroth order %
		if(nargin < 3), ord = 1; end	% Default to first order diffraction %
		
		%% Procedure %%
		% Calculate the offset for one order %
		px = (nm - Particle.CAL_INTER) / Particle.CAL_SLOPE;
		
		% Determine if we need further than the first order %
		if(ord ~= 1)
			% Scale accordingly, it's linear %
			px = ord * px;
		end
		
		% Determine if we need to apply an offset for the zeroth order %
		if(px_0 ~= 0)
			% Offset accordingly %
			px = px + px_0;
		end
	end
	function [nm] = S_PX2NM(px, px_0, ord)
	% Converts a given pixel position to the corresponding wavelength (nm) value 
	% based on the calibration.  The pixel offset 'px_0' represents the location of 
	% the zeroth order diffraction, and is optional.  Options to include the nth 
	% order diffraction position are also included in 'ord'
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> px:	[#] A vector of values to find the wavelength value at the nth order
	%		diffraction
	%
	%	~ px_0:	(#)	A value that determines how far offset the pixels should be from
	%		the zeroth order diffraction.
	%			(~) Defaults to 0
	%
	%	~ ord:	(#) The order of diffraction to locate
	%			(~)	Defaults to 1
	%	
	%	------------------------------------------------------------------------
	%	Outputs:
	%	< px:	[#] A vector of values corresponding to the pixel locations of the
	%		nth order diffraction
	
		%% Arugment Defaults %%
		if(nargin < 2), px_0 = 0; end	% Default to centered zeroth order %
		if(nargin < 3), ord = 1; end	% Default to first order diffraction %
		
		%% Procedure %%
		% Determine if we need to adjust for the zeroth order offset %
		if(px_0 ~= 0)
			% Shift it accordingly %
			px = px - px_0;
		end
		
		% Determine if we are looking at a higher order diffraction %
		if(ord ~= 1)
			% Scale it accordingly %
			px = px / ord;
		end
		
		% Calculate the wavelength for one order %
		nm = Particle.CAL_SLOPE * px + Particle.CAL_INTER;
	end
	
	%% VISUALIZATION %%
	function S_DispPeak(part, ax)
	% Displays the image of the peak of the particle given by 'part' on the axes
	% given by 'ax'.  Defaults to the MainWin's second axes.
	%
	% To plot in a separate figure, use:
	% > figure;
	% > Particle.S_DispPeak([Particle], gca);
	%
	% Additional considerations taken:
	% + Aspect ratio is 1:1
	% + Grid
	% + Title is "Peak Image"
	% + Labeled colorbar with "Intensity (arb.)"
	% + Axes labels
	% + Tick marks are shifted to the (x,y) coordinates of the peak
	%	--------------------------------------------------------------------	
	%	Argument Definitions:
	%	> part:	(Particle) The particle whose information you want to display.
	%			(~) If left empty, will write an error message to the plot
	%
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Peak Image' axes 

		%% Argument Defaults %%
		if(nargin < 1), part = []; end		% Default to the error message %
		if(nargin < 2), ax = UI.axs(2); end	% Default to the PeakImg axes %

		%% Refresh the Axes %%
		cla(ax, 'reset');

		% Determine if there's a particle to plot %
		if(isempty(part))
			% Then output "Please select a particle" to the axes %
			text(ax, 0.25, 0.5, "Please select a particle");

			% We need say no more %
			return;	
		end

		%% Preliminaries %%
		% Create the tick range %
		tick_rng = (0:5:2*Frame.winval(1)) + 1;

		%% Plotting %%
		imagesc(ax, part.peak_img);				% Plot the image %

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

		tick_lbl = tick_rng + part.peak_pos - Frame.winval(1) - 1;	% Tick labels %
		xticklabels(ax, {tick_lbl(1,:)});
		yticklabels(ax, {flip(tick_lbl(2,:))});
	end
	function S_DispSpec(part, ax)
	% Displays the image of the spectrum of the particle specified by 'part' on the 
	% axes specified by 'ax'. 
	%
	% To plot in a separate figure, use:
	% > figure;
	% > Particle.DispSpec([Particle], gca);
	%
	% Additional considerations taken:
	% + Grid
	% + Title is "Spectrum Image"
	% + Labeled colorbar with "Intensity (arb.)"
	% + Axes labels
	% + Tick marks are shifted to the (x,y) coordinates of the spectrum
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> part:	(Particle) The particle whose information you want to display.
	%			(~) If left empty, will write an error message to the plot
	%
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Spectrum Image' axes 
	
		%% Argument Defaults %%
		if(nargin < 1), part = []; end		% Default to the error message % 
		if(nargin < 2), ax = UI.axs(3); end	% Default to the SpecImg axes %
		
		%% Refresh %%
		cla(ax, 'reset');

		% Determine if there's a particle to plot %
		if(isempty(part))
			% Then output "Please select a particle" to the axes %
			text(ax, 0.33, 0.5, "Please select a particle");

			% We need say no more %
			return;	
		end
		
		%% Preliminaries %%
		% Create the tick range %
		ptk_rng = (0:5:2*Frame.winval(1)) + 1;
		
		stk_rng = 1:10:ceil(range(Particle.BND_PX));
		
		%% Plotting %%
		imagesc(ax, part.spec_img);								% Plot the image %
		
		axis(ax, 'image');						% 1:1 aspect ratio %
																% No aspect ratio! %
		grid(ax, 'on');											% Show grid %
		
		%% Labelling %%
		title(ax, "Selected Spectrum Image");					% Title %
		
		cb = colorbar(ax);										% Colorbar %
		cb.Label.String = "Intensity (arb.)";
		
		xlabel(ax, "X position (px)");							% Axis labels %
		ylabel(ax, "Y position (px)");
		
		xticks(ax, stk_rng);									% Tick marks %
		yticks(ax, ptk_rng);
		
		ptk_lbl = ptk_rng + part.peak_pos(2) - Frame.winval(1) - 1;	% Tick labels %
		stk_lbl = stk_rng + round(Particle.BND_PX(1)) + part.peak_pos(1) - 1;

		xticklabels(ax, {stk_lbl});
		yticklabels(ax, {flip(ptk_lbl)});
	end
	function S_DispPlot(part, ax)
	% Displays a plot of the selection, background, signal, and fit of the spectrum
	% of the particle specified by 'part' on axes provided by 'ax'.
	%
	% To plot in a separate figure, use:
	% > figure;
	% > Particle.S_DispPlot([Particle], gca);
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
	%	> part:	(Particle) The particle whose information you want to display.
	%			(~) If left empty, will write an error message to the plot
	%
	%	> ax:	(axes handle) The axes to display the image on
	%			(~) The default will reference the 'Spectrum Plot' axes 
	
		%% Argument Defaults %%
		if(nargin < 1), part = []; end			% Default to the error message % 
		if(nargin < 2), ax = UI.axs(4); end		% Default to the Spec Plot axes %
		
		%% Refresh %%
		cla(ax, 'reset');
		
		%% Initialization %%		
		% Check if we should even plot anything %
		if(isempty(part))
			text(ax, 0.40, 0.5, "Please fit spectra");
			return;
		elseif(isempty(part.spec_plots))
			% Also nix it %
			text(ax, 0.40, 0.5, "Please fit spectra");
			return;
		end
		
		% Make the appropriate xgrid %
		if(Particle.pltopt(1))
			xgrid = part.RNG_EV;	% Use eV %
		else
			xgrid = part.RNG_NM;	% Use nm %
		end
		
		% Initialize the legend %
		leg = {};
		
		%% Plotting %%
		% Plot the data on the xgrid %
		hold(ax, 'on');
		if(Particle.pltopt(6))	% Show Amplitude Threshold %
			fill(ax, [min(xgrid), min(xgrid), max(xgrid), max(xgrid)], ...
				sqrt(Particle.fitval(1)*part.pow_noise) * [-1, 1, 1, -1], ...
				[1, 0.5, 0], 'EdgeColor', 'none', 'FaceAlpha', 0.2);
			leg{end+1} = "Threshold";
		end
		
		if(Particle.pltopt(2))	% Show Selection %
			plot(ax, xgrid, part.spec_plots(:,1), '--', 'Color', [0, 0, 1]);
			leg{end+1} = "Selection";
		end
		
		if(Particle.pltopt(3))	% Show Outliers %
			plot(ax, xgrid, part.spec_plots(:,2), '--', 'Color', [1, 0, 0]);
			leg{end+1} = "Outliers";
		end
		
		if(Particle.pltopt(4))	% Show Signal %
			fill(ax, [xgrid, flip(xgrid)], ...
				[part.spec_plots(:,3)' + sqrt(part.pow_noise) / Particle.fitval(2), ...
				flip(part.spec_plots(:,3)') - sqrt(part.pow_noise) / Particle.fitval(2)], ...
				[0.8, 0, 0.8], 'EdgeColor', 'none', 'FaceAlpha', 0.3, ...
				'HandleVisibility', 'off');
			
			plot(ax, xgrid, part.spec_plots(:,3), 'Color', [0.8, 0, 0.8]);
			leg{end+1} = "Signal";
		end
		
		if(Particle.pltopt(5))	% Show Fit Decomposition %
			plot(ax, xgrid, [part.spec_fits.curve], '-', 'Color', [0, 0.8, 0], ...
				'LineWidth', 2, 'HandleVisibility', 'off');
		end
		
		% Plot the Total Lorenzian Fit %
		plot(ax, xgrid, sum([part.spec_fits.curve], 2), 'k-', 'LineWidth', 2);
		leg{end+1} = "Total Lorenztian Fit";
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
	
	%% Fitting? %%
	function [issue, err] = FitIssue(data, fit, thr)
		fit_err = data - fit;
		fit_issue = abs(fit_err) > thr;
		
		issue = sum(fit_issue) > 0.10 * length(data);
		err = fit_err .* fit_issue;
	end
end


end