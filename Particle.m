classdef Particle < handle
properties(Constant)
	% Selection Boxes %
	SPEC_RAD = 100
	
	% Spectral calibration %
	CAL_SLOPE = 1.927	% Calibration slope (nm) %
	CAL_INTER = -1.7313 % Calibration intercept (nm) %
end
properties
	% Peak %
	peak_pos	% (x, y) Peak position %
	peak_img	% [x, y] Image at the peak with a small surrounding window %
	peak_lines	% [line] Array of lines that define the box around the peak image %
	
	% Spectrum %
	spec_pos	% (x, y) Spectrum position %
	spec_img	% [x, y] Image at the spectrum with a small surrounding window %
	spec_plot	% [sel, bg, sig, fit(s)] Various parts of the integrated spectrum % 
	spec_fits
	spec_lines	% [line] Array of lines that define the box around the spectrum img %
	
	% Display %
	str_lbox
end
	
methods
	function [obj] = Particle(peak_pos, img)
		obj.peak_pos = peak_pos;
		
		% The window size is encoded in the dimensions of img %
		filt_rad = size(img, 1);
		
		% Find the position that's the center of the spectrum
		% Current calibration has it for 700nm to be the center 
		% off = (700 - b)/a
		obj.spec_pos = obj.peak_pos;
		obj.spec_pos(1) = obj.spec_pos(1) + round((700 - Particle.CAL_INTER)/Particle.CAL_SLOPE);
		
		% Make the appropriate ranges %
		py_rng = (-floor(filt_rad/2):floor(filt_rad/2)) + obj.peak_pos(1);
		sy_rng = (-obj.SPEC_RAD:obj.SPEC_RAD) + obj.spec_pos(1);
		
		% Get the image chunks %
		obj.peak_img = img(:,py_rng);
		obj.spec_img = img(:,sy_rng);
		
		% Write up the listbox string %
		obj.str_lbox = join(["Particle @ (", obj.peak_pos(1), ",", ...
			obj.peak_pos(2), ")"]);
	end
	function [] = FitSpec(this, parent, param, flt)
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
	
	function DispImg(this, ax_peak, ax_spec)
		%% Acquire some information from the images %%
		% Figure out the dimensions %
		[filt_sz(2), filt_sz(1)] = size(this.peak_img);
		[spec_sz(2), spec_sz(1)] = size(this.spec_img);
		
		% Get the radii %
		filt_rad = floor(filt_sz(1) / 2);
		spec_rad = floor(spec_sz(1) / 2);
		
		% Set up some ranges %
		ftick_rng = 1:5:filt_sz(1);
		stick_rng = 1:10:spec_sz(1);

		%% Peak Image %%
		cla(ax_peak, 'reset');							% Reset axes %
		
		imagesc(ax_peak, this.peak_img);				% Display %
		axis(ax_peak, 'image');							% Set aspect ratio %
		
		title(ax_peak, join(["Peak Image"]));			% Set title %
		cb_peak = colorbar(ax_peak);					% Set colorbar %
		cb_peak.Label.String = "Intensity (arb.)";
		xlabel(ax_peak, "X position (px)");				% Set axis labels %
		ylabel(ax_peak, "Y position (px)");
		
		xticks(ax_peak, ftick_rng);						% Set tick marks %
		yticks(ax_peak, ftick_rng);
		grid(ax_peak, 'on');							% Show a grid %

		ftklbl = zeros([length(ftick_rng), 2]);			% Set the tick labels %
		for tk = 1:length(ftick_rng)
			ftklbl(tk,:) = ftick_rng(tk) + this.peak_pos - filt_rad - 1;
		end
		xticklabels(ax_peak, {ftklbl(:,1)});
		yticklabels(ax_peak, {flip(ftklbl(:,2))});
		
		%% Spectrum Image %%
		cla(ax_spec, 'reset');							% Reset axes %
		
		imagesc(ax_spec, this.spec_img);				% Display %
														% Don't set aspect ratio! %
														
		title(ax_spec, join(["Spectrum Image"]));		% Set title %
		cb_spec = colorbar(ax_spec);					% Set colorbar %
		cb_spec.Label.String = "Intensity (arb.)";
		xlabel(ax_spec, "X position (px)");				% Set axis labels %
		ylabel(ax_spec, "Y position (px)");
		
		xticks(ax_spec, stick_rng);						% Set tick marks %
		yticks(ax_spec, ftick_rng);
		grid(ax_spec, 'on');							% Show a grid %
		
		stklbl = {zeros([length(stick_rng), 1])};		% Set the tick labels %
		for tk = 1:length(stick_rng)
			stklbl{tk} = stick_rng(tk) + this.spec_pos(1) - spec_rad - 1;
		end
		xticklabels(ax_spec, stklbl);
		yticklabels(ax_spec, {flip(ftklbl(:,2))});
	end
	function DispBox(this, ax, color)
		%% Arugment Defaults %%
		if(nargin < 3), color = [1, 0, 0]; end
		
		%% Draw the box %%
		% Check if it doesn't exist yet, and if so, draw it! %
		if(isempty(this.peak_lines))
			% Figure out the dimensions %
			[filt_sz(2), filt_sz(1)] = size(this.peak_img);
			[spec_sz(2), spec_sz(1)] = size(this.spec_img);

			% For brevity: (1,1) = left, (2,1) = right, (1,2) = bottom, (2,2) = top %
			peak_edges = (this.peak_pos + [-1,1]'/2 * filt_sz);
			spec_edges = (this.spec_pos + [-1,1]'/2 * spec_sz);

			%% Peak Box %%
			% Sides %
			this.peak_lines{1} = line(ax, peak_edges(1)*[1,1], peak_edges(3:4));
			this.peak_lines{2} = line(ax, peak_edges(2)*[1,1], peak_edges(3:4));

			% Top/Bottom %
			this.peak_lines{3} = line(ax, peak_edges(1:2), peak_edges(3)*[1,1]);
			this.peak_lines{4} = line(ax, peak_edges(1:2), peak_edges(4)*[1,1]);

			%% Spectrum Box %%
			% Sides %
			this.spec_lines{1} = line(ax, spec_edges(1)*[1,1], spec_edges(3:4));
			this.spec_lines{2} = line(ax, spec_edges(2)*[1,1], spec_edges(3:4));

			% Top/Bottom %
			this.spec_lines{3} = line(ax, spec_edges(1:2), spec_edges(3)*[1,1]);
			this.spec_lines{4} = line(ax, spec_edges(1:2), spec_edges(4)*[1,1]);
		end
		
		% Color the boxes %
		for e = 1:4
			this.peak_lines{e}.Color = color;
			this.spec_lines{e}.Color = color;
		end
	end
	function DispPlt(this, ax)
		cla(ax, 'reset');
		
		plot(ax, [this.spec_plot.sel, this.spec_plot.bg], '--');
		hold(ax, 'on');
		plot(ax, this.spec_plot.sig, '.');
		plot(ax, this.spec_fits.fits, 'k-', 'LineWidth', 1);
		hold(ax, 'off');
		axis(ax, 'tight');
		
		title(ax, "Spectrum Plot");
		
		xlabel(ax, "Wavelength (nm)");				% Set axis labels %
		ylabel(ax, "Intensity (arb.)");
		
		legend(ax, "Selection", "Background", "Signal", "Lorentzian Fit");
	end
end

methods(Static)
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
		
		issue = sum(fit_issue) > 0.00 * length(data);
		err = fit_err .* fit_issue;
	end
end

end