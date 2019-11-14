classdef Fit < handle
% A semi-static class for generating and fitting functions.  Contains some
% functionality for organizing fit information

%% DYNAMIC VARIABLES %%
properties
	params	% The fit parameters for each curve %
	
	domain	% The domain of the fit curves %
	curves	% The fit curves themselves %
end

%% DYNAMIC METHODS %%
methods
	%% CONSTRUCTOR %%
	function [obj] = Fit(domain, params, fxns)
		%% Argument Transfer %%
		% Make sure that the domain is long across the first axis - this is useful 
		% when concatenating the results from many Fit objects together
		obj.domain = zeros([length(domain), 1]);
		obj.domain(:) = domain;
		
		%% Initialization %%
		% Set the parameters and the curve results to zero %		
		obj.params = zeros([params, fxns]);		
		obj.curves = zeros([length(obj.domain), fxns]);
	end
	
	%% FIT CALLS %%
	function Fit_Lorentzian(this, fxn, err, num, manual)
	% Fits multiple ('num') Lorentzians to the data provided in 'err' and stores it
	% in this instance's 'curves' and 'params' properties.  The metric for
	% determining how many fits are ideal is based on the coefficient of
	% determination R^2.  We seek for the best total fit while minimizing the number
	% of Lorentzians in that fit.
	%
	%	------------------------------------------------------------------------
	%	Argument Defaults:
	%	~ err
	%	~ num
	%	~ manual
		%% Argument Defaults %%
		if(nargin < 2), err = this.curves; end
		if(nargin < 3), num = size(this.params, 2); end
		if(nargin < 4), manual = false; end
		
		%% Initialization %%
		% Initialize the parameters to all zeros %
		param = zeros([3, 1]);
		
		% Determine the total sum of squares for the coefficient of determination %
		ss_tot = sum((err - mean(err)).^2);
		
		% Zero out some vectors %
		ss_res = zeros([num, 1]);
		r2_res = zeros([num, 1]);
		r2_out = zeros([num, 1]);
		
		%% Perform Fit %%
		% In doing this fitting process, we will determine the minimum number of
		% Lorentzians to fit to the data in order to achieve the optimal fit.  This
		% number is capped by 'num'.
		if(~manual)
			% You know best, so fit 'num' Lorentzians to the data and call it a day %
			[param, ~, fit_sep] = Fit.NonLin(...
				@Fit.Fxn_Lorentzian, this.domain, err, num, param);
			
			% Place the relevant values in this object %
			this.params = param;
			this.curves = fit_sep;
			return;
		end
		
		% Else, we need to determine how many fits to use - we need a place to store
		% everything, so make two cells; one for the parameters and one for the fits
		all_param = cell([num, 1]);
		all_fit_sep = cell([num, 1]);
		
		% Then iterate through all available number of Lorentzians and apply lex
		% parsimoniae (law of simplicity)
		for n = 1:num
			% Firstly, fit the data with the current number of Lorentzians - use the
			% parameters found last time (or the zeroed out initialization) to speed 
			% fitting new peaks by already having fit the previous iteration.
			[param, fit_tot, fit_sep] = Fit.NonLin(...
				@Fit.Fxn_Lorentzian, this.domain, err, n, param);
			
			% Append to the cells that contain the parameters and fits for each 
			% number of Lorentzians
			all_param{n} = param;
			all_fit_sep{n} = fit_sep;
			
			%% Determine Total Coefficient of Determination %%
			% Recall: R^2 = 1 - (ss_res / ss_tot) %
			ss_res(n) = sum((err - fit_tot).^2);
			r2_res(n) = 1 - (ss_res(n)/ss_tot);
			
			%% Determine Fit Importance %%
			% Determine the importance of each fit by looking at 1 - R^2 for the sum
			% of the other fits.  In the case of having only one fit, calculate its
			% R^2 in order to determine its importance
			clear ss_out
			if(n > 1)
				for i = 1:n
					% Sum together all other fits and compute the sum of squares %
					ss_out(i) = sum( (err - sum(fit_sep(:,(1:n)~=i), 2)).^2, 1);
				end
			else
				% Else, compute it as if we just estimated the mean %
				ss_out = ss_tot - sum( (err - fit_sep).^2, 1);
			end
			
			% Introduce a trailoff factor, emphasizing smaller sum of squares (the
			% more accurate fits) over the larger ones using a geometric series.  In
			% this manner, we emphasize the accuracy of the best fit over the lesser
			% ones
			trail_factor = 2;	% The severity of the drop-off %
			trail = trail_factor.^(-1.*(0:n-1));	% The geometric series %
			trail = trail / sum(trail);			% Normalization %
			
			ss_sort = trail * sortrows(ss_out');	% Weighting the sum %
			
			% Compute the R^2 of these outlying sums of squares %
			r2_out(n) = ss_sort / ss_tot;

			%% DEBUG - Plot Fits %%
% 			subplot(1,num,n);
% 			plot(this.domain, [err, fit_tot, fit_sep]);
% 			text(1.6, max(err), sprintf("%4.2f", r2_res(n)), 'FontWeight', 'bold', 'FontSize', 10);
% 			for i = 1:n
% 				text(1.6, max(err)*(1 - 0.1*i), sprintf("%4.2f", ss_out(i) / ss_tot));
% 			end
% 			text(1.8, max(err), sprintf("%4.2f", r2_out(n)/(sqrt(n)+3)), 'FontWeight', 'bold', 'FontSize', 10);
		end
		
		%% Determine Goodness Metric %%
		% We want three things:
		% + Maximize the total coefficient of determination (r2_tot)
		% - Minimize the number of fits						(num   )
		% + Maximize the importance of those fits			(r2_out)
		%
		% The division by sqrt(n)+3 has been found empirically, and may be subject to
		% change in the future.  Be weary.  However, I have found that this works out
		% surprisingly well for most applications, even if I can't give you a sound
		% mathematical reasoning for it.
		metric = r2_res + r2_out ./ (sqrt(1:num)+3)';
		
		% Then we simply select the best number of fits based on this criteria %
		[~, best] = max(metric);
		
		% Pick out the best fits and parameters and place them in this object %
		this.params = all_param{best};
		this.curves = all_fit_sep{best};
	end
	function Fit_Peaks(this, peakfxn, err, num, manual)
	% Fits multiple ('num') peaks to the data provided in 'err' and stores it in this
	% instance's 'curves' and 'params' properties.  The metric for determining how 
	% many fits are ideal is based on the coefficient of determination R^2.  We seek 
	% for the best total fit while minimizing the number of peaks in that fit.
	%
	%	------------------------------------------------------------------------
	%	Argument Defaults:
	%	~ err
	%	~ num
	%	~ manual
		%% Argument Defaults %%
		if(nargin < 3), err = this.curves; end
		if(nargin < 4), num = size(this.params, 2); end
		if(nargin < 5), manual = false; end
		
		%% Initialization %%
		% Initialize the parameters to all zeros %
		param = zeros([4, 1]);
		
		% Determine the total sum of squares for the coefficient of determination %
		ss_tot = sum((err - mean(err)).^2);
		
		% Initialize the goodness metric for all values %
		metric = zeros([num, 1]);

		%% Perform Fit %%
		% In doing this fitting process, we will determine the minimum number of
		% peaks to fit to the data in order to achieve the optimal fit.  This
		% number is capped by 'num'.
		if(~manual)
			% You know best, so fit 'num' peaks to the data and call it a day %
			[param, ~, fit_sep] = Fit.NonLin(peakfxn, this.domain, err, num, param);
			
			% Place the relevant values in this object %
			this.params = param;
			this.curves = fit_sep;
			return;
		end
		
		% Else, we need to determine how many fits to use - we need a place to store
		% everything, so make two cells; one for the parameters and one for the fits
		all_param = cell([num, 1]);
		all_fit_sep = cell([num, 1]);
		
		% Then iterate through all available number of peaks and apply lex
		% parsimoniae (law of simplicity)
		for n = 1:num
			% Firstly, fit the data with the current number of peaks - use the
			% parameters found last time (or the zeroed out initialization) to speed 
			% fitting new peaks by already having fit the previous iteration.
			[param, fit_tot, fit_sep] = Fit.NonLin(peakfxn, this.domain, err, n, param);
			
			% Append to the cells that contain the parameters and fits for each 
			% number of Lorentzians
			all_param{n} = param;
			all_fit_sep{n} = fit_sep;
			
			%% Determine Total Coefficient of Determination %%
			% Recall: R^2 = 1 - (ss_res / ss_tot) %
			ss_res = sum((err - fit_tot).^2);
			r2_res = 1 - (ss_res/ss_tot);
			
			%% Determine Fit Importance %%
			% Determine the importance of each fit by looking at 1 - R^2 for the sum
			% of the other fits.  In the case of having only one fit, calculate its
			% R^2 in order to determine its importance
			clear ss_out
			if(n > 1)
				for i = 1:n
					% Sum together all other fits and compute the sum of squares %
					ss_out(i) = sum( (err - sum(fit_sep(:,(1:n)~=i), 2)).^2, 1);
				end
			else
				% Else, compute it as if we just estimated the mean %
				ss_out = ss_tot - sum( (err - fit_sep).^2, 1);
			end
			
			% Introduce a trailoff factor, emphasizing smaller sum of squares (the
			% more accurate fits) over the larger ones using a geometric series.  In
			% this manner, we emphasize the accuracy of the best fit over the lesser
			% ones
			trail_factor = 2;						% The severity of the drop-off %
			trail = trail_factor.^(-1.*(0:n-1));	% The geometric series %
			trail = trail / sum(trail);				% Normalization %
			
			ss_sort = trail * sortrows(ss_out');	% Weighting the sum %
			
			% Compute the R^2 of these outlying sums of squares %
			r2_out = ss_sort / ss_tot;
			
			%% Determine Goodness Metric %%
			% We want three things:
			% + Maximize the total coefficient of determination (r2_res)
			% - Minimize the number of fits						(   n  )
			% + Maximize the importance of those fits			(r2_out)
			%
			% Below we develop a rather interesting way to hit these goals.
			% Subtracting 'r2_res' from one means that we need to flip the polarity
			% once again by dividing by this difference.  Then, it's just a matter of
			% what powers go where.  This seemed to work out rather well.
			% 
			% Additionally, we take advantage of the fact that we're thresholding in
			% the fits - the amplitude barrier drops the more fits are added, but
			% this ensures that the fewer fits are more likely to hit important new
			% things rather than tweak what's already working
			metric_res = sqrt(1 - r2_res);
			metric_out = (r2_out) / n;
			metric(n) = metric_out / metric_res;
		end
		
		% Then we simply select the best number of fits based on this criteria %
		[~, best] = max(metric);
		
		% Pick out the best fits and parameters and place them in this object %
		this.params = all_param{best};
		this.curves = all_fit_sep{best};
	end
end

%% STATIC METHODS %%
methods(Static)
	%% FITTING %%
	function [params, fit_tot, fit_sep] = ...
			NonLin(fit_fxn, x, y, num, p_0, p_len, extend, tol, dis)
		%% Argument Defaults %%
		if(nargin < 3), y = zeros(size(x));	end
		if(nargin < 4), num = 1; end
		if(nargin < 5), p_0 = 0; end
		if(nargin < 6), p_len = size(p_0, 1); end
		if(nargin < 7), extend = (0.5)/num; end
		if(nargin < 8), tol = 1E-6; end
		if(nargin < 9), dis = false; end
		
		%% Initialization %%
		% Initialize the nonlinear least squares optimization options %
		lsqopt = optimset('lsqnonlin');
		
		lsqopt.TolFun = tol;	% Tolerance %
		lsqopt.TolX = tol;
		
		if(dis)
			lsqopt.Display = 'iter';
		else
			lsqopt.Display = 'off';
		end
		
		% Get the upper and lower bounds on the amplitude %
		amp_min = min(y) + extend*range(y);	% You read that right, extend it up %
		amp_max = max(y) + extend*range(y);
		
		% Establish the bounds and initial value of the nonlinear fit %
		switch(fit_fxn)
			case "Gaussian"		% [mu, sigma, (A), (b)] %
				bnd_min = [min(x);	0;				amp_min;	min(y)];
				bnd_max = [max(x);	range(x);		amp_max;	max(y)];
				ini_val = [mean(x);	sqrt(range(x));	mean(y);	0];
				fit_fxn = @Fit.Fxn_Gaussian;
				
			case "Lorentzian"	% [mu, Gamma, (A), (b)] %
				bnd_min = [min(x);	0;				amp_min;	min(y)];
				bnd_max = [max(x);	range(x);		amp_max;	max(y)];
				ini_val = [mean(x);	range(x)/8;		mean(y);	0];
				fit_fxn = @Fit.Fxn_Lorentzian;
				
			case "Gaurentzian"	% [eta, mu, fwhm, (A), (b)] %
				bnd_min = [0;	min(x);		0;				amp_min;	min(y)];
				bnd_max = [1;	max(x);		range(x);		amp_max;	max(y)];
				ini_val = [0.5;	mean(x);	range(x)/8;		mean(y);	0];
				fit_fxn = @Fit.Fxn_Gaurentzian;
		end
		% Make these the same across all fits %
		bnd_min = bnd_min * ones([1, num]);
		bnd_max = bnd_max * ones([1, num]);
		ini_val = ini_val * ones([1, num]);
		
		% If an initial value for the parameters is given, use that %
		if(sum(abs(p_0(:))) > 0)
			% Use the given parameters for as far as they are included.  If the given
			% parameters exceed 'num' in dimension, then throw the appropriate error.
			ini_val(1:size(p_0,1), 1:size(p_0,2)) = p_0;
		elseif(isscalar(p_0))
			% Else default to the maximum number of parameters %
			p_len = size(ini_val, 1);
		end
		
		%% Nonlinear Least Squares Fitting %%
		% Perform a nonlinear least-squares fit on the peak function to find the 
		% parameters of best fit.
		params = lsqnonlin( @(p) sum(fit_fxn(p, x, y), 2), ...
			ini_val(1:p_len, :), bnd_min(1:p_len, :), bnd_max(1:p_len, :), lsqopt);
		
		% Additionally, return the fits themselves if asked %
		if(nargout > 1)
			% Compute all fits independently %
			fit_sep = fit_fxn(params, x); 
			
			% If we just want the total fit, sum them together %
			fit_tot = sum(fit_sep, 2);
		end
	end
	
	%% FUNCTIONS %%
	function [res] = Fxn_Gaussian(p, x, y)
	% Computes the value of a linear combination of multiple Gaussians defined by
	% parameters 'p' over domain 'x'.  May also compute the difference with some
	% values 'y' to establish a residual 'res'.
	%
	% The Gaussian function is modeled as:
	%	G(x) = A * exp(- (x - mu).^2 / (2*sigma.^2) ) + b
	%
	% where A is the amplitude of the Gaussian, mu is the peak position, sigma is the
	% standard deviation of the Gaussian, and b is the vertical offset.  These
	% parameters are presented in [mu, sigma, A, b] order to allow for construction
	% unit-amplitude Gaussians with ease.
	% 
	%	--------------------------------------------------------------------
	%	Argument Definitions:
	%	> p:	[mu, sigma, A, b] The parameters that define each Gaussian.
	%
	%	> x:	[#] The values to evaluate the Gaussians at
	%
	%	~ y:	[#] The values to compare the Gaussians to, must be the same
	%		dimensions as 'x'
	%			(~) Default: 0
	%
	%	--------------------------------------------------------------------
	%	Outputs:
	%	< res:	[#] The values of the Gaussian that was evaluated
	%			[#]	The error with the compared values given in 'y'
	
		%% Argument Defaults %%
		if(nargin < 3), y = zeros(size(x)); end
	
		%% Compute the Gaussian %%
		% Compute the base Gaussian (position & standard deviation) %
		res = exp( -(x - p(1,:)).^2 ./(2*p(2,:).^2) );
		
		% If applicable, introduce separate scaling factors %
		if(size(p, 1) > 2), res = res .* p(3,:); end
		
		% If applicable, introduce separate offsets %
		if(size(p, 1) > 3), res = res + p(4,:); end
		
		%% Determine Residual %%
		% Subtract out the values to compare the Gaussians to, if any %
		res = res - y/size(p,2);
	end
	function [res] = Fxn_Lorentzian(p, x, y)
	% Computes the value of a Lorentzian defined by parameters 'p' at each value 
	% of 'x'.  May also compute the difference with some values 'y' to establish
	% a residual 'res'
	%
	% Recall that a general Lorentzian has form:
	%
	%	L(x) = A / [ (2(x-mu)/Gamma )^2 + 1] + b
	%
	% Where mu is the peak offset, Gamma is the Full Width at Half Maximum
	% (FWHM), A is the peak amplitude, and b is the vertical offset.  These
	% parameters are presented in [mu, Gamma, A, b] order to allow for
	% constructing unit-amplitude Lorentzians with ease.
	% 
	%	--------------------------------------------------------------------
	%	Argument Definitions:
	%	> p:	[mu, Gamma, A, b] The parameters that define each Lorentzian.
	%
	%	> x:	[#] The values to evaluate the Lorentzians at
	%
	%	~ y:	[#] The values to compare the Lorentzians to, must be the same
	%		dimensions as 'x'
	%			(~) Default: 0
	%
	%	--------------------------------------------------------------------
	%	Outputs:
	%	< res:	[#] The values of the Lorentzian that was evaluated
	%			[#]	The error with the compared values given in 'y'
	
		%% Argument Defaults %%
		if(nargin < 3), y = zeros(size(x)); end
	
		%% Compute the Lorentzian %%
		% Compute the base Lorentizan (position & FWHM) %
		res = 1 ./ ((2*(x - p(1,:))./p(2,:)).^2 + 1);
		
		% If applicable, introduce separate scaling factors %
		if(size(p, 1) > 2), res = res .* p(3,:); end
		
		% If applicable, introduce separate offsets %
		if(size(p, 1) > 3), res = res + p(4,:); end
		
		%% Determine Residual %%
		% Subtract out the values to compare the Lorentzians to, if any %
		res = res - y/size(p,2);
	end
	function [res] = Fxn_Gaurentzian(p, x, y)
	% Computes the value of a linear combination of a Gaussian and Lorentzian defined
	% by parameters 'p' over domain 'x'.  May also compute the difference with some 
	% values 'y' to establish a residual 'res'.
	%
	% The combined Gaussian and Lorentzian function is modeled as:
	%	V(x) = A * [ eta L(x, mu, fwhm) + (1-eta) G(x, mu, fwhm) ] + b
	% where
	%	G(x, mu, fwhm) = exp(-(x-mu)^2/(2 [fwhm/2sqrt(2ln2)]^2))
	%	L(x, mu, fwhm) = 1./(1 + ( (x-mu)/[fwhm/2] )^2)
	%
	% Where eta is the Lorentzian percentage (0 <= eta <= 1) and tells how "in focus"
	% the spectrum is, mu is the peak offset, fwhm is the Full Width at Half Maximum 
	% (FWHM), A is the peak amplitude, and b is the vertical offset.  These 
	% parameters are presented in [eta, mu, fwhm, A, b] order to allow for 
	% constructing unit-amplitude Gaurentzians with ease.
	% 
	%	--------------------------------------------------------------------
	%	Argument Definitions:
	%	> p:	[eta, mu, fwhm, A, b] The parameters that define each Gaurentzian.
	%
	%	> x:	[#] The values to evaluate the Lorentzians at
	%
	%	~ y:	[#] The values to compare the Lorentzians to, must be the same
	%		dimensions as 'x'
	%			(~) Default: 0
	%
	%	--------------------------------------------------------------------
	%	Outputs:
	%	< res:	[#] The values of the Gaurentzian that was evaluated
	%			[#]	The error with the compared values given in 'y'
	
		%% Argument Defaults %%
		if(nargin < 3), y = zeros(size(x)); end
	
		%% Function Handles %%
		% For readability and speed later %
		persistent L;
		persistent G;
		if(isempty(L))
			L = @(x, mu, fwhm) 1./(1 + ( (x-mu)./(fwhm/2) ).^2);
			G = @(x, mu, fwhm) exp(-(x-mu).^2./(2*(fwhm/(2*sqrt(2*log(2)))).^2));
		end
		
		%% Compute the Gaurentzian %%
		% Compute the base Gaurentzian (ratio, position, and FWHM) %
		res = p(1,:).*L(x, p(2,:), p(3,:)) + (1-p(1,:)).*G(x, p(2,:), p(3,:));
	
		% If applicable, introduce separate scaling factors %
		if(size(p, 1) > 3), res = res .* p(4,:); end
		
		% If applicable, introduce separate offsets %
		if(size(p, 1) > 4), res = res + p(5,:); end
		
		%% Determine Residual %%
		% Subtract out the values to compare the Gaurentzians to, if any %
		res = res - y/size(p,2);
	end
	
end

end