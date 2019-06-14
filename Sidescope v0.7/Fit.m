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
	function Fit_Lorentzian(this, err, num, tol, dis)
	% Fits multiple ('num') Lorentzians to the data provided in 'err' and stores it
	% in this instance's 'curves' and 'params' properties
	%
	%	------------------------------------------------------------------------
	%	Argument Defaults:
	%	~ err
	%	~ num
	%	~ tol
	%	~ dis
		%% Argument Defaults %%
		if(nargin < 2), err = this.curves; end
		if(nargin < 3), num = size(this.params, 2); end
		if(nargin < 4), tol = 1E-6; end
		if(nargin < 5), dis = false; end
		
		%% Initialization %%
		% Initialize the parameters to all zeros %
		param = zeros([3, 1]);
		
		cell_param = {};
		cell_fittot = {};
		cell_fitsep = {};
		
		% Determine the total sum of squares for the coefficient of determination %
		ss_tot = sum((err - mean(err)).^2);
		
		%% Perform Fit %%
		% In doing this fitting process, we will determine the minimum number of
		% Lorentzians to fit to the data in order to achieve the optimal fit.  This
		% number is capped by 'num'.
		figure(200);
		for n = 1:num
			[param, fit_tot, fit_sep] = ...
				Fit.Lorentzian(this.domain, err, n, param, 3, 1);
			
			cell_param{n} = param;
			cell_fittot{n} = fit_tot;
			cell_fitsep{n} = fit_sep;
			
			% Determine the total coefficient of determination %
			ss_res(n) = sum((err - fit_tot).^2);
			r2_tot(n) = 1 - (ss_res(n)/ss_tot);
			
			% Determine each fit's coefficient of determination %
			ss_fit(n) = mean(sum((err - fit_sep).^2, 1));
			r2_sep(n) = 1 - (ss_fit(n)/ss_tot);
			
			subplot(1,num,n);
			plot(this.domain, [err, fit_tot, fit_sep]);
		end
		[r2_tot', r2_sep']
		[ss_res', ss_fit']
		
		(r2_tot .* r2_sep) ./ abs(ss_fit/ss_tot - ss_res/ss_tot)
		%r2_tot ./ sqrt(3*num + (1:num)) %sqrt(2*num + (1:num))
		[~, best_idx] = max((r2_tot .* r2_sep) ./ abs(ss_fit - ss_res));
		
		subplot(1,num,best_idx);
		title("SELECTED");
		%figure(201);
		%plot(this.domain, [err, cell_fittot{best_idx}, cell_fitsep{best_idx}]);
	end
end

%% STATIC METHODS %%
methods(Static)
	%% FITTING %%
	function [params, fit_tot, fit_sep] = ...
			Lorentzian(x, y, num, p_0, p_len, extend, tol, dis)
		%% Argument Defaults %%
		if(nargin < 3), num = 1; end
		if(nargin < 4), p_0 = 0; end
		if(nargin < 5), p_len = 3; end
		if(nargin < 6), extend = 0.20; end
		if(nargin < 7), tol = 1E-6; end
		if(nargin < 8), dis = false; end
		
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
		amp_min = min(y) - extend*range(y);
		amp_max = max(y) + extend*range(y);
		
		% Establish the bounds of the nonlinear fit [x_0, Gamma, (A), (b)] %
		bnd_min = [min(x); 0; amp_min; min(y)] * ones([1, num]);
		bnd_max = [max(x); range(x); amp_max; max(y)] * ones([1, num]);
		
		% Define the initial value of the nonlinear fit parameters - if one is given,
		% use that instead
		init = [mean(x); range(x)/4; mean(y(:)); 0] * ones([1, num]);
		if(sum(abs(p_0(:))) > 0)
			% Use the given parameters for as far as they are included.  If the given
			% parameters exceed 'num' in dimension, then throw the appropriate error.
			init(1:size(p_0,1), 1:size(p_0,2)) = p_0;
		end
		
		%% Nonlinear Least Squares Fitting %%
		% Perform a nonlinear least-squares fit on the Lorentzian Function to find
		% the parameters of best fit.
		params = lsqnonlin( @(p) sum(Fit.Fxn_Lorentzian(p, x, y), 2), ...
			init(1:p_len, :), bnd_min(1:p_len, :), bnd_max(1:p_len, :), lsqopt);
		
		% Additionally, return the fits themselves if asked %
		if(nargout > 1)
			% Compute all fits independently %
			fit_sep = Fit.Fxn_Lorentzian(params, x); 
			
			% If we just want the total fit, sum them together %
			fit_tot = sum(fit_sep, 2);
		end
	end
	
	%% FUNCTIONS %%
	function [res] = Fxn_Lorentzian(p, x, y)
	% Computes the value of a Lorentzian defined by parameters 'p' at each value 
	% of 'x'.  May also compute the difference with some values 'y' to establish
	% a residual 'res'
	%
	% Recall that a general Lorentzian has form:
	%
	%	L(x) = A / [ (2(x-x_0)/Gamma )^2 + 1] + b
	%
	% Where x_0 is the peak offset, Gamma is the Full Width at Half Maximum
	% (FWHM), A is the peak amplitude, and b is the vertical offset.  These
	% parameters are presented in [x_0, Gamma, A, b] order to allow for
	% constructing unit-amplitude Lorentzians with ease.
	% 
	%	--------------------------------------------------------------------
	%	Argument Definitions:
	%	> p:	[x_0, Gamma, A, b] The parameters that define each Lorentzian.
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
end

end