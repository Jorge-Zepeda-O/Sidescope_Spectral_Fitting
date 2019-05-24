classdef Fit < handle
% A semi-static class for generating and fitting functions.  Contains some
% functionality for organizing fit information
properties
	param	% The fit parameters %
	
	domain	% The domain of the fit curve %
	curve	% The fit curve itself %
	
	isActive	% Is this fit useful? %
end

methods
	function [obj] = Fit(domain, params, isActive)
		
		%% Arugment Defaults %%
		if(nargin < 3), isActive = true; end	% Default to an active fit %
		
		obj.param = zeros([params, 1]);
		
		obj.domain = domain;
		obj.curve = zeros(size(domain));
		
		obj.isActive = isActive;
		
	end
	function Refresh(this)
		this.param = 0 * this.param;
		this.curve = 0 * this.curve;
	end
	
	function Fit_Lorentzian(this, err, tol, dis)
		if(nargin < 2), err = 0; end
		if(nargin < 3), tol = 1E-6; end
		if(nargin < 4), dis = false; end
		
		if(err == 0)
			[this.param, this.curve] = Fit.Lorentzian(this.domain, this.curve, ...
			length(this.param), tol, dis);
		else
			[this.param, this.curve] = Fit.Lorentzian(this.domain, err, ...
			length(this.param), tol, dis);
		end
	end
end

%% FITTING METHODS %%
methods(Static)
	function [para, fit] = Lorentzian(x, y, plen, tol, dis)
	% Calculates a Lorentzian fit to the data 'y' along the axis 'x'.  Ability to
	% specify tolerance 'tol' and toggle display of iteration steps also included.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> x:	[#] A vector of values to evaluate the Lorentzian at
	%
	%	> y:	[#] A vector of values that need to be fit to
	%
	%	~ plen:	(#)	The number of parameters to use, value between 2 and 4
	%			(~) Defaults to 3 [x_0, Gamma, A]
	%
	%	~ tol:	(#) The tolerance for nonlinlsq
	%			(~) Defaults to 1E-6
	%
	%	~ dis:	(bool) Determines if nonlinlsq displays iteration steps or not
	%			(~)	Defaults to not displaying iteration steps
	%
	%	------------------------------------------------------------------------
	%	Outputs:
	%	< para:	[#] The parameters of the fit
	%	~ fit:	[#] The Lorentzian function evaluated with the fit parameters
	
		%% Argument Defaults %%
		if(nargin < 3), plen = 3; end		% Default to [x_0, Gamma, A]
		if(nargin < 4), tol = 1E-6; end		% Default to the default tolerance %
		if(nargin < 5), dis = false; end	% Default to not displaying steps %
		
		%% Initialization %%				
		% Initalize the nonlinear least squares optimization options %
		lsqopt = optimset('lsqnonlin');
		lsqopt.TolFun = tol;			% Set up tolerances %
		lsqopt.TolX = tol;
		if(dis)							% Display iteration steps %
			lsqopt.Display = 'on';
		else
			lsqopt.Display = 'off';
		end
		
		% Establish the bounds of the nonlinear fit [x_0, Gamma, (A), (b)] %
		bnd_min = [x(1),				0,		-max(y),-max(y)	];
		bnd_max = [x(end),			length(x),	max(y),	 max(y)	];
		
		% The initial value of the nonlinear fit parameters %
		p_0 =	  [x(round(end/2)),		20,		mean(y),	0	];
		
		%% Nonlinear Least Squares Fitting %%
		% Perform a nonlinear least-squares fit on the Lorentzian Function to find
		% the parameters of best fit.  We would just solve this outright, but it'd be
		% messy to try and form this in a linear-least-squares manner.
		para = lsqnonlin(@(p) Fit.Fxn_Lorentzian(p, x, y), ...
			p_0(1:plen), bnd_min(1:plen), bnd_max(1:plen), lsqopt);
		
		% Additionally, return the fit itself if asked %
		if(nargout > 1), fit = Fit.Fxn_Lorentzian(para, x); end
	end
end

%% FUNCTION METHODS %%
methods(Static)
	function [res] = Fxn_Lorentzian(p, x, y)
	% Computes the value of a Lorentzian defined by parameters 'p' at each value 
	% of 'x'.  May also compute the difference with some values 'y' to establish
	% an error 'err'
	%
	% Recall that a general Lorentzian has form:
	%
	%	L(x) = A / [ (2(x-x_0)/Gamma )^2 + 1] + b
	%
	% Where x_0 is the peak offset, Gamma is the Full Width at Half Maximum
	% (FWHM), A is the peak amplitude, and b is the vertical offset.  These
	% parameters are presented in [x_0, Gamma, A, b] order to allow for
	% constructing a unit-amplitude Lorentzian with ease.
	% 
	%	--------------------------------------------------------------------
	%	Argument Definitions:
	%	> p:	[#] The parameters that define the Lorentzian.  In order,
	%		[x_0, Gamma, A, b].
	%
	%	> x:	[#] The values to evaluate the Lorentzian at
	%
	%	~ y:	[#] The values to compare the Lorentzian to, must be the same
	%		dimensions as 'x'
	%
	%	--------------------------------------------------------------------
	%	Outputs:
	%	< val:	[#] The values of the Lorentzian that was evaluated
	%			[#]	The error with the compared values given in 'y'
		
		%% Compute the Lorentzian Function %%
		% Compute the base Lorentzian %
		res = 1 ./ ((2*(x-p(1))/p(2)).^2 + 1);

		% Determine if we need to scale it or shift it up and down %
		if(length(p) == 3)	% scale by 'A' %
			res = res * p(3);
		end
		if(length(p) == 4)	% shift up by 'b' %
			res = res + p(4);	
		end
		
		%% Determine the Result %%
		% Return the residual if there was data given %
		if(nargin == 3), res = (res - y); end
	end
end
end