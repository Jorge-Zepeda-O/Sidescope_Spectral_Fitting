classdef Fit_old < handle
% A semi-static class for generating and fitting functions.  Contains some
% functionality for organizing fit information

%% DYNAMIC VARIABLES %%
properties
	param	% The fit parameters %
	
	domain	% The domain of the fit curve %
	curve	% The fit curve itself %
	
	isActive	% Is this fit useful? %
end

%% DYNAMIC METHODS %%
methods
	function [obj] = Fit(domain, params, isActive)
		
		%% Arugment Defaults %%
		if(nargin < 3), isActive = true; end	% Default to an active fit %
		
		obj.param = zeros([params, 1]);
		
		obj.domain = zeros([length(domain), 1]);
		obj.domain(:) = domain;
		
		obj.curve = zeros(size(obj.domain));
		
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
				this.param, length(this.param), tol, dis);
		else
			[this.param, this.curve] = Fit.Lorentzian(this.domain, err, ...
				this.param, length(this.param), tol, true);
		end
	end
	
	function Fit_Lorentzians(this, num, err, tol, dis)
		if(nargin < 3), err = 0; end
		if(nargin < 4), tol = 1E-6; end
		if(nargin < 5), dis = false; end
		
		if(sum(err(:)) == 0)
			[this.param, this.curve] = Fit.Lorentzians(this.domain, this.curve, num, ...
				this.param, length(this.param), tol, dis);
		else
			[this.param, this.curve] = Fit.Lorentzians(this.domain, err, num, ...
				this.param, length(this.param), tol, true);
		end
	end
end

%% FITTING METHODS %%
methods(Static)
	function [para, fit] = Lorentzians(x, y, num, p_0, plen, tol, dis)
		if(nargin < 3), num = 1; end
		if(nargin < 4), p_0 = 0; end		% Default
		if(nargin < 5), plen = 3; end		% Default to [x_0, Gamma, A]
		if(nargin < 6), tol = 1E-6; end	% Default to the default tolerance %
		if(nargin < 7), dis = false; end	% Default to not displaying steps %
		
		%% Initialization %%				
		% Initalize the nonlinear least squares optimization options %
		lsqopt = optimset('lsqnonlin');
		lsqopt.TolFun = tol;			% Set up tolerances %
		lsqopt.TolX = tol;
		%lsqopt.MaxFunEvals = 1500;
		if(dis)							% Display iteration steps %
			lsqopt.Display = 'on';
		else
			lsqopt.Display = 'off';
		end
		
		% Establish the bounds of the nonlinear fit [x_0, Gamma, (A), (b)] %
		bnd_min = [min(x),		0,			 min(y)];
		bnd_max = [max(x),	length(x),		 max(y)];
		
		bnd_min = bnd_min' * ones([1, num]);
		bnd_max = bnd_max' * ones([1, num]);
		
		% The initial value of the nonlinear fit parameters %
		if(sum(p_0(:)) == 0)
			p_0 =	  [mean(x),	length(x)/4,	max(y)];
			p_0 = p_0' * ones([1, num]);
		end
		
		%% Nonlinear Least Squares Fitting %%
		% Perform a nonlinear least-squares fit on the Lorentzian Function to find
		% the parameters of best fit.  We would just solve this outright, but it'd be
		% messy to try and form this in a linear-least-squares manner.
		para = lsqnonlin(@(p) Fit.Fxn_Lorentzians(p, x, y), ...
			p_0(1:plen,:), bnd_min(1:plen,:), bnd_max(1:plen,:), lsqopt);
		
		% Additionally, return the fit itself if asked %
		if(nargout > 1), fit = Fit.Fxn_Lorentzians(para, x); end
	end
	function [para, fit] = Lorentzian(x, y, p_0, plen, tol, dis)
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
		if(nargin < 3), p_0 = 0; end		% Default
		if(nargin < 4), plen = 3; end		% Default to [x_0, Gamma, A]
		if(nargin < 5), tol = 1E-6; end	% Default to the default tolerance %
		if(nargin < 6), dis = false; end	% Default to not displaying steps %
		
		%% Initialization %%				
		% Initalize the nonlinear least squares optimization options %
		lsqopt = optimset('lsqnonlin');
		lsqopt.TolFun = tol;			% Set up tolerances %
		lsqopt.TolX = tol;
		lsqopt.MaxFunEvals = 1500;
		if(dis)							% Display iteration steps %
			lsqopt.Display = 'on';
		else
			lsqopt.Display = 'off';
		end
		
		% Establish the bounds of the nonlinear fit [x_0, Gamma, (A), (b)] %
		bnd_min = [min(x),		0,			 min(y),	min(y)	];
		bnd_max = [max(x),	length(x),		 max(y),	max(y)	];
		
		% The initial value of the nonlinear fit parameters %
		if(sum(p_0(:)) == 0)
			p_0 =	  [mean(x),	length(x)/4,	max(y),		0	];
		end
		
		%% Nonlinear Least Squares Fitting %%
		% Perform a nonlinear least-squares fit on the Lorentzian Function to find
		% the parameters of best fit.  We would just solve this outright, but it'd be
		% messy to try and form this in a linear-least-squares manner.
		para = lsqnonlin(@(p) Fit.Fxn_Lorentzian(p, x, y), ...
			p_0(1:plen), bnd_min(1:plen), bnd_max(1:plen), lsqopt);
		
		% Additionally, return the fit itself if asked %
		if(nargout > 1), fit = Fit.Fxn_Lorentzian(para, x); end
	end
	function [para, fit] = Gaussian2(xx, yy, zz)
		
	end
end

%% FUNCTION METHODS %%
methods(Static)
	% 1D %
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
	%	> p:	(x_0, Gamma, A, b) The parameters that define the Lorentzian.
	%
	%	> x:	[#] The values to evaluate the Lorentzian at
	%
	%	~ y:	[#] The values to compare the Lorentzian to, must be the same
	%		dimensions as 'x'
	%
	%	--------------------------------------------------------------------
	%	Outputs:
	%	< res:	[#] The values of the Lorentzian that was evaluated
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
	function [res] = Fxn_Lorentzians(p, x, y)
		% If p is not a 4xK array, make it one %
		if(~ismatrix(p))
			p = reshape(p, 3, []);
		end
		
		% Perform the thing %
		res = sum(p(3,:) ./ ((2*(x-p(1,:))./p(2,:)).^2 + 1), 2);
		
		if(nargin == 3), res = res - y; end
	end
	
	function [res] = Fxn_Gaussian(p, x, y)
		
	end
	
	% 2D %
	function [res] = Fxn_Gaussian_2D(p, x, y)
	%
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> p:	(sig_x, x_0, sig_y, y_0, rho) The parameters that define the Gaussian
	%	
	%	> xx:	[[#, #]]	The grid of values to evaluate the x value
	%	
	%	> yy:	[[#, #]]	The grid of values to evaluate y in, must have the same
	%		dimensions as 'xx'
	%
	%	~ zz:	[[#, #]]	The grid of values to compare the evaluated Gaussian to,
	%		must have the same dimensions as 'xx' and 'yy'
	
		%% Compute the Gaussian Function %%
		% Find the exponent first %
		lnres = (x(:,1) - p(2)).^2 / p(1)^2;
		
		if(length(p) > 2)
			% Add in the y dimension %
			lnres = lnres + (x(:,2) - p(4)).^2 / p(3)^2;
		end
		
		if(length(p) == 5)
			% Add in the correlation term %
			lnres = lnres - (p(5) * (x(:,1)-p(2)).*(x(:,2)-p(4))/(p(1)*p(2)));
			
			% Divide by -1/(2 (1-rho^2)) %
			lnres = lnres / (2*(p(5) - 1));
		else
			% Just divide by -2 %
			lnres = lnres / (-2);
		end
		
		% Exponentiate %
		res = exp(lnres);
	end
	
end
end