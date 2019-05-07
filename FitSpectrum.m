% Fits the spectrum obtained from selected particle using either a single or double
% Lorentzian, based on the data.

%% INITIALIZATION %%
isev = false;

% Calibration Values - for wavelength %
cal_slope = 1.927;
cal_inter =-1.7313;

% From MainWin %
peakimg = MainWin.UserData.peakimg;	% Peak image %
specimg = MainWin.UserData.specimg;	% Spectrum image %
cx = MainWin.UserData.cx; % Center of the peak - y %
cy = MainWin.UserData.cy; % Center of the peak - x %
sy = MainWin.UserData.sy; % Center of the spectrum - x %

% Ranges %
nm_rng = (NM_RNG - cal_inter) / cal_slope;
nm_grid = (floor(nm_rng(1)):floor(nm_rng(2))) + 400;
ev_grid = HC ./ (cal_slope * (nm_grid - 400) + cal_inter);

%% PROCESS SPECTRUM %%
% "Signal" range initialization %
sel_sig = str2double(txt_selsig.String);
sel_grid = -SEL_RAD:SEL_RAD;
sel_wgts = exp(-(sel_grid).^2/(2*sel_sig^2));	% Gaussian window the region %
sel_thresh = str2double(txt_selthr.String) / 100;

% Signal selection %
specplot = sel_wgts * specimg / sum(sel_wgts);			% Signal region %
backplot = (1-sel_wgts) * specimg / sum(1-sel_wgts);	% Background region %

% Obtain the background-subtracted signal using soft thresholding %
sigplot = max(specplot - backplot, 0);

%% LORENTZIAN FITTING %%
corrections = str2double(txt_correct.String);

% Fit the first Lorentzian %
[fit, A, x_0, Gamma] = Fit_Lorentzian(sigplot, nm_grid);

% Determine if the first fit was bad or not %
[issue, errplot] = IsIssue(sigplot, fit, sel_thresh);
if(issue)	% If more than 10% is bad... %
	% Fit a second Lorentzian to them! %
	[fit(2,:), A(2), x_0(2), Gamma(2)] = Fit_Lorentzian(errplot, nm_grid);
	
	% Then use the second fit to correct the first fit, and use the new first fit to
	% correct the second fit.  This should be enough, but the process can iterate	
	for c = 1:corrections
		% Fix fit 1 %
		[issue, errplot] = IsIssue(sigplot, fit(1,:), sel_thresh);
		if(issue)
			[fit(1,:), A(1), x_0(1), Gamma(1)] = Fit_Lorentzian(errplot, nm_grid);
		else
			break
		end
		
		 % Fix fit 2 %
		[issue, errplot] = IsIssue(sigplot, fit(2,:), sel_thresh);
		if(issue)
			[fit(2,:), A(2), x_0(2), Gamma(2)] = Fit_Lorentzian(errplot, nm_grid);
		else
			break
		end
	end
end

%% ANALYSIS %%
isev = tog_useevs.Value;

% Display the fit values %
for lbl = 1:size(fit, 1)
	lbl_Avals(lbl+1).String = sprintf("%4.0f", A(lbl));
	
	if(isev)
		lbl_Xvals(lbl+1).String = sprintf("%1.3feV", HC/x_0(lbl));
		
		GammaeV = HC/(x_0(lbl) - Gamma(lbl)/2) - HC/(x_0(lbl) + Gamma(lbl)/2);
		lbl_Gvals(lbl+1).String = sprintf("%1.3feV", GammaeV);
	else
		lbl_Xvals(lbl+1).String = sprintf("%3.1fnm", x_0(lbl));
		lbl_Gvals(lbl+1).String = sprintf("%2.2fnm", Gamma(lbl));
	end
	
end

% Clear the values if they're not used %
if(size(fit, 1) ~= 2)
	lbl_Avals(end).String = "";
	lbl_Xvals(end).String = "";
	lbl_Gvals(end).String = "";
end

figure(98);
plot(sel_wgts, 1:length(sel_wgts), 1-sel_wgts, 1:length(sel_wgts));
ylim([1, length(sel_wgts)]);

%% VISUALIZATION %%
figure(99);
if(isev)
	plot_grid = ev_grid;
else
	plot_grid = nm_grid;
end

plot(plot_grid, specplot, '.-', 'Color', [0, 0, 1]);
hold on;
plot(plot_grid, backplot, '.-', 'Color', [1, 0, 0]);
plot(plot_grid, sigplot, '.-', 'Color', [0, 0.5, 0]);
plot(plot_grid, sum(fit,1), '-', 'Color', [0.6, 0, 0.6], 'LineWidth', 2);
legend("Selection", "Background", "Signal", "Lorentzian Fit");
hold off;
grid on;
xlim([min(plot_grid), max(plot_grid)]);
ylim([0, inf]);
ylabel("Intensity (arbs.)");
if(isev)
	xlabel("Energy (eV)");
else
	xlabel("Wavelength (nm)");
end

%% HELPER FUNCTIONS %%
function [fit, A, x_0, Gamma] = Fit_Lorentzian(y, xrng)
%{
	The Lorentzian function is given by:
	
	L(x) = 2/pi * Gamma/[ 4(x - x_0)^2 + Gamma^2 ]
		 = 2/pi / Gamma [ ( 2(x-x_0)/Gamma )^2 + 1]
 
	It has the special property that the FWHM is Gamma

	However, for broad and fitting appeal, we need to introduce an amplitude and
	y-intercept term: A, and b respectively.  We can absorb the 2/pi into A:

	L(x) = A / [ (2(x-x_0)/Gamma )^2 + 1] + b

	We constrain such that b = 0, as the background should be subtracted
%}
	tolerance = 1E-6; % Originally 1E-32, but 1E-6 works fine %
	
	Y = length(y);
	x = 1:Y;
	
	% Make the composite bounds on each of the four parameters %
	LB = [-1E2, 0, 0];
	UB = [1E4, Y, Y];
	
	% Initialize the nonlinear least squares optimization %
	lsqopt = optimset('lsqnonlin');
	lsqopt.TolFun = tolerance;
	lsqopt.TolX = tolerance;
	lsqopt.Display = 'off';
	
	p_0 = [1E3, Y/2, 20];
	
	% Perform the optimization %
	P = lsqnonlin(@(p) LorentzianFxn(p, x, y), p_0, LB, UB, lsqopt);
	
	% Assign values %
	A = P(1);
	x_0 = P(2) + xrng(1);	% Because we're fitting to the data, not the nm %
	Gamma = P(3);
	
	% Return the fit itself %
	fit = LorentzianFxn([A, x_0, Gamma], xrng);
end
function [res] = LorentzianFxn(p, x, y)
	% Compute the value of the function at each x given the parameters %
	vals = p(1) ./ ((2*(x-p(2))/p(3)).^2 + 1);
	
	if(nargin < 3)
		% Return the values obtained %
		res = vals;
	else
		% Remove the observed signal to determine the residual at each point %
		res = vals - y;
	end
end

function [issue, err] = IsIssue(sig, fit, thresh)
	prob = (sig - fit) > thresh*max(sig);
	issue = sum(prob) > 0.10*length(prob);
	err = (sig - fit) .* prob;
end