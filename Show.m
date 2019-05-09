classdef Show < handle
% A static class that contains functions for showing images or plots

methods(Static)
	function Image(ax, img, ttl)
	% Function displaying an image 'img' on axes 'ax' and having title 'ttl'.
	% Additionally adjusts the aspect ratio to have 1:1 correspondence, labels the
	% colorbar with "Intensity (arb.)", and sets the y-direction to 'normal'.
	
		% Reset the axes %
		cla(ax, 'reset');
		
		% Plot the new data %
		imagesc(ax, img);
		
		% Apply all the fancy labeling things %
		title(ax, ttl);		% Set the title %
		axis(ax, 'image')	% Set the aspect ratio %
		c = colorbar(ax);	% Makes the colorbar visible %
		c.Label.String = "Intensity (arb.)";	% Labels the colorbar %
		set(ax, 'YDir', 'normal');	% Sets the y-axis direction to that of plot %
		
		grid(ax, 'on');		% Plop on a grid for amusement %
	end
	
	function SpecPlot(ax, part, num, isev)
		
		xgrid = part.grids(:,2-isev);
		fits = size(part.fits, 2);
		
		% Reset the axes %
		cla(ax, 'reset');
		
		% Plot the new data %
		pl = plot(ax, xgrid, [part.spec(:,3:end), part.best_fit]);
		
		% Add the title %
		ttl = join(["Spectrum Plot for Particle", num, "|", ...
			"A* =", sprintf("%4.0f", part.best_params(1)), "|"...
			"x_0* =", sprintf("%3.1f", part.best_params(2)), "nm |" ...
			"FWHM* =", sprintf("%2.2f", part.best_params(3)), "nm"]);
		title(ax, ttl);
		
		% Other goodies %
		xlim(ax, [min(xgrid), max(xgrid)]);
		ylabel(ax, "Intensity (arb.)");
		if(isev)
			xlabel(ax, "Energy (eV)");
		else
			xlabel(ax, "Wavelength (nm)");
		end
		grid(ax, 'on');
		
		% Colors and widths %
		% First plot %
		pl(1).Color = [0,0,1];
		pl(1).LineWidth = 1;
		
		% All fits %
		for f = 1:fits
			pl(f + 1).Color = [0,0,0];
			pl(f + 1).LineWidth = 2;
		end
		
		% Best fit %
		pl(end).Color = [0,0.5,0];
		pl(end).LineWidth = 3;
	
		% The darn legend %
		if(fits == 1)
			pl(2).HandleVisibility = 'off';
			legend(ax, "Signal", "Lorentzian Fit");
		else
			pl(3:end-1).HandleVisibility = 'off';
			legend(ax, "Signal", "Other Lorentzians", "Best Lorentzian Fit");
		end
		
	end
	
	function Box(ax, particle, color)
	% Draws a colored box around where the given particle is and its spectrum %
	
		%% Argument Defaults %%
		if(nargin < 3), color = [1, 0, 0]; end
	
		%% Initialization %%
		% Particle side positions %
		lft = particle.pos(1) - particle.FILT_RAD;
		rgt = particle.pos(1) + particle.FILT_RAD;
		
		% Spectrum side positions %
		slft = particle.spos(1) - particle.SPEC_RAD;
		srgt = particle.spos(1) + particle.SPEC_RAD;
		
		% Particle & Spectrum top/bot positions %
		top = particle.pos(2) - particle.FILT_RAD;
		bot = particle.pos(2) + particle.FILT_RAD;
	
		%% The particle itself %%
		% Sides %
		line(ax, [lft,lft], [top,bot], 'Color', color);
		line(ax, [rgt,rgt], [top,bot], 'Color', color);
		
		% Top/Bot %
		line(ax, [lft,rgt], [top,top], 'Color', color);
		line(ax, [lft,rgt], [bot,bot], 'Color', color);
		
		%% The particle's spectrum %%
		% Sides %
		line(ax, [slft,slft], [top,bot], 'Color', color);
		line(ax, [srgt,srgt], [top,bot], 'Color', color);
		
		% Top/Bot %
		line(ax, [slft,srgt], [top,top], 'Color', color);
		line(ax, [slft,srgt], [bot,bot], 'Color', color);
	end
	function SpecBox(ax, sig, color)
	% Draws a colored box around the first standard deviation in the spectrum image %
	
		%% Argument Defaults %%
		if(nargin < 3), color = [1, 0, 0]; end
	
		%% Initialization %%
		% Box side positions %
		lft = 1;
		rgt = 2*Particle.SPEC_RAD + 1;
		
		% Box top and bottom positions %
		top = Particle.FILT_RAD + 1 + sig;
		cen = Particle.FILT_RAD + 1;
		bot = Particle.FILT_RAD + 1 - sig;
		
		%% Draw the lines %%
		line(ax, [lft,rgt], [top,top], 'Color', color);
		line(ax, [lft,rgt], [cen,cen], 'Color', 'g', 'LineWidth', 2);
		line(ax, [lft,rgt], [bot,bot], 'Color', color);
	end
end
end