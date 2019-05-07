classdef Select < handle	
	
% properties(Constant)
% 	sel_rad = 10	% Selection radius %
% 	sel_rng = -sel_rad:sel_rad	% Selection Range %
% 	
% 	spc_off = 713	% Spectrum offset %
% 	spc_rad	= 100	% Spectrum radius %
% 	spc_rng = -spc_rad:spc_rad	% Spectrum Range %
% end

methods(Static)
	function Particle(parent, sel_rad, spc_off, spc_rad)
		%% Initialize %%		
		% Make some ranges %
		sel_rng = -sel_rad:sel_rad;	% Selection Range %
		spc_rng = -spc_rad:spc_rad;	% Spectrum Range %
		
		% Get the Image, its Axes, and the image size %
		Img = parent.UserData.Img;
		ImgAxes = parent.UserData.ImgAxes;
		[X, Y] = size(Img);
		
		% Make the axes for the three plots we need to create %
		if(~isfield(parent.UserData, 'selAxes'))
			selAxes = axes('OuterPosition', [0.00, 0.00, 0.25, 0.25]);
		else
			selAxes = parent.UserData.selAxes;
		end
		if(~isfield(parent.UserData, 'peakAxes'))
			peakAxes = axes('OuterPosition', [0.25, 0.00, 0.25, 0.25]);
		else
			peakAxes = parent.UserData.peakAxes;
		end
		if(~isfield(parent.UserData, 'specAxes'))
			specAxes = axes('OuterPosition', [0.50, 0.00, 0.50, 0.25]);
		else
			specAxes = parent.UserData.specAxes;
		end
		
		% If not already showing, re-show the image %
		Select.PlotImg(parent.UserData.ImgAxes, Img, "Original Image");
		
		%% Acquire particle position %
		% Determine the (x,y) position of the user's selection and store it in the
		% figure's UserData
		[parent.UserData.py, parent.UserData.px] = ginput(1);
		
		% Load in the particle and background locations - making sure they're within
		% the image bounds first, of course.
		px = min(max(round(parent.UserData.px), sel_rad+1), X-sel_rad);
		py = min(max(round(parent.UserData.py), sel_rad+1), Y-sel_rad);
		
		%% Find the peak and spectra images %%
		% Search for the peak in a box around our particle position %
		selimg = Img(sel_rng+px, sel_rng+py);
		[cx, cy] = find(selimg == max(selimg(:)));
		
		% Make sure we've got the mean if there's more than one peak %
		if(length(cx) > 1 || length(cy) > 1)
			cx = round(mean(cx(:)));
			cy = round(mean(cy(:)));
		end
		
		% Move these down so it's based around (px,py) %
		cx = px + cx - sel_rad - 1;
		cy = py + cy - sel_rad - 1;
		
		% Capture the peak and spectra images %
		peakimg = Img( sel_rng+cx, sel_rng+cy );
		specimg = Img( sel_rng+cx, spc_rng+cy+spc_off );
		
		% Copy in the center positions to the UserData %
		parent.UserData.cx = cx;
		parent.UserData.cy = cy;
		parent.UserData.sy = cy + spc_off;
		
		% Copy in the images to the UserData %
		parent.UserData.peakimg = peakimg;
		parent.UserData.specimg = specimg;
		
		%% Visualization %%
		% Refresh the old Image Axes %
		Select.PlotImg(ImgAxes, Img, "Original Image");
		
		% Show the selected 10x10 area %
		Select.PlotImg(selAxes, selimg, "Image of Selection");
		parent.UserData.selAxes = selAxes;

		% Find and plot the associated spectra with this point %
		Select.PlotImg(peakAxes, peakimg, "Image of Peak");
		parent.UserData.peakAxes = peakAxes;
		Select.DrawBox(ImgAxes, [cy, cx], [-1,1]*sel_rad, [-1,1]*sel_rad);

		% Find and plot the associated spectra with this point %
		Select.PlotImg(specAxes, specimg, "Image of Spectrum");
		axis(specAxes, 'normal');
		parent.UserData.specAxes = specAxes;
		Select.DrawBox(ImgAxes, [cy+spc_off, cx], [-1,1]*sel_rad, [-1,1]*spc_rad);
		
		% Plop in all the boxes to the image axes %
		parent.UserData.ImgAxes = ImgAxes;
	end
	
	function DrawBox(ax, cen, yb, xb)
		line(ax, [xb(1),xb(1)]+cen(1), [yb(1),yb(2)]+cen(2), 'Color', 'r');
		line(ax, [xb(2),xb(2)]+cen(1), [yb(1),yb(2)]+cen(2), 'Color', 'r');
		line(ax, [xb(1),xb(2)]+cen(1), [yb(1),yb(1)]+cen(2), 'Color', 'r');
		line(ax, [xb(1),xb(2)]+cen(1), [yb(2),yb(2)]+cen(2), 'Color', 'r');
	end
	function PlotImg(ax, img, ttl)
		% Reset the axes %
		cla(ax, 'reset');
		
		% Plot the data %
		imagesc(ax, img);
		title(ax, ttl);		% Set title %
		axis(ax, 'image');	% Make each pixel square %
		c = colorbar(ax);	% Make the colorbar visible %
		c.Label.String = "Intensity (arb.)";
		set(ax, 'YDir', 'normal');	% Set the y-axis direction to be normal %
	end
end
end


