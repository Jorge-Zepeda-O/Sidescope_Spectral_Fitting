% Conventions followed throughout this project:
%
% (#)		- A (1 x 1) single value
% ("str")	- A (1 x 1) single string
% ('chr')	- A (1 x 1) single character value
%
% [#]		- A (N x 1) vector of unrelated values - meanings listed in description
% [x]		- A (N x 1) vector of related values corresponding to "x"
% {x}		- A (N x 1) cell of related values corresponding to "x"
%
% [x, y]	- A (N x 2) array of related values corresponding to "x" and "y"
% [[x, y]]	- A (N x M) array of related values, where "x" is (N x 1) and 
%				"y" is (M x 1)
%
% (Obj)		- A (1 x 1) single object
% [Obj]		- A (N x 1) vector of related objects
% {Obj}		- A (N x 1) cell of related objects

%% REFRESH %%
% Clear the static variables %
UI.REFRESH;
Frame.REFRESH;
Particle.REFRESH;

%% MAINWIN %%
% Here we will set up the main window that we'll be using everything for %
MainWin = figure('Name', 'Sidescope Pro', 'Menubar', 'none', ...
				 'NumberTitle', 'off', 'DoubleBuffer', 'on', ...
				 'Position', [100, 100, 1800, 900]);
			 
% Set up the various parameters we'll need %
MainWin.UserData.DEBUG = false;

MainWin.UserData.Plotting = [false, true, true];	% Use Evs, Show Sel, Show Sig %
 
% Add in a menu for toggleable parameters / quick functions %
menu_file = uimenu(MainWin, 'Text', 'File');
	UI.MakeMenu(MainWin, menu_file, "Load Image", @menu_load_OnClick);
	
menu_plt = uimenu(MainWin, 'Text', 'Plotting');
	UI.MakeMenu(MainWin, menu_plt, "Use eVs", @menu_plot_OnClick, 1);
	UI.MakeMenu(MainWin, menu_plt, "Show Filter Spectra", @menu_plot_OnClick, 2, true, true);
	UI.MakeMenu(MainWin, menu_plt, "Show Signal Spectrum", @menu_plot_OnClick, 3, true);
			 
% Make the axes we will be using %
UI.MakeAxes(MainWin, [0.00, 0.20, 0.40, 0.80], "Original Image", ...
	"X (px)", "Y (px)");

UI.MakeAxes(MainWin, [0.375, 0.75, 0.20, 0.25], "Selected Peak Image", ...
	"X (px)", "Y (px)");
UI.MakeAxes(MainWin, [0.55, 0.75, 0.475, 0.25], "Selected Spectrum Image", ...
	"X Position (px)", "Y (px)");

UI.MakeAxes(MainWin, [0.55, 0.25, 0.45, 0.50], "Selected Spectrum", ...
	"Wavelength (nm)", "Intensity (arb.)");

%% UI CONTROLS %%
% Buttons %
UI.MakeButton(MainWin, [0.05, 0.10, 0.10, 0.05], "Zoom", 'zoom', true, false);
UI.MakeButton(MainWin, [0.05, 0.05, 0.10, 0.05], "Pan", 'pan', true, false);

UI.MakeButton(MainWin, [0.20, 0.10, 0.15, 0.05], "Select Particles", ...
	@sel_part_OnClick, false, false);
UI.MakeButton(MainWin, [0.20, 0.05, 0.15, 0.05], "Fit Selection", ...
	@fit_part_OnClick, false, false);

UI.MakeButton(MainWin, [0.80, 0.10, 0.10, 0.05], "Export Data", ...
	@export_OnClick, false, false);

% Frame Slider %
UI.MakeFrameSlider(MainWin, [0.05, 0.15, 0.30, 0.05], "Frame Control", ...
	[1, 1, 1], @sld_frame_OnValueChanged, false);

% Fit Parameter Sliders %
UI.MakeParamSlider(MainWin, [0.40, 0.10, 0.15, 0.05], "Window Radius", ...
	[5, 10, 15, 0.1, 0.2], @ctrl_psld_OnValueChanged, 1, true, '%2.0f(px)', @(val) val);
UI.MakeParamSlider(MainWin, [0.40, 0.05, 0.15, 0.05], ['Filter ', char(963)], ...
	[1, 3, 6], @ctrl_psld_OnValueChanged, 2, false, '%4.2f(px)', @(val) val);

UI.MakeParamSlider(MainWin, [0.60, 0.15, 0.15, 0.05], "SNR Threshold", ...
	[1, 3, 10, 1/90, 1/9], @ctrl_psld_OnValueChanged, 3, false, '%5.1f', @(val) val^2);
UI.MakeParamSlider(MainWin, [0.60, 0.10, 0.15, 0.05], "Max Lorentzians", ...
	[1, 2, 5, 0.25, 0.5], @ctrl_psld_OnValueChanged, 4, true, '%1.0f', @(val) val);
UI.MakeParamSlider(MainWin, [0.60, 0.05, 0.15, 0.05], "Fit Iterations", ...
	[0, 10, 20, 0.05, 0.25], @ctrl_psld_OnValueChanged, 5, true, '%2.0f', @(val) val);

% List Boxes %
UI.MakeListbox(MainWin, [0.42, 0.30, 0.12, 0.40], "Found Particles", ...
	@found_part_OnValueChanged, false);

%% UI CALLBACKS %%
function menu_load_OnClick(parent, ~, ~)
% Callback function that prompts the user to select one or more TIFF files from a
% folder, then parses through them creating multiple Frame objects.  It selects the
% first Frame and displays it in the 'Original Image' axes.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.

	%% Refresh %%
	% Wipe the memory of the parent from having any Frames - do note that this also
	% wipes any memory of found / fit Particles, as they are stored in each
	% individual Frame.
	parent.UserData.Frames = Frame.empty;

	%% File Search %%
	% Prompt the user to select one or more files %
	[file_names, file_folder] = uigetfile({'*.tif'; '*.mptiff'}, 'MultiSelect', 'on');
	
	% Determine if the user cancelled the prompt and return %
	if(isequal(file_names, 0)), return; end
	
	% Make the filenames into a cell if they aren't already %
	if(~iscell(file_names)), file_names = {file_names}; end
	
	%% File Loading %%
	% Create a waitbar for this time-intensive process - initialize with the string
	% of what we're doing first
	wb = waitbar(0, "Parsing Frame Information...");
	wb.Children.Title.Interpreter = 'none';	% Allows underscores to be underscores %
	
	% Determine how many frames we have in total, as well as how many frames we have
	% per file.  This becomes useful for making a waitbar when loading in many files
	% or very large files with many frames.
	file_frames = zeros([1, length(file_names)]);
	for file = 1:length(file_names)
		% Figure out how many frames are in this file %
		file_info = imfinfo([file_folder, file_names{file}]);
		file_frames(file) = numel(file_info);
	end
	
	% Introduce a counter because file_frames is not uniform %
	framenum = 0;
	
	% Read in each frame from each file %
	for file = 1:length(file_names)
		for frame = 1:file_frames
			% Update the waitbar %
			framenum = framenum + 1;
			waitbar(framenum/sum(file_frames), wb, ...
				join(["Reading", file_names(file); ...
				join(["File ", file, "/", length(file_names), "|"]), ...
				join(["Frame", frame, "/", file_frames(file)])]));
			
			% Load in the data %
			parent.UserData.Frames(framenum) = Frame(file_folder, file_names{file}, frame);
		end
	end
	
	% Close the waitbar %
	close(wb);
	
	%% MainWin Update %%
	% Activate the first Frame %
	parent.UserData.Frames(1).isActive = true;
	Frame.actidx(1, true);
	
	% Draw the active frame into the 'Original Image' axes %
	parent.UserData.Frames(Frame.actidx).DispImg;
	
	% Draw the Frame Slider if applicable %
	if(length(parent.UserData.Frames) > 1)
		
	end
	
	% Enable the Zoom, Pan, and Select Particle Buttons %
	UI.EnableControl(UI.ctrls, "btn: Zoom");
	UI.EnableControl(UI.ctrls, "btn: Pan");
	UI.EnableControl(UI.ctrls, "btn: Select Particles");
end
function menu_plot_OnClick(parent, obj, arg)
% Callback function that changes the appropriate plots when one of the Plotting
% parameters changes due to the user selecting the appropriate menu item.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.
%
%	> obj:		(Menu) The menu handle that called this callback function.
%
%	> arg:		(#) The index associated with this parameter in the array:
%		'MainWin.UserData.Plotting'

	%% Toggle State %%
	% Change the state in the MainWin %
	parent.UserData.Plotting(arg) = ~parent.UserData.Plotting(arg);
	
	% Affix the checked state in the menu item %
	state = 'off';
	if(parent.UserData.Plotting(arg)), state = 'on'; end
	obj.Checked = state;
	
	%% MainWin Update %%
	% Display the currently selected particle's fit - if applicable %
	if(~isempty(parent.UserData.Frames(active).Particles))
		% Find the Listbox %
		lbx = UI.FindObject(UI.ctrls, "lbx: Found Particles");
		
		% Draw the currently selected particle's spectrum %
		parent.UserData.Frames(active).Particles(lbx.Value).DrawSpec( ...
			UI.axs(4), parent.UserData.Plotting);
	elseif(strcmp(obj.Tag, "menu: Use eVs"))
		% Change the x-axis label appropriately %
		if(parent.UserData.Plotting(1))		% Use eVs? %
			xlabel(UI.axs(4), "Energy (eV)");
		else
			xlabel(UI.axs(4), "Wavelength (eV)");
		end
	end
end

function sld_frame_OnValueChanged(parent, obj, numlbl)
% Callback for whenever a Frame Slider has its value changed.  It must update the
% shown Frame, as well as any found Particles and their associated images and plots
% to the new Frame.  Potentially a computationally intensive process.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.
%
%	> obj:		(UIControl) The UIControl handle that called this callback function.
%		In this case, a uicontrol with 'Style' = 'slider'
%
%	> numlbl:	(UIControl) The associated numeric label UIControl handle.
	
	if(~isfield(parent.UserData, 'Frames')), return; end

	%% Frame Updates %%
	% Round the value obtained from the slider %
	val = round(obj.Value);
	
	% If there isn't a difference between 'val' and 'actidx' then do nothing %
	if(val == Frame.actidx), return; end
	
	% Else, deactivate the currently active frame and activate the new frame %
	Frame.actidx(val, true);
	
	% Update the associated numeric label %
	numlbl.String = join(["Frame:", val]);

	%% MainWin Update %%
	% Get the active Frame (as we won't be modifying it any more) %
	activeFrame = parent.UserData.Frames(Frame.actidx);
	
	% Draw the new Frame %
	activeFrame.DispImg;	% 'Original Image' axes %
	
	% Check if there's been any selected particles - if not, return %
	num_part = length(activeFrame.Particles);
	if(num_part == 0), return; end
	
	% For each particle found %
	for p = 1:num_part
		% Draw boxes around this particle's peak and spectrum images %
		activeFrame.Particles(p).DispBox(UI.axs(1), p == Particle.actidx);
	end
	% Send the particle strings to the 'Found Particles' Listbox %
	UI.SetControlParam(parent, "lbx: Found Particles", ...
		'String', [activeFrame.Particles(:).str]);
	
	% Get the active Particle (as we also won't be modifying it any more) %
	activeParticle = activeFrame.Particles(Particle.actidx);
	
	% Display the peak and spectrum images of the active particle %
	activeParticle.DispPeak;	% 'Peak Image' axes %
	activeParticle.DispSpec;	% 'Spectrum Image' axes %
	
	% Display the spectrum plot of the active particle %
	activeParticle.DispPlot;	% 'Selected Spectrum' axes %
end
function sld_winpara_OnValueChanged(parent, obj, numlbl)
% Callback for whenever a Window Parameter Slider has its value changed.  It must 
% update the boxes around each particle peak and its associated spectra, the images
% of the peak and spectra.  Finding or losing particles in the selected region of
% interest is not taken into account, and the process must be redone using the
% 'Select Particles' function.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.
%
%	> obj:		(UIControl) The UIControl handle that called this callback function.
%		In this case, a uicontrol with 'Style' = 'slider'
%
%	> numlbl:	(UIControl) The associated numeric label UIControl handle.
%
%	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%	Implicit Values:
%	> obj.UserData.arg: (#)		A number corresponding to which argument in the
%		associated static variable this slider is responsible for
%	> obj.UserData.rnd: (bool)	Determines if this number needs to be an integer
%
%	> numlbl.UserData.fcode:	('chr')	A character code that determines how to
%		present the numerical value encapsulated by this parameter
%	> numlbl.UserData.valfxn:	(@fxn)	A character code that determines how to
%		present the numerical value encapsulated by this parameter

	%% Window Value Updates %%
	

	%% MainWin Update %%
	
	%% UI Update %%

end
function ctrl_psld_OnValueChanged(parent, psld, numlbl)
	% If the value needs to be rounded, do so %
	val = numlbl.UserData.valfxn(psld.Value);
	if(psld.UserData.rnd), val = round(val); end

	% Update the global variable %
	parent.UserData.Fitting.params(psld.UserData.arg) = val;

	% Update the associated numeric label %
	numlbl.String = join([numlbl.UserData.str, "=", ...
		sprintf(numlbl.UserData.fcode(1:5), val), ...
		numlbl.UserData.fcode(6:end)]);
end

function sel_part_OnClick(parent, ~, ~)
	%% Refresh %%
	% Clear off the previous boxes and the previous particles %
	delete(UI.axs(1).Children(1:end-1));
	parent.UserData.Frames(Frame.actidx).Particles = Particle.empty;

	%% Select New Particles %%
	% Function wrapper for selecting the particles in each frame %
	parent.UserData.Frames(Frame.actidx).SelectROI();
	
	% Make sure to draw a box around every found particle and its spectrum %
	for p = 1:length(parent.UserData.Particles)
		if(p == 1)
			parent.UserData.Frames(Frame.actidx).Particles(p).DispBox(UI.axs(1), true);
		else
			parent.UserData.Frames(Frame.actidx).Particles(p).DispBox;
		end
	end
	
	%% MainWin Update %%	
	% Activate the Found Particles Listbox %
	UI.SetControlParam(UI.ctrls, "lbx: Found Particles", 'Enable', 'on');
	
	% Update the Listbox title %
	UI.SetControlParam(UI.ctrls, "ttllbx: Found Particles", 'String', ...
		join(["Found Particles:", ...
		length(parent.UserData.Frames(Frame.actidx).Particles)]));
	
	% Clear the spectrum plot axes %
	cla(UI.axs(4), 'reset');
	text(UI.axs(4), 0.35, 0.5, "Please fit the selected spectra");
	
	% Activate the Fit Selection Button and deactivate the Export Data Button %
	UI.EnableControl(UI.ctrls, "btn: Fit Selection");
	UI.EnableControl(UI.ctrls, "btn: Export Data", 'off');
end
function fit_part_OnClick(parent, ~, ~)
	%% Preliminaries %%
	% Obtain some parameters from the sliders %
	params = num2cell(parent.UserData.Fitting.params);
	[filt_rad, sel_sigma, param.thresh, param.lorentz, param.iter] = params{:};
	
	% Make the selection Filters %
	xgrid = -filt_rad:filt_rad;
	
	flt.sel = exp(- (xgrid-1).^2 / (2*sel_sigma^2));	% Gaussian weighting %
	flt.bg = 1 - flt.sel;
	
	flt.sel = flt.sel / sum(flt.sel);
	flt.bg = flt.bg / sum(flt.bg);
	
	% Make the SNR filters %
	flt.img = ones(size(xgrid)) / length(xgrid);	% Flat integration %
	
	lor_gamma = 2*sel_sigma;	
	flt.sm = 1 ./ (lor_gamma * (( 2*xgrid/lor_gamma ).^2 + 1));	% Smoothing %
	flt.sm = flt.sm / sum(flt.sm);

	%% Fit the selected particles %%	
	% Function wrapper for fitting each particle %
	numpart = length(parent.UserData.Particles);
	wb = waitbar(0, join(["Fitting Particles ( 0 /", numpart, ")"]));
	for p = 1:length(parent.UserData.Particles)
		waitbar(p/numpart, wb, join(["Fitting Particle:", p, "/", numpart]));
		parent.UserData.Particles{p}.FitSpec(parent, param, flt);
	end
	close(wb);
	
	%% MainWin Update %%
	% Display the fit that's currently selected %
	listbox = findobj(parent.Children, 'flat', 'Tag', "lbx: Found Particles");
	parent.UserData.Particles{listbox.Value}.DispPlt(...
		parent.UserData.axs{4},	parent.UserData.Plotting);
	
	% Activate the Export Data Button %
	UI.EnableControl(parent, "btn: Export Data");
end

function found_part_OnValueChanged(obj, parent)
	% Get the previous and current selected values %
	curVal = obj.Value;
	preVal = obj.UserData.preVal;
	obj.UserData.preVal = curVal;

	%% MainWin Update %%
	% Plot the new peak and spectrum images %
	parent.UserData.Particles{curVal}.DispImg(...
		parent.UserData.axs{2},	parent.UserData.axs{3});
	
	% Update the box colors in the main image %
	parent.UserData.Particles{preVal}.DispBox(parent.UserData.axs{1});	% Red out %
	parent.UserData.Particles{curVal}.DispBox(parent.UserData.axs{1}, [0, 0.8, 0]);
	
	% Plot the spectrum plot if needed %
	parent.UserData.Particles{curVal}.DispPlt( ...
			parent.UserData.axs{4},	parent.UserData.Plotting);
end

function export_OnClick(parent, ~, ~)
	%% Initialize %%
	% Write a header for each particle %
	particles = length(parent.UserData.Particles);
	data.peak_loc = zeros([particles, 2]);	% [x, y] %
	data.peak_max = zeros([particles, 1]);
	
	% Allocate space for the signals ... %
	data.spec_sig = zeros([particles, length(parent.UserData.Particles{1}.spec_plot.sig)]);
	data.spec_max = data.peak_max;
	
	% And the (summed) fits... %
	data.fit_sum = data.spec_sig;
	data.fit_sep = {};
	
	% And their parameters %
	data.fit_para = zeros([particles, 3]);
	
	%% Collect Data %%
	for p = 1:size(data.peak_loc, 1)
		part = parent.UserData.Particles{p};
		
		data.peak_loc(p,:) = part.peak_pos;
		data.peak_max(p) = max(part.peak_img(:));
		
		data.spec_sig(p,:) = part.spec_plot.sig;
		data.spec_max(p) = max(part.spec_plot.sel);
		
		data.fit_sum(p,:) = sum(part.spec_fits.fits, 2);
		data.fit_sep{p} = part.spec_fits.fits;
		
		if(isempty(part.spec_fits.para))
			data.fit_para(p,:) = [0, 0, 0];
		else
			data.fit_para(p,:) = part.spec_fits.para(:,1);	% Get the first one for now %
		end
	end
	
	%% Export to a .mat file %%
	disp(join(["Successfully Exported", size(data.peak_loc, 1), "Spectra..."]));
	
	save_folder = [pwd, '\Analysis\', date, '\'];
	mkdir(save_folder);
	save([save_folder, 'Spectra of selected particles in [', ...
		parent.UserData.Frames.imgs{parent.UserData.Frames.current}.filename(1:end-4), '].mat'], ...
		'data');
	
	figure(4);
	plot(data.fit_sum');
end