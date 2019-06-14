%% REFRESH %%
% Clear the static variables %
UI.REFRESH;
Frame.REFRESH;
Particle.REFRESH;

%% MAINWIN %%
% Get the dimensions of the screen itself and the desired dimensions of MainWin %
screen = get(0, 'screensize');
mw_dim = [1600, 800];			% 2:1 aspect ratio %

% Here we will set up the main window that we'll be using everything for %
MainWin = figure('Name', 'Sidescope Pro', 'Menubar', 'none', ...
				 'NumberTitle', 'off', 'DoubleBuffer', 'on', ...
				 'Position', [screen(3:4) - mw_dim, 2*mw_dim]/2);
			 
% Set up the various parameters we'll need %
MainWin.UserData.DEBUG = false;

% Add in a menu for toggleable parameters / quick functions %
menu_file = uimenu(MainWin, 'Text', 'File');
	UI.MakeMenu(MainWin, menu_file, "Load Image", @menu_load_OnClick);
	
menu_plt = uimenu(MainWin, 'Text', 'Plot Options');
	UI.MakeMenu(MainWin, menu_plt, "Use eVs", @menu_plot_OnClick, 1);
	UI.MakeMenu(MainWin, menu_plt, "Show Selection Spectra", @menu_plot_OnClick, 2, true, true);
	UI.MakeMenu(MainWin, menu_plt, "Show Outlier Spectra", @menu_plot_OnClick, 3, true);
	UI.MakeMenu(MainWin, menu_plt, "Show Signal Spectrum", @menu_plot_OnClick, 4, true);
	UI.MakeMenu(MainWin, menu_plt, "Show Fit Decomposition", @menu_plot_OnClick, 5, true);
	UI.MakeMenu(MainWin, menu_plt, "Show Ampltude Threshold", @menu_plot_OnClick, 6, false, true);
	
menu_opmode = uimenu(MainWin, 'Text', 'Operation Mode');
	UI.MakeMenu(MainWin, menu_opmode, "Illumination Correction", @menu_opmode_OnClick, 1, true);
	UI.MakeMenu(MainWin, menu_opmode, "Automated Background Subtraction (Experimental)", @menu_opmode_OnClick, 2);
	
% Make the axes we will be using %
UI.MakeAxes(MainWin, [0.000, 0.225, 0.350, 0.800], "Original Image", ...
	"X (px)", "Y (px)");

UI.MakeAxes(MainWin, [0.350, 0.725, 0.175, 0.275], "Selected Peak Image", ...
	"X (px)", "Y (px)");
UI.MakeAxes(MainWin, [0.500, 0.725, 0.525, 0.275], "Selected Spectrum Image", ...
	"X Position (px)", "Y (px)");

UI.MakeAxes(MainWin, [0.500, 0.250, 0.500, 0.475], "Selected Spectrum", ...
	"Wavelength (nm)", "Intensity (arb.)");

%% UI CONTROLS %%
% Buttons %
UI.MakeButton(MainWin, [0.05, 0.10, 0.10, 0.05], "Zoom", 'zoom', false, true);
UI.MakeButton(MainWin, [0.05, 0.05, 0.10, 0.05], "Pan", 'pan', false, true);

UI.MakeButton(MainWin, [0.20, 0.10, 0.15, 0.05], "Select Particles", ...
	@btn_sel_OnClick, false);
UI.MakeButton(MainWin, [0.20, 0.05, 0.15, 0.05], "Fit Selection", ...
	@btn_fit_OnClick, false);

UI.MakeButton(MainWin, [0.80, 0.10, 0.10, 0.05], "Export Data", ...
	@btn_exp_OnClick, false);

% Sliders - Frame Control %
UI.MakeFrameSlider(MainWin, [0.05, 0.20, 0.30, 0.05], "Frame Control", ...
	[1, 1, 1], @sld_frame_OnValueChanged, false);

% Sliders - Window Parameters (Frame.winval) %
UI.MakeParamSlider(MainWin, [0.40, 0.075, 0.15, 0.05], "Window Radius", ...
	[5, 10, 15, 0.1, 0.2], @sld_winpara_OnValueChanged, 1, true, '%2.0f(px)');
UI.MakeParamSlider(MainWin, [0.40, 0.025, 0.15, 0.05], ['Filter ', char(963)], ...
	[1, 3, 6], @sld_winpara_OnValueChanged, 2, false, '%4.2f(px)');

% Sliders - Fit Parameters (Particle.fitval) %
UI.MakeParamSlider(MainWin, [0.60, 0.175, 0.15, 0.05], "SNR Threshold", ...
	[1, 3, 5, 1/40, 1/4], @sld_fitpara_OnValueChanged, 1, false, '%3.1f');
UI.MakeParamSlider(MainWin, [0.60, 0.125, 0.15, 0.05], "SNR Band", ...
	[0.5, 1, 3, 1/25, 1/5], @sld_fitpara_OnValueChanged, 2, false, '%3.1f');
UI.MakeParamSlider(MainWin, [0.60, 0.075, 0.15, 0.05], "Max Lorentzians", ...
	[1, 2, 5, 0.25, 0.5], @sld_fitpara_OnValueChanged, 3, true, '%1.0f');
UI.MakeParamSlider(MainWin, [0.60, 0.025, 0.15, 0.05], "Fit Iterations", ...
	[0, 10, 20, 0.05, 0.25], @sld_fitpara_OnValueChanged, 4, true, '%2.0f');

% List Boxes %
UI.MakeListbox(MainWin, [0.38, 0.30, 0.13, 0.40], "Found Particles", ...
	@lbx_found_OnValueChanged, false);

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
		for frame = 1:file_frames(file)
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
	Frame.actidx(1, true);
	
	% Draw the active frame into the 'Original Image' axes %
	parent.UserData.Frames(Frame.actidx).DispImg();

	% Enable the Zoom, Pan, and Select Particle Buttons %
	UI.Ctrl_Enable("btn: Zoom");
	UI.Ctrl_Enable("btn: Pan");
	UI.Ctrl_Enable("btn: Select Particles");
	
	% Show the Frame Slider if applicable %
	if(framenum > 1)
		UI.Ctrl_Set("sld: Frame Control", UI.ctrls, 'Max', framenum);
		UI.Ctrl_Show("sld: Frame Control");
		UI.Ctrl_Show("numlbl: Frame Control");
	end
end
function menu_plot_OnClick(parent, obj, arg)
% Callback function that changes the appropriate plots when one of the Plotting
% parameters changes due to the user selecting the appropriate menu item.  Plot 
% options are stored in the Particle class. Currently available plotting options 
% include:
% - Using the nm or eV axis (nm default)
% - Show/hide everything individually but the total Lorentzian fit
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.
%
%	> obj:		(Menu) The menu handle that called this callback function.
%
%	> arg:		(#) The index associated with this parameter in the array:
%		'Particle.pltopt'

	%% Particle Update %%
	% Change the state of that particular plotting option - if available %
	if(length(Particle.pltopt) >= arg)
		thisarg = Particle.pltopt(arg);
		Particle.pltopt(~thisarg, arg);
	else
		% Write to that position the current value of obj.Checked %
		Particle.pltopt(strcmp(obj.Checked, 'on'), arg);
	end
	
	%% MainWin Update %%
	% Affix the checked state in the menu item %
	if(Particle.pltopt(arg))
		obj.Checked = 'on';
	else
		obj.Checked = 'off';
	end
	
	%% UI Update %%
	% If we haven't loaded a frame yet, drop everything and run %
	if(~isfield(parent.UserData, 'Frames')), return; end
	
	% Get the currently active frame %
	activeFrame = parent.UserData.Frames(Frame.actidx);
	
	% Display the currently selected particle's fit - if applicable %
	if(~isempty(activeFrame.Particles))
		Particle.S_DispPlot(activeFrame.Particles(activeFrame.actPar));
	elseif(strcmp(obj.Tag, "menu: Use eVs"))
		% Change the x-axis label appropriately %
		if(Particle.pltopt(1))		% Use eVs? %
			xlabel(UI.axs(4), "Energy (eV)");
		else
			xlabel(UI.axs(4), "Wavelength (eV)");
		end
	end
end
function menu_opmode_OnClick(parent, obj, arg)
% Callback function for handling different operation modes.  These options are stored
% in the Frame class, as that is the class that will utilize them most.  Available 
% operation modes include:
% - Illumination correction			(Default: true)
% - Automatic background correction (Default: false)
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.
%
%	> obj:		(Menu) The menu handle that called this callback function.
%
%	> arg:		(#) The index associated with this parameter in the array:
%		'Frame.opmode'


	%% Frame Update %%
	% Change the state of that particular operation mode option - if available %
	if(length(Frame.opmode) >= arg)
		thisarg = Frame.opmode(arg);
		Frame.opmode(~thisarg, arg);
	else
		% Write to that position the current value of obj.Checked %
		Frame.opmode(strcmp(obj.Checked, 'on'), arg);
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
	
	%% Preliminary Tests %%
	% Check if we've even initialized the Frames field - if not, no business here %
	if(~isfield(parent.UserData, 'Frames')), return; end

	%% Frame Updates %%
	% Round the value obtained from the slider %
	val = round(obj.Value);
	
	% If there isn't a difference between 'val' and 'actidx' then do nothing %
	if(val == Frame.actidx), return; end
	
	% Else, deactivate the currently active frame and activate the new frame %
	Frame.actidx(val, true);

	%% MainWin Update %%
	% Update the associated numeric label %
	numlbl.String = join(["Frame:", val]);
	
	% Get the active Frame (as we won't be modifying it any more) %
	activeFrame = parent.UserData.Frames(Frame.actidx);
	
	% Draw the new Frame %
	activeFrame.DispImg();	% 'Original Image' axes %
	
	% Check if there's been any selected particles - if not, return %
	num_part = length(activeFrame.Particles);
	% Display the number of particles found - even if it's zero %
	UI.Ctrl_Set("ttllbx: Found Particles", UI.ctrls, 'String', ...
		join(["Found Particles:", num_part]));
	
	% Refresh the main axes and (maybe) draw a box around each particle %
	activeFrame.DispBox();
	
	if(num_part == 0)
		% Empty out the Found Particles listbox %
		UI.Ctrl_Set("lbx: Found Particles", UI.ctrls, 'String', {});

		% Clear out the peak and spectrum images %
		Particle.S_DispPeak();
		Particle.S_DispSpec();
		
		% Clear out the spectrum plot too %
		Particle.S_DispPlot();
	else
		% Send the particle strings to the 'Found Particles' Listbox %
		str_loc = [activeFrame.Particles(:).str_loc];
		for p = 1:length(activeFrame.Particles)
			str(p) = join(["Particle #", p, "@", str_loc(p)]);
		end
		UI.Ctrl_Set("lbx: Found Particles", UI.ctrls, 'String', str);

		% Get the active Particle (as we also won't be modifying it any more) %
		activeParticle = activeFrame.Particles(activeFrame.actPar);

		% Display the peak and spectrum images of the active particle %
		Particle.S_DispPeak(activeParticle);	% 'Peak Image' axes %
		Particle.S_DispSpec(activeParticle);	% 'Spectrum Image' axes %

		% Display the spectrum plot of the active particle %
		Particle.S_DispPlot(activeParticle);	% 'Selected Spectrum' axes %
	end
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
%	Implicit Parameters:
%	>> obj.UserData.arg: (#)		A number corresponding to which argument in the
%		associated static variable this slider is responsible for
%	>> obj.UserData.rnd: (bool)	Determines if this number needs to be an integer
%
%	>> numlbl.UserData.fcode:	('chr')	A character code that determines how to
%		present the numerical value encapsulated by this parameter
%	>> numlbl.UserData.valfxn:	(@fxn)	A character code that determines how to
%		present the numerical value encapsulated by this parameter

	%% Window Value Updates %%
	% Round the value obtained from the slider (after putting it through the value
	% function)
	val = numlbl.UserData.valfxn(obj.Value);
	if(obj.UserData.rnd), val = round(val); end
	
	% If there isn't a difference between 'val' and this winval then do nothing %
	if(length(Frame.winval) >= obj.UserData.arg)	% Make sure it's at least there %
		if(val == Frame.winval(obj.UserData.arg)), return; end
	end
	
	% Else, set the parameter to the new value %
	Frame.winval(val, obj.UserData.arg);

	%% MainWin Update %%
	% Update the associated numeric slider %
	numlbl.String = join([numlbl.UserData.str, "=", ...
		sprintf(numlbl.UserData.fcode(1:5), val), numlbl.UserData.fcode(6:end)]);
	
	%% UI Update %%
	% Make sure that there is actually something to update %
	if(~isfield(parent.UserData, 'Frames')), return; end
	
	% To clarify things, and because we're not mutating anything, copy down the
	% active Frame
	activeFrame = parent.UserData.Frames(Frame.actidx);
	
	% Check to make sure that the Particles field exists; if it doesn't exist, then
	% don't do anything - you don't need to.
	if(isempty(activeFrame.Particles)), return; end
	
	% Else, Re-plot the boxes around the particles %
	%activeFrame.DispBox();
	
	% Re-plot the peak and spectrum images %
	
	% And remove the spectrum plot %
end
function sld_fitpara_OnValueChanged(~, obj, numlbl)
% Callback for whenever a Fit Parameter Slider has its value changed.  It must 
% update the boxes around each particle peak and its associated spectra, the images
% of the peak and spectra.  Finding or losing particles in the selected region of
% interest is not taken into account, and the process must be redone using the
% 'Select Particles' function.
%	----------------------------------------------------------------------------	
%	Argument Definitions:%
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
	
	%% Fitting Value Updates %%
	% Round the value obtained from the slider (after putting it through the value
	% function)
	val = numlbl.UserData.valfxn(obj.Value);
	if(obj.UserData.rnd), val = round(val); end
	
	% If there isn't a difference between 'val' and this fitval then do nothing %
	if(length(Particle.fitval) >= obj.UserData.arg)	% Make sure it's at least there %
		if(val == Particle.fitval(obj.UserData.arg)), return; end
	end
	
	% Else, set the parameter to the new value %
	Particle.fitval(val, obj.UserData.arg);

	%% MainWin Update %%
	% Update the associated numeric slider %
	numlbl.String = join([numlbl.UserData.str, "=", ...
		sprintf(numlbl.UserData.fcode(1:5), val), numlbl.UserData.fcode(6:end)]);
end

function btn_sel_OnClick(parent)
% Callback for the 'Select Particles' button click event.  It must clear off the
% selection boxes on the main image, then prompt the user to select a region of
% interest in order to obtain new particles.  Once done so, it draws boxes around
% each one of these particles' peaks and spectrum.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.

	%% Appending Particles %%
	% Check to see if we are indeed, *appending* particles instead of wiping them %
	append = strcmp(parent.CurrentKey, 'shift');

	%% Refresh %%
	% Make a *safe* reference to the children - mutations to children will affect
	% UI.axs(1).Children.  Handles in MATLAB are the one object you can actually pass
	% a reference to and it count, I think?
	children = UI.axs(1).Children;
	
	if(append)
		% Make all the boxes white for now, to indicate that we can add more %
		for c = 1:length(children)-3
			children(c).Color = [1, 1, 1];
		end
	else
		% Clear off the previous boxes by making them invisible %
		for c = 1:length(children)-3
			children(c).Visible = 'off';
		end
	end
	
	% Clear out the listbox %
	if(~append)
		parent.UserData.Frames(Frame.actidx).Particles = Particle.empty;
		UI.Ctrl_Set("lbx: Found Particles", UI.ctrls, 'String', []);
	end

	%% Select New Particles %%
	% Function wrapper for selecting the particles in each frame %
	parent.UserData.Frames(Frame.actidx).SelectROI(append);
	
	% Make sure to draw a box around every found particle and its spectrum %
	parent.UserData.Frames(Frame.actidx).DispBox();
	
	%% UI Update %%	
	% We're not modifying the active frame anymore, so make a copy for convienence %
	activeFrame = parent.UserData.Frames(Frame.actidx);
	
	% Activate the Found Particles Listbox %
	UI.Ctrl_Enable("lbx: Found Particles");
	
	% Update the Listbox title %
	UI.Ctrl_Set("ttllbx: Found Particles", UI.ctrls, 'String', ...
		join(["Found Particles:", length(activeFrame.Particles)]));
	
	% Determine if there's any particles around %
	if(isempty(activeFrame.Particles))
		% Prompt the user to select more particles %
		Particle.S_DispPeak();
		Particle.S_DispSpec();
		Particle.S_DispPlot();
		
		% Deactivate the Fit Selection Button and deactivate the Export Data Button %
		UI.Ctrl_Enable("btn: Fit Selection", UI.ctrls, false);
		UI.Ctrl_Enable("btn: Export Data", UI.ctrls, false);
	else
		% For our sanity, make a reference to the active particle %
		activeParticle = activeFrame.Particles(activeFrame.actPar);
		
		% Show the active particle's peak and spectrum images %
		Particle.S_DispPeak(activeParticle);
		Particle.S_DispSpec(activeParticle);
		
		% Show the active particle's spectrum plot if applicable %
		Particle.S_DispPlot(activeParticle);

		% Activate the Fit Selection Button and deactivate the Export Data Button %
		UI.Ctrl_Enable("btn: Fit Selection");
		UI.Ctrl_Enable("btn: Export Data", UI.ctrls, false);
	end
	
end
function btn_fit_OnClick(parent)
% Callback for the 'Fit Selection' button click event.  For all found particles, it
% must first pick apart the spectrum image using a selection filter (a Gaussian) and
% a background filter (1 - Gaussian) as well as make the "SNR filter", which we will
% use to determine how noisy our signal is and whether or not a peak is valid.
%
% Once we have these plots and values, we will then fit a maximum number of 
% Lorentzians to the data using a corrective approach.  The various parameters used
% in this fitting process can be tuned using sliders on the MainWin.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.

	%% Fit the Selected Particle Spectra %%
	% Introduce a waitbar because this may take some time %
	num_part = length(parent.UserData.Frames(Frame.actidx).Particles);
	wb = waitbar(0, join(["Fitting Spectra ( 0 /", num_part, ")"]));
	
	% Run the function wrapper - which also creates the filters needed %
	parent.UserData.Frames(Frame.actidx).FitROI(wb);
	
	% Close the waitbar when finished %
	close(wb);
	
	%% UI Update %%
	% Get the currently active frame and particle %
	activeFrame = parent.UserData.Frames(Frame.actidx);
	activeParticle = activeFrame.Particles(activeFrame.actPar);
	
	% Display the currently selected fit %
	Particle.S_DispPlot(activeParticle);
	
	% Activate the "Export Data" button %
	UI.Ctrl_Enable("btn: Export Data");
end

function lbx_found_OnValueChanged(parent, obj)
% Callback for whenever the Found Particle Listbox has its value changed.  It must 
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.
%
%	> obj:		(UIControl) The UIControl handle that called this callback function.
%		In this case, a uicontrol with 'Style' = 'listbox'
%
%	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%	Implicit Values:
%	>> obj.UserData.preVal: (#)	The index of the previous value of the Listbox.
	
	% Get the previous and current selected values %
	preVal = parent.UserData.Frames(Frame.actidx).actPar;
	curVal = obj.Value;
	
	%% Frame Update %%
	% Update the currently selected Particle in this Frame %
	parent.UserData.Frames(Frame.actidx).actPar = curVal;
	
	%% UI Update %%
	% Get the currently active Frame %
	activeFrame = parent.UserData.Frames(Frame.actidx);
	
	% Update the box colors on the main image %
	activeFrame.Particles(preVal).DispBox();				% Color red %
	activeFrame.Particles(curVal).DispBox(UI.axs(1), true);	% Color green %
	
	% Plot the selected peak and spectrum images %
	Particle.S_DispPeak(activeFrame.Particles(curVal));
	Particle.S_DispSpec(activeFrame.Particles(curVal));
	
	% Plot the selected spectrum if available %
	Particle.S_DispPlot(activeFrame.Particles(curVal));
end

function btn_exp_OnClick(parent)
%
%	----------------------------------------------------------------------------
%	Argument Definitions
%	> parent:	(MainWin)

	%% Initialization %%
	% Write a header for each particle present in this frame %
	particles = parent.UserData.Frames(Frame.actidx).Particles;
	num_parts = length(particles);
	
	%% Data Dump %%
	% For each particle, export out... %
	for p = 1:num_parts
		% Position %
		data(p).pos_peak = particles(p).peak_pos';
		
		% Images %
		data(p).img_peak = particles(p).peak_img;
		data(p).img_spec = particles(p).spec_img;
		
		% Find out where the peak is, pixel wise %
		[~, data(p).idx_spec] = max(sum(particles(p).spec_img(7:13,:), 2));
		
		% Plots %
		data(p).plt_sel = particles(p).spec_plots(:,1);
		data(p).plt_bg = particles(p).spec_plots(:,2);
		data(p).plt_sig = particles(p).spec_plots(:,3);
		
		% Fits %
		data(p).fit_domain = [Particle.RNG_NM, Particle.RNG_EV];
		data(p).fit_curves = [particles(p).spec_fits.curve];
		data(p).fit_params = [particles(p).spec_fits.param];
	end
	
	%% Write to File %%
	save_folder = [pwd, '\Analysis\', date, '\'];
	mkdir(save_folder);
	save([save_folder, 'Spectra of selected particles in [', ...
		parent.UserData.Frames(Frame.actidx).file_name(1:end-4), '].mat'], ...
		'data');
	
	disp(join(["Successfully Exported", num_parts, "Spectra..."]));
end