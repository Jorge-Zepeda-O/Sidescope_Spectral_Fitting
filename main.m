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
	UI.MakeMenu(menu_file, "Load Image", @menu_load_OnClick);
	
menu_visopt = uimenu(MainWin, 'Text', 'Visualization');
	UI.MakeMenu(menu_visopt, "Show Scale Bars", @menu_visopt_OnClick, 1, true);
	UI.MakeMenu(menu_visopt, "Show Selection Filter Guides", @menu_visopt_OnClick, 2, true);
	UI.MakeMenu(menu_visopt, "Use eVs", @menu_visopt_OnClick, 3, false, true);
	UI.MakeMenu(menu_visopt, "Show Selection & Outliers", @menu_visopt_OnClick, 4, true);
	UI.MakeMenu(menu_visopt, "Show Signal Spectrum", @menu_visopt_OnClick, 5, true);
	UI.MakeMenu(menu_visopt, "Show Fit Decomposition", @menu_visopt_OnClick, 6, false);
	UI.MakeMenu(menu_visopt, "Show Hybridization Colors", @menu_visopt_OnClick, 7, false);
	
menu_opmode = uimenu(MainWin, 'Text', 'Operation Mode');
	UI.MakeMenu(menu_opmode, "Illumination Correction", @menu_opmode_OnClick, 1, true);
	UI.MakeMenu(menu_opmode, "Automate Background Subtraction (Experimental)", @menu_opmode_OnClick, 2);
	UI.MakeMenu(menu_opmode, "Allow Lorentzian-Gaussian Hybridization", @menu_opmode_OnClick, 3, false, true);
	UI.MakeMenu(menu_opmode, "Automate Peak Fitting (Experimental)", @menu_opmode_OnClick, 4, false);
	UI.MakeMenu(menu_opmode, "Use Gaussian Selection Window", @menu_opmode_OnClick, 5, true);
	
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
UI.MakeButton(MainWin, [0.05, 0.10, 0.05, 0.05], "Zoom", 'zoom', false, true);
UI.MakeButton(MainWin, [0.05, 0.05, 0.05, 0.05], "Pan", 'pan', false, true);

UI.MakeButton(MainWin, [0.15, 0.125, 0.10, 0.05], "Select Particles", ...
	@btn_sel_OnClick, false);
UI.MakeButton(MainWin, [0.25, 0.125, 0.10, 0.05], "Batch Selection", ...
	@btn_sel_OnClick, false, false, true);

UI.MakeButton(MainWin, [0.15, 0.075, 0.10, 0.05], "Fit Selection", ...
	@btn_fit_OnClick, false);
UI.MakeButton(MainWin, [0.25, 0.075, 0.10, 0.05], "Batch Fit", ...
	@btn_fit_OnClick, false, false, true);

UI.MakeButton(MainWin, [0.15, 0.025, 0.10, 0.05], "Clear Selection", ...
	@btn_clr_OnClick, false);
UI.MakeButton(MainWin, [0.25, 0.025, 0.10, 0.05], "Batch Clear", ...
	@btn_clr_OnClick, false, false, true);

UI.MakeButton(MainWin, [0.75, 0.10, 0.10, 0.05], "Export Data", ...
	@btn_exp_OnClick, false);
UI.MakeButton(MainWin, [0.85, 0.10, 0.10, 0.05], "Batch Export", ...
	@btn_exp_OnClick, false, false, true);

% Sliders - Frame Control %+
UI.MakeFrameSlider(MainWin, [0.05, 0.20, 0.30, 0.05], "Frame Control", ...
	[1, 1, 1], @sld_frame_OnValueChanged, false);

% Sliders - Window Parameters (Frame.winval) %
UI.MakeParamSlider(MainWin, [0.40, 0.075, 0.15, 0.05], "Window Radius", ...
	[5, 10, 15, 0.1, 0.2], @sld_winpara_OnValueChanged, 1, true, '%2.0f(px)');
UI.MakeParamSlider(MainWin, [0.40, 0.025, 0.15, 0.05], ['Filter ', char(963)], ...
	[1/3, 3/2, 10/3, 1/60, 1/9], @sld_winpara_OnValueChanged, 2, false, '%4.2f(px)');

% Sliders - Fit Parameters (Particle.fitval) %
UI.MakeParamSlider(MainWin, [0.60, 0.175, 0.15, 0.05], "SNR Threshold", ...
	[1, 3, 5, 1/40, 1/4], @sld_fitpara_OnValueChanged, 1, false, '%3.1f');
UI.MakeParamSlider(MainWin, [0.60, 0.125, 0.15, 0.05], "SNR Band", ...
	[0.5, 1, 3, 1/25, 1/5], @sld_fitpara_OnValueChanged, 2, false, '%3.1f');
UI.MakeParamSlider(MainWin, [0.60, 0.075, 0.15, 0.05], "Peaks", ...
	[1, 1, 5, 0.25, 0.5], @sld_fitpara_OnValueChanged, 3, true, '%1.0f');

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
	UI.Ctrl_Enable("btn: Batch Selection");
	
	% Show the Frame Slider & Batch buttons if applicable %
	if(framenum > 1)
		UI.Ctrl_Set("sld: Frame Control", UI.ctrls, 'Max', framenum);
		UI.Ctrl_Show("sld: Frame Control");
		UI.Ctrl_Show("numlbl: Frame Control");
		
		UI.Ctrl_Show("btn: Batch Selection");
		UI.Ctrl_Show("btn: Batch Fit");
		UI.Ctrl_Show("btn: Batch Clear");
		UI.Ctrl_Show("btn: Batch Export");
	end
end
function menu_visopt_OnClick(parent, obj, arg)
% Callback function that changes the appropriate plots when one of the Plotting
% parameters changes due to the user selecting the appropriate menu item.  Plot 
% options are stored in the Particle class. Currently available plotting options 
% include:
% - Using the nm or eV axis (nm default)
% - Show/hide everything individually but the total peak fit
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.
%
%	> obj:		(Menu) The menu handle that called this callback function.
%
%	> arg:		(#) The index associated with this parameter in the array:
%		'Particle.visopt'

	%% Particle Update %%
	% Change the state of that particular plotting option - if available %
	if(length(Particle.visopt) >= arg)
		thisarg = Particle.visopt(arg);
		Particle.visopt(~thisarg, arg);
	else
		% Write to that position the current value of obj.Checked %
		Particle.visopt(strcmp(obj.Checked, 'on'), arg);
	end
	
	%% MainWin Update %%
	% Affix the checked state in the menu item %
	if(Particle.visopt(arg))
		obj.Checked = 'on';
	else
		obj.Checked = 'off';
	end
	
	%% UI Update %%
	% If we haven't loaded a frame yet, drop everything and run %
	if(~isfield(parent.UserData, 'Frames')), return; end
	
	% Get the currently active frame %
	if(isempty(Frame.actidx))
		activeFrame = parent.UserData.Frames(1);
	else
		activeFrame = parent.UserData.Frames(Frame.actidx);
	end
	
	% Display the currently selected particle's fit and spectrum - if applicable %
	if(~isempty(activeFrame.Particles))
		Particle.S_DispSpec(activeFrame.Particles(activeFrame.actPar));
		Particle.S_DispPlot(activeFrame.Particles(activeFrame.actPar));
		
		% Update main image scale bars %
		if(arg == 1)
			children_img = UI.axs(1).Children;
			
			% The ones we're looking for are the two above the image itself %
			for c = length(children_img) - (1:2)
				if(Particle.visopt(1))
					children_img(c).Visible = 'on';
				else
					children_img(c).Visible = 'off';
				end
			end
			
			% The peak image is quick to redo, don't mind %
			Particle.S_DispPeak(activeFrame.Particles(activeFrame.actPar));
		end
	elseif(strcmp(obj.Tag, "menu: Use eVs"))
		% Change the x-axis label appropriately %
		if(Particle.visopt(3))		% Use eVs? %
			xlabel(UI.axs(4), "Energy (eV)");
		else
			xlabel(UI.axs(4), "Wavelength (eV)");
		end
	end
end
function menu_opmode_OnClick(~, obj, arg)
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
	
	%% MainWin Update %%
	% Affix the checked state in the menu item %
	if(Frame.opmode(arg))
		obj.Checked = 'on';
	else
		obj.Checked = 'off';
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
	if(isempty(Frame.actidx))
		activeFrame = parent.UserData.Frames(1);
	else
		activeFrame = parent.UserData.Frames(Frame.actidx);
	end
	
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
		UI.Ctrl_Set("lbx: Found Particles", UI.ctrls, 'Value', activeFrame.actPar);
		UI.Ctrl_Set("lbx: Found Particles", UI.ctrls, 'String', activeFrame.lbx_str);

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
	if(isempty(Frame.actidx))
		activeFrame = parent.UserData.Frames(1);
	else
		activeFrame = parent.UserData.Frames(Frame.actidx);
	end
	
	% Check to make sure that the Particles field exists; if it doesn't exist, then
	% don't do anything - you don't need to.
	if(isempty(activeFrame.Particles)), return; end
	
	% Check if the window radius slider was moved %
	if(obj.UserData.arg == 1)
		% Wipe out the current particles, because they don't matter anymore %
		btn_clr_OnClick(parent);
	end
	
	% Check if the filter slider was moved %
	if(obj.UserData.arg == 2)
		% Move the lines accordingly %
		children_spec = UI.axs(3).Children;
		for c = 1:length(children_spec)-3
			children_spec(c).YData = Frame.winval(1)+1 + Frame.winval(2)*(c-2-1)*[1,1];
			
			% Adjust the visibility now if you get the chance %
			if(Particle.visopt(6))
				children_spec(c).Visible = 'on';
			else
				children_spec(c).Visible = 'off';
			end
		end
	end
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

function btn_clr_OnClick(parent, batch)
% Callback for the 'Clear Particles' button click event.  It wipes the current list
% of particles and clears out the listbox.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.

	%% Argument Defaults %%
	if(nargin < 2), batch = false; end

	%% Frame Update %%
	if(~batch)
		frame_range = Frame.actidx;
	else
		frame_range = 1:length(parent.UserData.Frames);
	end
	
	for f = frame_range
		% Clear out the Particle list and the listbox for this Frame %
		parent.UserData.Frames(f).Particles = Particle.empty;
	end
	
	%% UI Update %%
	% Clear out the listbox for the particles %
	UI.Ctrl_Set("lbx: Found Particles", UI.ctrls, 'String', []);
	
	% Clear out the boxes for the particles %
	children_img = UI.axs(1).Children;
	delete(children_img(1:end-3));
	
	% Clear out the images and plots %
	Particle.S_DispPeak();
	Particle.S_DispSpec();
	Particle.S_DispPlot();
		
	% Disable this button, along with the other particle related buttons %
	UI.Ctrl_Enable("btn: Fit Selection", UI.ctrls, false);
	UI.Ctrl_Enable("btn: Batch Fit", UI.ctrls, false);
	
	UI.Ctrl_Enable("btn: Clear Selection", UI.ctrls, false);
	UI.Ctrl_Enable("btn: Batch Clear", UI.ctrls, false);
	
	UI.Ctrl_Enable("btn: Export Data", UI.ctrls, false);
end
function btn_sel_OnClick(parent, batch)
% Callback for the 'Select Particles' button click event.  It prompts the user to 
% select a region of interest in order to obtain new particles, appending to any 
% previous list.  Once done so, it draws a box around each one of these particles' 
% peaks and one around its spectrum.
%	----------------------------------------------------------------------------	
%	Argument Definitions:
%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
%		storing and keeping track of many different instance variables.

	%% Argument Defaults %%
	if(nargin < 2), batch = false; end

	%% Refresh %%
	% Make a *safe* reference to the children - mutations to children will affect
	% UI.axs(1).Children.  Handles in MATLAB are the one object you can actually pass
	% a reference to and it count, I think?
	children = UI.axs(1).Children;
	
	% Make all the boxes white for now, to indicate that we can add more %
	for c = 1:length(children)-3
		children(c).Color = [1, 1, 1];
	end

	%% Select New Particles %%
	if(~batch)
		frame_range = Frame.actidx;
	else
		frame_range = 1:length(parent.UserData.Frames);
	end
	
	roi = round(getrect(UI.axs(1)));
	for f = frame_range
		% Function wrapper for selecting the particles in each frame %
		parent.UserData.Frames(f).SelectROI(roi);
	end
	
	% Make sure to show the active frame %
	parent.UserData.Frames(Frame.actidx).DispImg()
	
	% Make sure to draw a box around every found particle and its spectrum %
	parent.UserData.Frames(Frame.actidx).DispBox();
	
	% And make sure that the listbox is displaying the right strings and is enabled %
	lbx = UI.FindObject("lbx: Found Particles");
	lbx.String = parent.UserData.Frames(Frame.actidx).lbx_str;
	lbx.Value = parent.UserData.Frames(Frame.actidx).actPar;
	lbx.Enable = 'on';
	
	%% UI Update %%	
	% We're not modifying the active frame anymore, so make a copy for convienence %
	if(isempty(Frame.actidx))
		activeFrame = parent.UserData.Frames(1);
	else
		activeFrame = parent.UserData.Frames(Frame.actidx);
	end
	
	% Update the Listbox title %
	UI.Ctrl_Set("ttllbx: Found Particles", UI.ctrls, 'String', ...
		join(["Found Particles:", length(activeFrame.Particles)]));
	
	% Determine if there's any particles around %
	if(isempty(activeFrame.Particles))
		% Prompt the user to select more particles %
		Particle.S_DispPeak();
		Particle.S_DispSpec();
		Particle.S_DispPlot();
		
		% Deactivate the Fit Selection Buttons and the Export Data Button %
		UI.Ctrl_Enable("btn: Fit Selection", UI.ctrls, false);
		UI.Ctrl_Enable("btn: Batch Fit", UI.ctrls, false);

		UI.Ctrl_Enable("btn: Export Data", UI.ctrls, false);
	else
		% For our sanity, make a reference to the active particle %
		activeParticle = activeFrame.Particles(activeFrame.actPar);
		
		% Show the active particle's peak and spectrum images %
		Particle.S_DispPeak(activeParticle);
		Particle.S_DispSpec(activeParticle);
		
		% Show the active particle's spectrum plot if applicable %
		Particle.S_DispPlot(activeParticle);

		% Activate the Clear Selection and Fit Selection Buttons and deactivate the 
		% Export Data Button
		UI.Ctrl_Enable("btn: Fit Selection");
		UI.Ctrl_Enable("btn: Batch Fit");
		
		UI.Ctrl_Enable("btn: Clear Selection");
		UI.Ctrl_Enable("btn: Batch Clear");
		
		UI.Ctrl_Enable("btn: Export Data", UI.ctrls, false);
	end
	
end
function btn_fit_OnClick(parent, batch)
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
%
%	~ batch:	(T/F)	Determines if we batch fit or not
%				(~)		Default: no batch fitting

	%% Argument Defaults %%
	if(nargin < 2), batch = false; end

	%% Fit the Selected Particle Spectra %%
	if(~batch)
		frame_range = Frame.actidx;
	else
		frame_range = 1:length(parent.UserData.Frames);
	end
	
	% Introduce a waitbar because this may take some time %	
	counters = [1, 0, length([parent.UserData.Frames(frame_range).Particles])];
	wb = waitbar(0, "Fitting Spectra...");
	wb.UserData = counters;
	
	% Run the function wrapper - which also creates the filters needed %
	for f = frame_range
		parent.UserData.Frames(f).FitROI(wb);
	end
	
	% Close the waitbar when finished %
	close(wb);
	
	%% UI Update %%
	% Get the currently active frame and particle %
	if(isempty(Frame.actidx))
		activeFrame = parent.UserData.Frames(1);
	else
		activeFrame = parent.UserData.Frames(Frame.actidx);
	end
	activeParticle = activeFrame.Particles(activeFrame.actPar);
	
	% Display the currently selected fit %
	Particle.S_DispPlot(activeParticle);
	
	% Activate the "Export Data" buttons %
	UI.Ctrl_Enable("btn: Export Data");
	UI.Ctrl_Enable("btn: Batch Export");
end
function btn_exp_OnClick(parent, batch)
%
%	----------------------------------------------------------------------------
%	Argument Definitions
%	> parent:	(MainWin)
	
	%% Initialization %%
	save_folder = [pwd, '\Analysis\'];
	mkdir(save_folder);

	%% Data Dump %%
	if(~batch)
		frame_range = Frame.actidx;
	else
		frame_range = 1:length(parent.UserData.Frames);
	end

	% Dump the data for each frame %
	for f = frame_range
		% Write a header for each particle present in this frame %
		particles = parent.UserData.Frames(f).Particles;
		num_parts = length(particles);
		
		save_folder_frame = [save_folder, parent.UserData.Frames(f).file_name(1:end-4), ...
			'\Frame', sprintf("%d", f), '\'];
		mkdir(join(save_folder_frame, ''));

		% For each particle, export out... %
		for p = 1:num_parts
			delete(findall(gcf,'type','annotation'))
			
			% Position %
			data(p).pos_peak = particles(p).peak_pos';
			data(p).pos_spec = particles(p).peak_pos(2) + particles(p).spec_off;
			data(p).pos_bg = particles(p).bg_pos;
			
			% Images %
			data(p).img_peak = particles(p).peak_img;
			data(p).img_spec = particles(p).spec_img;
			
			% Background %
			data(p).img_bg = particles(p).bg_img;
			
			% Find out where the peak is, pixel wise %
			%[~, data(p).idx_spec] = max(sum(particles(p).spec_img(7:13,:), 2));

			% Plots %
			data(p).plt_sel = particles(p).spec_plots(:,1);
			data(p).plt_bg = particles(p).spec_plots(:,2);
			data(p).plt_sig = particles(p).spec_plots(:,3);

			% Fits %
			data(p).fit_domain = [Particle.RNG_NM, Particle.RNG_EV];
			data(p).fit_curves = [particles(p).spec_fits.curves];
			data(p).fit_params = [particles(p).spec_fits.params];
			
			% Other info %
			data(p).snr = particles(p).snr;
			
			% Figure %
			if(~exist('fig', 'var'))
				fig = figure('Position', [700, 320, 1000, 600], ...
					'Name', ['[', parent.UserData.Frames(f).file_name(1:end-4), ']', ...
					sprintf(' Particle at: (%d, %d)', ...
					particles(p).peak_pos(1), particles(p).peak_pos(2))]);
			end
			delete(fig.Children);			
			
			ax = axes(fig, 'Units', 'pixels', 'Position', [40, 460, 180, 120]);
			Particle.S_DispPeak(particles(p), ax);
			
			ax = axes(fig, 'Units', 'pixels', 'Position', [300, 460, 680, 120]);
			Particle.S_DispSpec(particles(p), ax);
			
			ax = axes(fig, 'Units', 'pixels', 'Position', [300, 50, 680, 340]);
			Particle.S_DispPlot(particles(p), ax);
			
			if(Frame.opmode(3))
				params = particles(p).spec_fits.params;
				params = sortrows(params', 1, 'descend')';
				if(~Particle.visopt(3))
					params(3,:) = Particle.HC .* (1./(params(2,:)-params(3,:)/2) - 1./(params(2,:)+params(3,:)/2));
					params(2,:) = Particle.HC ./ params(2,:);
				end
				ptbl = cell([4, size(params, 2)]);
				for param = 1:size(params, 2)
					ptbl{1,param} = char(sprintf("%5.1f%%", 100*params(1,param)));
					if(Particle.visopt(3))
						ptbl{2,param} = char(sprintf("%5.2f", params(2,param)));
						ptbl{3,param} = char(sprintf("%5.2f", params(3,param)));
					else
						ptbl{2,param} = char(sprintf("%5.1f", params(2,param)));
						ptbl{3,param} = char(sprintf("%5.1f", params(3,param)));
					end
					ptbl{4,param} = char(sprintf("%5.0f", params(4,param)));
				end

				tbl = uitable(fig, 'Position', [20, 50, 200, 340], 'Data', ptbl');
				if(Particle.visopt(3))
					tbl.ColumnName = {char(951), [char(956), ' (eV)'], [char(915), ' (eV)'], 'A'};
				else
					tbl.ColumnName = {char(951), [char(956), ' (nm)'], [char(915), ' (nm)'], 'A'};
				end
				tbl.ColumnWidth = {42};
				tbl.FontName = 'Consolas';
			else
				params = particles(p).spec_fits.params;
				params = sortrows(params', 1, 'descend')';
				if(~Particle.visopt(3))
					params(2,:) = Particle.HC .* (1./(params(1,:)-params(2,:)/2) - 1./(params(1,:)+params(2,:)/2));
					params(1,:) = Particle.HC ./ params(1,:);
				end
				ptbl = cell([4, size(params, 2)]);
				for param = 1:size(params, 2)
					if(Particle.visopt(3))
						ptbl{1,param} = char(sprintf("%5.2f", params(1,param)));
						ptbl{2,param} = char(sprintf("%5.2f", params(2,param)));
					else
						ptbl{1,param} = char(sprintf("%5.1f", params(1,param)));
						ptbl{2,param} = char(sprintf("%5.1f", params(2,param)));
					end
					ptbl{3,param} = char(sprintf("%5.0f", params(3,param)));
				end

				tbl = uitable(fig, 'Position', [20, 50, 200, 340], 'Data', ptbl');
				if(Particle.visopt(3))
					tbl.ColumnName = {[char(956), ' (eV)'], [char(915), ' (eV)'], 'A'};
				else
					tbl.ColumnName = {[char(956), ' (nm)'], [char(915), ' (nm)'], 'A'};
				end
				tbl.ColumnWidth = {42};
				tbl.FontName = 'Consolas';
			end
			annotation('textbox', [0.02, 0.65, 0.2, 0.04], ...
				'String', sprintf("SNR: %5.2f", particles(p).snr), ...
				'edgecolor', 'none', 'horizontalalignment', 'center');
				
% 			uicontrol(fig, 'Style', 'text', 'FontSize', 12, 'String', ...
% 				join([char(951), "=", sprintf("%5.2f%% | ", 100*particles(p).spec_fits.params(1,:))]), ...
% 				 'Units', 'pixels', 'Position', [40, 360, 180, 20]);
% 			 
% 			mu = particles(p).spec_fits.params(2,:);
% 			if(Particle.visopt(3))	% Use eVs %
% 				str = join([char(956), "=" sprintf("%4.2f eV | ", mu)]);
% 			else
% 				str = join([char(956), "=", sprintf("%5.1f nm | ", Particle.HC ./ mu)]);
% 			end
% 			uicontrol(fig, 'Style', 'text', 'FontSize', 12, 'String', str, ...
% 				 'Units', 'pixels', 'Position', [40, 320, 180, 20]);
% 			 
% 			fwhm = particles(p).spec_fits.params(3,:);
% 			if(Particle.visopt(3))	% Use eVs %
% 				str = join(["FWHM =" sprintf("%4.2f eV | ", fwhm)]);
% 			else
% 				up = Particle.HC ./ (mu+fwhm/2);
% 				down = Particle.HC ./ (mu-fwhm/2);
% 				
% 				str = join(["FWHM =", sprintf("%4.1f nm | ", down-up)]);
% 			end
% 			uicontrol(fig, 'Style', 'text', 'FontSize', 12, 'String', str, ...
% 				 'Units', 'pixels', 'Position', [40, 280, 180, 20]);
% 			 
% 			str = join(["A =", sprintf("%5.2f | ", particles(p).spec_fits.params(4,:))]);
% 			uicontrol(fig, 'Style', 'text', 'FontSize', 12, 'String', str, ...
% 				 'Units', 'pixels', 'Position', [40, 240, 180, 20]);
			
			drawnow limitrate;
			savefig(fig, join([save_folder_frame, sprintf('(%d,%d)', ...
				particles(p).peak_pos(1), particles(p).peak_pos(2))], ''), 'compact');
			saveas(fig, join([save_folder_frame, sprintf('(%d,%d)', ...
				particles(p).peak_pos(1), particles(p).peak_pos(2)), '.png'], ''));
			%close(fig);
		end

		% Write data to File (Currently overwrites) %
		save(join([save_folder_frame, 'Found Particles.mat'], ''), 'data');

		disp(join(["Successfully Exported", num_parts, "Spectra..."]));
	end
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
	if(isempty(Frame.actidx))
		activeFrame = parent.UserData.Frames(1);
	else
		activeFrame = parent.UserData.Frames(Frame.actidx);
	end
	
	% Update the box colors on the main image %
	activeFrame.Particles(preVal).DispBox();				% Color red %
	activeFrame.Particles(curVal).DispBox(UI.axs(1), true);	% Color green %
	
	% Plot the selected peak and spectrum images %
	Particle.S_DispPeak(activeFrame.Particles(curVal));
	Particle.S_DispSpec(activeFrame.Particles(curVal));
	
	% Plot the selected spectrum if available %
	Particle.S_DispPlot(activeFrame.Particles(curVal));
end

