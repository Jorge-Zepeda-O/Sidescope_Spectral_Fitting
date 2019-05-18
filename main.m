%% MAINWIN %%
% Here we will set up the main window that we'll be using everything for %
MainWin = figure('Name', 'Sidescope Pro', 'Menubar', 'none', ...
				 'NumberTitle', 'off', 'DoubleBuffer', 'on', ...
				 'Position', [100, 100, 1800, 900]);
MainWin.UserData.DEBUG = false;
 
% Add in a menu for toggleable parameters / quick functions %
MainWin.UserData.menus = {};
menu_file = uimenu(MainWin, 'Text', 'File');
	MakeMenu(MainWin, menu_file, "Load Image", 'off', @menu_load_OnClick);
	
menu_plt = uimenu(MainWin, 'Text', 'Plotting');
	MakeMenu(MainWin, menu_plt, "Use eVs", 'off', @menu_evs_OnClick);
			 
% Make the axes we will be using %
MainWin.UserData.axs = {};
MakeAxes(MainWin, [0.00, 0.25, 0.40, 0.75], "Original Image", ...
	"X (px)", "Y (px)", [1,1,1]);

MakeAxes(MainWin, [0.375, 0.75, 0.20, 0.25], "Selected Peak Image", ...
	"X (px)", "Y (px)", [1,1,1]);
MakeAxes(MainWin, [0.55, 0.75, 0.475, 0.25], "Selected Spectrum Image", ...
	"Wavelength (nm)", "Y (px)");

MakeAxes(MainWin, [0.55, 0.25, 0.45, 0.50], "Selected Spectrum", ...
	"Wavelength (nm)", "Intensity (arb.)");

%% UI CONTROLS %%
MainWin.UserData.ctrls = {};

% Buttons %
MakeButton(MainWin, [0.05, 0.15, 0.10, 0.05], "Zoom", 'zoom', true);
MakeButton(MainWin, [0.05, 0.10, 0.10, 0.05], "Pan", 'pan', true);

MakeButton(MainWin, [0.20, 0.15, 0.15, 0.05], "Select Particles", @sel_part_OnClick);
MakeButton(MainWin, [0.20, 0.10, 0.15, 0.05], "Fit Selection", @fit_part_OnClick);

% Fit Parameter Sliders %
MakeParamSlider(MainWin, [0.40, 0.12, 0.15, 0.05], "Window Radius", ...
	[5, 10, 15, 0.1, 0.2], 1, true, '%2.0f(px)', @(val) val);
MakeParamSlider(MainWin, [0.40, 0.06, 0.15, 0.05], ['Filter ', char(963)], ...
	[1, 3, 6], 2, false, '%4.2f(px)', @(val) val);

MakeParamSlider(MainWin, [0.60, 0.18, 0.15, 0.05], "SNR Threshold", ...
	[1, 3, 10, 1/90, 1/9], 3, false, '%5.1f', @(val) val^2);
MakeParamSlider(MainWin, [0.60, 0.12, 0.15, 0.05], "Max Lorentzians", ...
	[1, 2, 5, 0.25, 0.5], 4, true, '%1.0f', @(val) val);
MakeParamSlider(MainWin, [0.60, 0.06, 0.15, 0.05], "Fit Iterations", ...
	[0, 10, 20, 0.05, 0.25], 5, true, '%2.0f', @(val) val);

% List Boxes %
MakeListbox(MainWin, [0.40, 0.25, 0.15, 0.49], "Found Particles", ...
	@found_part_OnValueChanged);
SetControlParam(MainWin, "lbx: Found Particles", 'Enable', 'off');

%% UI FUNCTIONS %%
function [ax] = MakeAxes(parent, pos, ttl, xlbl, ylbl, aspect)
	%% Make the Axes %%
	ax = axes(parent, 'OuterPosition', pos, 'Tag', join(["ax:", ttl]));
	
	% Give it a title and other goodies %
	ax.Title.String = ttl;
	ax.YDir = 'normal';
	grid(ax, 'on');
	
	if(nargin > 3), xlabel(ax, xlbl); end
	if(nargin > 4), ylabel(ax, ylbl); end
	if(nargin > 5), ax.PlotBoxAspectRatio = aspect; end
	
	%% Update the UserData %%
	parent.UserData.axs{end+1} = ax;
end
function [menu] = MakeMenu(parent, host, txt, chk, cb)
	%% Argument Defaults %%
	if(nargin < 3), chk = 'off'; end
	
	%% Initialize %%
	menu = uimenu(host, 'Text', txt, 'Checked', chk, 'Tag', join(["menu:", txt]));
	
	% Callback %
	if(nargin > 3)
		menu.Callback = @(src, event) cb(parent, src, event);
	end
	
	%% Update the UserData %%
	parent.UserData.menus{end+1} = menu;
end

function [btn] = MakeButton(parent, pos, string, callback, toggle)
% Makes a button UI control in 'parent' with normalized position 'position'.  The
% button contains text specified by 'string', and calls 'callback' when pressed with
% arguments specified by 'varargin'.
	
	if(nargin < 5), toggle = false; end
	
	if(toggle), buttonType = 'Togglebutton'; else, buttonType = 'Pushbutton'; end

	% Create a uicontrol that is a button %
	btn = uicontrol(parent, 'Style', buttonType, 'Fontsize', 12, ...
		'Units', 'normalized', 'Position', pos, ...
		'String', string, 'Tag', join(["btn:", string]));
	
	% Determine if the callback is a character array or not %
	if(ischar(callback))
		% Then call the callback function without arguments - probably built in %
		btn.Callback = callback;
	else
		% Call the callback function with the arguments given by varargin and pass in
		% a reference to parent, which is where we store relevant information
		if(toggle)
			btn.Callback = @(src, evargs) callback(parent, btn);
		else
			btn.Callback = @(src, evargs) callback(parent);
		end
	end
end

function [fsld, numlbl] = MakeFrameSlider(parent, pos, tag, vals, cb)
	%% Create the Slider %%
	fsld = uicontrol(parent, 'Style', 'slider', 'Fontsize', 12, ...
		'Units', 'normalized', ...
		'Position', [pos(1), pos(2) + pos(4)/2, pos(3), pos(4)/2], ...
		'Tag', join(["sld:", tag]), ...
		'Min', vals(1), 'Value', vals(2), 'Max', vals(3));
	if(length(vals) == 5), fsld.SliderStep = vals(4:5); end
	
	% Initialize the UserData value %
	parent.UserData.Frames.current = fsld.Value;
	
	%% Create the Associated Numeric Label %%
	numlbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
		'Units', 'normalized', ...
		'Position', [pos(1), pos(2), pos(3), pos(4)/2], ...
		'String', join(["Frame:", fsld.Value]), ...
		'Tag', join(["nlbl:", tag]), 'HorizontalAlignment', 'center');
	
	% Set up the OnValueChanged callback for the slider %
	fsld.Callback = @(src, event) cb(parent, src, numlbl);
	
	%% Update the UserData %%
	parent.UserData.ctrls{end+1} = fsld;
	parent.UserData.ctrls{end+1} = numlbl;
end
function [psld, numlbl] = MakeParamSlider(parent, pos, str, vals, varargin)
	%% Create the Slider %%
	psld = uicontrol(parent, 'Style', 'slider', 'Fontsize', 12, ...
		'Units', 'normalized', ...
		'Position', [pos(1), pos(2) + pos(4)/2, pos(3), pos(4)/2], ...
		'Tag', join(["psld:", str]), ...
		'Min', vals(1), 'Value', vals(2), 'Max', vals(3));
	if(length(vals) == 5), psld.SliderStep = vals(4:5); end
	
	%% Create the Associated Numeric Label %%
	numlbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
		'Units', 'normalized', ...
		'Position', [pos(1), pos(2), pos(3), pos(4)/2], ...
		'Tag', join(["numlbl:", str]), ...
		'HorizontalAlignment', 'center');
	
	% Parse the varargin and keep track of it in the slider's UserData %
	[psld.UserData.arg, psld.UserData.rnd, ...
		numlbl.UserData.fcode, numlbl.UserData.valfxn] = varargin{:};
	numlbl.UserData.str = str;

	%% Update the UserData %%
	% Set up the OnValueChanged callback %
	psld.Callback = @(src, event) ...
		ctrl_psld_OnValueChanged(parent, src, numlbl);
	
	% Refresh the slider and its label %
	ctrl_psld_OnValueChanged(parent, psld, numlbl);
			
	% Concatenate these controls to the pile %
	parent.UserData.ctrls{end+1} = psld;
	parent.UserData.ctrls{end+1} = numlbl;
end

function [lbx, ttllbl] = MakeListbox(parent, pos, str, callback)
	%% Create the ListBox %%
	lbx = uicontrol(parent, 'Style', 'listbox', 'FontSize', 12, ...
		'Units', 'normalized', ...
		'Position', [pos(1), pos(2), pos(3), pos(4) - 0.04], ...
		'Tag', join(["lbx:", str]));
	lbx.Callback = @(src, evargs) callback(src, parent);
	
	%% Create the Title Label %%
	ttllbl = uicontrol(parent, 'Style', 'text', 'FontSize', 12, 'FontWeight', 'bold', ...
		'Units', 'normalized', ...
		'Position', [pos(1), pos(2) + pos(4) - 0.04, pos(3), 0.04], ...
		'String', str, 'Tag', join(["ttllbx:", str]), 'HorizontalAlignment', 'center');
	
	%% Update the UserData %%			
	% Concatenate these controls to the pile %
	parent.UserData.ctrls{end+1} = lbx;
	parent.UserData.ctrls{end+1} = ttllbl;
end

function [ctrl] = SetControlParam(parent, tag, param, value)
	%% Find the control %%
	for c = 1:length(parent.UserData.ctrls)
		if(strcmp(parent.UserData.ctrls{c}.Tag, tag))
			ctrl = parent.UserData.ctrls{c};
			break;
		end
	end
	
	%% Set the parameter to the given value %%
	set(ctrl, param, value);
end

%% UI CALLBACKS %%
function menu_load_OnClick(parent, ~, ~)
	%% Initialize %%	
	% Prompt the user for the image file - they are forced to one folder %
	[filenames, filefolder] = uigetfile({'*.tif', '*.mptiff'}, 'MultiSelect', 'on');
	imgs = {};
	
	%% File Loading %%
	% Make the filenames into a cell if it isn't already %
	if(~iscell(filenames)), filenames = {filenames}; end
	
	% Read all files, concatenating them by frame %
	for f = 1:length(filenames)
		% Concatenate the file folder and each file name to get each file path %
		filepath = [filefolder, filenames{f}];

		% Read the entire image and add it to the list of images %
		frames = Load_TIFF(filepath);
		for t = 1:size(frames, 3)
			img = Image(frames(:,:,t), filenames{f});
			imgs{end+1} = img;
		end
	end

	%% UserData Management %%
	% Refresh the previous UserData fields %
	parent.UserData.Frames.imgs = imgs;	% Refresh the image(s)
	parent.UserData.Frames.num = length(imgs);
	
	parent.UserData.Particles = {};			% Clear the particles
	
	%% Visualization %%
	% Plot the thresholded image (first frame) on the Original Image axes %
	parent.UserData.Frames.imgs{1}.DispImg(parent.UserData.axs{1});
	
	% Draw the Frame Slider %
	if(parent.UserData.Frames.num > 1)
		MakeFrameSlider(parent, [0.05, 0.25, 0.30, 0.05], "Frame Control", ...
			[1, 1, parent.UserData.Frames.num], @ctrl_fsld_OnValueChanged); 
	else
		parent.UserData.Frames.current = 1;
	end
end
function menu_evs_OnClick(parent, obj, ~)
	%% Initialize %%
	
	
	%% Toggle Checked State %%
	if(strcmp(obj.Checked, 'on'))
		obj.Checked = 'off';
	else
		obj.Checked = 'on';
	end
end

function ctrl_fsld_OnValueChanged(parent, fsld, numlbl)
	% The value must be an integer, so round it and update the slider value %
	val = round(fsld.Value);

	% Update the UserData value %
	parent.UserData.Frames.current = val;

	% Update the associated numeric label %
	numlbl.String = join(["Frame:", val]);
	
	%% Update Visualization %%
	parent.UserData.Frames.imgs{val}.DispImg(parent.UserData.axs{1});
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
	delete(parent.UserData.axs{1}.Children(1:end-1));
	parent.UserData.Particles = {};

	%% Select New Particles %%
	% Function wrapper for selecting the particles in each frame %
	parent.UserData.Frames.imgs{parent.UserData.Frames.current}.Select(parent);
	
	% Make sure to draw a box around every found particle and its spectrum %
	for p = 1:length(parent.UserData.Particles)
		if(p == 1)
			parent.UserData.Particles{p}.DispBox(parent.UserData.axs{1}, [0, 0.8, 0]);
		else
			parent.UserData.Particles{p}.DispBox(parent.UserData.axs{1});
		end
	end
	
	%% MainWin Update %%	
	% Activate the Found Particles Listbox %
	SetControlParam(parent, "lbx: Found Particles", 'Enable', 'on');
	
	% Update the Listbox title %
	SetControlParam(parent, "ttllbx: Found Particles", 'String', ...
		join(["Found Particles:", length(parent.UserData.Particles)]));
	
	% Clear the spectrum plot axes %
	cla(parent.UserData.axs{4}, 'reset');
	text(parent.UserData.axs{4}, 0.35, 0.5, "Please fit the selected spectra");
end
function fit_part_OnClick(parent, ~, ~)
	%% Preliminaries %%
	% Obtain some parameters from the sliders %
	params = num2cell(parent.UserData.Fitting.params);
	[filt_rad, sel_sigma, param.thresh, param.lorentz, param.iter] = params{:};
	spec_rad = Particle.SPEC_RAD;
	
	% Make the selection Filters %
	xgrid = -filt_rad:filt_rad;
	ygrid = -spec_rad:spec_rad;
	
	flt.sel = exp(- xgrid.^2 / (2*sel_sigma^2));	% Gaussian weighting %
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
	for p = 1:length(parent.UserData.Particles)
		parent.UserData.Particles{p}.FitSpec(parent, param, flt);
	end
	
	%% MainWin Update %%
	% Display the fit that's currently selected %
	listbox = findobj(parent.Children, 'flat', 'Tag', "lbx: Found Particles");
	parent.UserData.Particles{listbox.Value}.DispPlt(parent.UserData.axs{4});
end
function found_part_OnValueChanged(obj, parent)
	% Get the previous and current selected values %
	curVal = obj.Value;
	preVal = obj.UserData.preVal;
	obj.UserData.preVal = curVal;

	%% MainWin Update %%
	% Plot the new peak and spectrum images %
	parent.UserData.Particles{curVal}.DispImg(parent.UserData.axs{2}, parent.UserData.axs{3});
	
	% Update the box colors in the main image %
	parent.UserData.Particles{preVal}.DispBox(parent.UserData.axs{1});	% Red out %
	parent.UserData.Particles{curVal}.DispBox(parent.UserData.axs{1}, [1, 1, 1]);
	
	% Plot the spectrum plot if needed %
	parent.UserData.Particles{curVal}.DispPlt(parent.UserData.axs{4});
end