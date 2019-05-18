% HELP ME

%% INITIALIZATION %%
% Set up the main figure that we'll be using %
MainWin = figure('Name', 'Sidescope Processor', 'Menubar', 'none', ...
				 'NumberTitle', 'off', 'DoubleBuffer', 'on', ...
				 'Position', [100, 100, 1000, 800]);
			 
% Make some axes that we will be using in the future %
ax_img = axes('OuterPosition', [0.00, 0.25, 0.75, 0.75]);	% Original Image %
MainWin.UserData.ax_img = ax_img;

ax_spec = axes('OuterPosition', [0.00, 0.00, 0.75, 0.25]);	% Spectrum of particle %
MainWin.UserData.ax_spec = ax_spec;

% Default some parameters to false %
MainWin.UserData.use_evs = false;
MainWin.UserData.specplt = false;
			 
%% UI CONTROLS - BUTTONS %%
% Read Image %
but_readimg = MakeButton(MainWin, [0.75, 0.925, 0.20, 0.05], ...
						"Load Image", @Read.Image);

% Transform the Image %
but_zoomimg	= MakeButton(MainWin, [0.75, 0.85, 0.20, 0.05], ...
						"Zoom Image", 'zoom');	% Built in MATLAB function %
but_zoomimg.Enable = 'off';

but_panimg	= MakeButton(MainWin, [0.75, 0.80, 0.20, 0.05], ...
						"Pan Image", 'pan');	% Built in MATLAB function %
but_panimg.Enable = 'off';

% Particle Identification and Fitting %
but_selpart = MakeButton(MainWin, [0.75, 0.725, 0.20, 0.05], ...
						"Select Particles", @Particle.Select);
but_selpart.Enable = 'off';

but_fitpart = MakeButton(MainWin, [0.75, 0.675, 0.20, 0.05], ...
						"Fit Selected Particles", @Particle.FitSelected);
but_fitpart.Enable = 'off';

%% UI CONTROLS - TEXT %%
lbl_fitpara = MakeLabel(MainWin, [0.75, 0.625, 0.20, 0.025], ...
						"Fitting Parameters", 'center', 'bold');
lbl_fndpart = MakeLabel(MainWin, [0.75, 0.225, 0.20, 0.025], ...
						"Found Particles", 'center', 'bold');
					
%% UI CONTROLS - SLIDERS & THEIR LABELS %%
sld_selsig	= MakeSlider(MainWin, [0.75, 0.575, 0.20, 0.05], ...
						['Filter ', char(963), ':'], 1, [0.5, 3, 5, 0.01, 0.2], ...
						false, '%1.2f (px)');
					
sld_selthr	= MakeSlider(MainWin, [0.75, 0.50, 0.20, 0.05], ...
						"Threshold:", 2, [0, 10, 20, 0.05, 0.25], ...
						true, '%2.0f %');
					
sld_fitlor	= MakeSlider(MainWin, [0.75, 0.425, 0.20, 0.05], ...
						"Max Lorentzians:", 3, [1, 2, 10, 0.10, 0.20], ...
						true, '%2.0f');
					
sld_fititr	= MakeSlider(MainWin, [0.75, 0.35, 0.20, 0.05], ...
						"Fit Iterations:", 4, [0, 10, 20, 0.05, 0.25], ...
						true, '%2.0f');
					
%% UI CONTROLS - TOGGLE BUTTONS %%
tog_useevs	= MakeButton(MainWin, [0.75, 0.30, 0.10, 0.03], ...
						"Use eVs", @useevs_callback, true);
					
tog_specplt	= MakeButton(MainWin, [0.85, 0.30, 0.10, 0.03], ...
						"Spec Plot", @specplt_callback, true);
					
%% UI CONTROLS - LIST BOXES %%
lbx_fndpart = MakeListbox(MainWin, [0.75, 0.025, 0.20, 0.20], ...
						"lbx: Found Particles", @fndpart_callback);
lbx_fndpart.Enable = 'off';
					
%% HELPER FUNCTIONS %%
% UI Controls %
function [but] = MakeButton(parent, pos, string, callback, toggle)
% Makes a button UI control in 'parent' with normalized position 'position'.  The
% button contains text specified by 'string', and calls 'callback' when pressed with
% arguments specified by 'varargin'.
	
	if(nargin < 5), toggle = false; end
	if(toggle), buttonType = 'Togglebutton'; else, buttonType = 'Pushbutton'; end

	% Create a uicontrol that is a button %
	but = uicontrol(parent, 'Style', buttonType, 'Fontsize', 12, ...
					'Units', 'normalized', 'Position', pos, ...
					'String', string, 'Tag', string);
	% We specify that 'Tag' = 'String' just so we can always call this button later %
	
	% Determine if the callback is a character array or not %
	if(ischar(callback))
		% Then call the callback function without arguments - probably built in %
		but.Callback = callback;
	else
		% Call the callback function with the arguments given by varargin and pass in
		% a reference to parent, which is where we store relevant information
		if(toggle)
			but.Callback = @(src, evargs) callback(but, parent);
		else
			but.Callback = @(src, evargs) callback(parent);
		end
	end
end
function [lbl] = MakeLabel(parent, pos, string, align, weight)

	%% Argument Defaults %%
	if(nargin < 4), align = 'center'; end
	if(nargin < 5), weight = 'normal'; end
	
	%% Initialization %%
	% Create a uicontrol that is a label %
	lbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
					'Units', 'normalized', 'Position', pos, ...
					'String', string, 'Tag', string, ...
					'HorizontalAlignment', align, 'FontWeight', weight);	
end
function [sld] = MakeSlider(parent, pos, string, arg, vals, rnd, code)
	%% Create the Slider %%
	% Create a uicontrol that is a slider %
	sld = uicontrol(parent, 'Style', 'slider', 'Fontsize', 12, ...
					'Units', 'normalized', ...
					'Position', [pos(1), pos(2) + pos(4)/2, pos(3), pos(4)/2], ...
					'Tag', join(["sld:", string]), ...
					'Min', vals(1), 'Value', vals(2), 'Max', vals(3));
	% We specify that 'Tag' = 'String' just so we can always call this button later %
	
	sldrng = vals(3) - vals(1);
	sld.SliderStep = [vals(4), vals(5)];
	
	% Initialize the value %
	parent.UserData.FitParams{arg} = sld.Value;
	
	% Set up the OnValueChanged callback %
	sld.Callback = @(src, evargs) SetVal(src);

	function SetVal(obj)
		% If the value needs to be rounded, do so %
		val = obj.Value;
		if(rnd), val = round(val); end
		
		% Update the global variable %
		parent.UserData.FitParams{arg} = val;

		% Update the associated label %
		dlbl.String = [sprintf(code(1:5), val), code(6:end)];
		
		obj.Value = val;
	end

	%% Create the static label %%
	slbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
					'Units', 'normalized', ...
					'Position', [pos(1), pos(2), pos(3)*2/3, pos(4)/2], ...
					'String', string, 'Tag', join(["slbl:", string]), ...
					'HorizontalAlignment', 'center');
				
	%% Create the dynamic label %%
	dlbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
					'Units', 'normalized', ...
					'Position', [pos(1) + pos(3)*2/3, pos(2), pos(3)/3, pos(4)/2], ...
					'String',[sprintf(code(1:5), sld.Value), code(6:end)], ...
					'Tag', join(["dlbl:", string]), 'HorizontalAlignment', 'left');
end
function [lbx] = MakeListbox(parent, pos, tag, callback)
	lbx = uicontrol(parent, 'Style', 'listbox', 'FontSize', 12, ...
					'Units', 'normalized', 'Position', pos, ...
					'Tag', tag);
	lbx.Callback = @(src, evargs) callback(lbx, parent);
end

% Callback functions that don't really fit elsewhere %
function useevs_callback(obj, parent)
	parent.UserData.use_evs = obj.Value;
end
function specplt_callback(obj, parent)
	if(~isempty(parent.UserData.Particles(1).grids))
		parent.UserData.specplt = obj.Value;
	else
		parent.UserData.specplt = false;
		obj.Value = false;
	end
end
function fndpart_callback(obj, parent)
	% Determine which item has been selected %
	sel_idx = obj.Value;
	
	% Draw a red box over the previous point we were looking at, if applicable, in an
	% attempt to erase it from our sight
	if(isfield(obj.UserData, 'prev_idx'))
		Show.Box(parent.UserData.ax_img, parent.UserData.Particles(obj.UserData.prev_idx));
	end
	
	% Draw a *green* box around the point we're looking at %
	Show.Box(parent.UserData.ax_img, parent.UserData.Particles(sel_idx), [0,1,0]);
	
	if(parent.UserData.specplt)
		% Plot that item in the spectrum axes %
		Show.SpecPlot(parent.UserData.ax_spec, parent.UserData.Particles(sel_idx), ...
					sel_idx, parent.UserData.use_evs);
% 		Show.Plot(parent.UserData.ax_spec, ...
% 					parent.UserData.Particles(sel_idx).grids, ...
% 					[parent.UserData.Particles(sel_idx).spec(:,3:end), ...
% 					 parent.UserData.Particles(sel_idx).best_fit], ...
% 					join(["Spectrum Plot for Particle", sel_idx, "|"]), ...
% 					parent.UserData.use_evs, ...
% 					[0,0,1; 0,0,0; 0,0.5,0], [1,2,2]);
		%legend(parent.UserData.ax_spec, "Signal", "Lorentzian Fit(s)");
	else
		% Show the associated spectrum image %
		img = parent.UserData.img;
		part = parent.UserData.Particles(sel_idx);
		spec_img = img(part.spos(2) + (-Particle.FILT_RAD:Particle.FILT_RAD), ...
					   part.spos(1) + (-Particle.SPEC_RAD:Particle.SPEC_RAD));
		
		Show.Image(parent.UserData.ax_spec, spec_img, ...
			join(["Spectrum Image for Particle", sel_idx]));
		axis(parent.UserData.ax_spec, 'normal');
		
		% Draw a box around the first standard deviation %
		Show.SpecBox(parent.UserData.ax_spec, parent.UserData.FitParams{1});
	end
	
	obj.UserData.prev_idx = sel_idx;
end