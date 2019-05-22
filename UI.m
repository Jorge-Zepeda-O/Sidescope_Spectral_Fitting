classdef UI
% A static class for creating and mutating UI controls %
%
% Static variables:
%	> axs:		{Axes}
%	> menus:	{Menu}
%	> ctrls:	{UIControl}


%% STATIC METHODS %%
methods(Static)
	%% CONTROL CREATION %%
	function [menu] = MakeMenu(parent, host, txt, chk, cb, arg, sep)
		%% Argument Defaults %%
		if(nargin < 3), chk = 'off'; end

		%% Initialize %%
		menu = uimenu(host, 'Text', txt, 'Checked', chk, 'Tag', join(["menu:", txt]));

		% Callback %
		if(nargin > 5)
			menu.Callback = @(src, event) cb(parent, src, arg);
		elseif(nargin > 4)
			menu.Callback = @(src, event) cb(parent, src, event);
		end

		% Is this a separator menuitem? %
		if(nargin > 6)
			menu.Separator = sep; 
		end

		%% UI Update %%
		% Concatenate this menu to the list of menus %
		UI.menus_add(menu);
		
		%% No Output Handling %%
		if(nargout == 0), clear menu; end
	end
	
	function [ax] = MakeAxes(parent, pos, ttl, xlbl, ylbl)
		%% Make the Axes %%
		ax = axes(parent, 'OuterPosition', pos, 'Tag', join(["ax:", ttl]));

		% Give it a title and other goodies %
		ax.Title.String = ttl;
		ax.YDir = 'normal';
		grid(ax, 'on');

		if(nargin > 3), xlabel(ax, xlbl); end
		if(nargin > 4), ylabel(ax, ylbl); end

		%% UI Update %%
		% Concatenate these axes to the list of axes %
		UI.axs_add(ax);
		
		%% No Output Handling %%
		if(nargout == 0), clear ax; end
	end

	function [btn] = MakeButton(parent, pos, string, cb, tog, en)
	% Makes a button UI control in 'parent' with normalized position 'position'.  The
	% button contains text specified by 'string', and calls 'callback' when pressed

		if(nargin < 5), tog = false; end
		if(nargin < 6), en = 'on'; end

		if(tog), buttonType = 'Togglebutton'; else, buttonType = 'Pushbutton'; end

		% Create a uicontrol that is a button %
		btn = uicontrol(parent, 'Style', buttonType, 'Fontsize', 12, ...
			'Units', 'normalized', 'Position', pos, 'Enable', en, ...
			'String', string, 'Tag', join(["btn:", string]));

		% Determine if the callback is a character array or not %
		if(ischar(cb))
			% Then call the callback function without arguments - probably built in %
			btn.Callback = cb;
		else
			% Call the callback function with the arguments given by varargin and pass in
			% a reference to parent, which is where we store relevant information
			if(tog)
				btn.Callback = @(src, evargs) cb(parent, btn);
			else
				btn.Callback = @(src, evargs) cb(parent);
			end
		end
		
		%% UI Update %%
		% Concatenate this control to the list of controls %
		UI.ctrls_add(btn);
		
		%% No Output Handling %%
		if(nargout == 0), clear btn; end
	end

	function [fsld, numlbl] = MakeFrameSlider(parent, pos, tag, vals, cb)
		%% Create the Slider %%
		fsld = uicontrol(parent, 'Style', 'slider', 'Fontsize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2) + pos(4)/2, pos(3), pos(4)/2], ...
			'Tag', join(["sld:", tag]), ...
			'Min', vals(1), 'Value', vals(2), 'Max', vals(3));
		if(length(vals) == 5), fsld.SliderStep = vals(4:5); end

		%% Create the Associated Numeric Label %%
		numlbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2), pos(3), pos(4)/2], ...
			'String', join(["Frame:", fsld.Value]), ...
			'Tag', join(["nlbl:", tag]), 'HorizontalAlignment', 'center');

		% Set up the OnValueChanged callback for the slider %
		fsld.Callback = @(src, event) cb(parent, src, numlbl);

		%% UI Update %%
		% Concatenate these controls to the list of controls %
		UI.ctrls_add(fsld);
		UI.ctrls_add(numlbl);
		
		%% No Output Handling %%
		if(nargout == 0), clear fsld numlbl; end
	end
	function [psld, numlbl] = MakeParamSlider(parent, pos, str, vals, cb, varargin)
		%% Create the Slider %%
		psld = uicontrol(parent, 'Style', 'slider', 'Fontsize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2) + pos(4)/2, pos(3), pos(4)/2], ...
			'Tag', join(["psld:", str]), ...
			'Min', vals(1), 'Value', vals(2), 'Max', vals(3));
		
		% Adjust the slider step if need be %
		if(length(vals) == 5), psld.SliderStep = vals(4:5); end

		%% Create the Associated Numeric Label %%
		numlbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2), pos(3), pos(4)/2], ...
			'Tag', join(["numlbl:", str]), ...
			'HorizontalAlignment', 'center');
		
		% Pass in the given string for updating later %
		numlbl.UserData.str = str;

		%% Set up the Controls %%
		% Parse the varargin and keep track of it in the slider's UserData %
		[psld.UserData.arg, psld.UserData.rnd, ...
			numlbl.UserData.fcode, numlbl.UserData.valfxn] = varargin{:};
		
		% Set up the OnValueChanged callback %
		psld.Callback = @(src, event) cb(parent, src, numlbl);

		%% MainWin Update %%
		% Refresh the slider and its label %
		cb(parent, psld, numlbl);

		%% UI Update %%
		% Concatenate these controls to the list of controls %
		UI.ctrls_add(psld);
		UI.ctrls_add(numlbl);
		
		%% No Output Handling %%
		if(nargout == 0), clear psld numlbl; end
	end

	function [lbx, ttllbl] = MakeListbox(parent, pos, str, cb, en)
		%% Create the ListBox %%
		lbx = uicontrol(parent, 'Style', 'listbox', 'FontSize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2), pos(3), pos(4) - 0.02], ...
			'Tag', join(["lbx:", str]));
		lbx.Callback = @(src, evargs) cb(src, parent);
		lbx.Enable = en;

		%% Create the Title Label %%
		ttllbl = uicontrol(parent, 'Style', 'text', 'FontSize', 12, 'FontWeight', 'bold', ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2) + pos(4) - 0.02, pos(3), 0.02], ...
			'String', str, 'Tag', join(["ttllbx:", str]), 'HorizontalAlignment', 'center');

		%% UI Update %%
		% Concatenate these controls to the list of controls %
		UI.ctrls_add(lbx);
		UI.ctrls_add(ttllbl);
		
		%% No Output Handling %%
		if(nargout == 0), clear lbx ttllbl; end
	end

	%% CONTROL LOCATION %%
	function [obj] = FindObject(group, tag)
	% Finds an object in 'group' based on the Tag property.  'group' could be any
	% array or cell of UIControls/Axes/Menus, but it is preferred to use the
	% persistent variables in UI as groups.
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> group:	[GraphicsPlaceholder] A group of objects with a 'Tag' property
	%	> tag:		("str") The exact string to look for.  Must be complete.
	
		%% Procedure %%
		obj = findobj(group, 'flat', 'Tag', tag);
	end
	
	%% CONTROL MUTATION %%
	function SetControlParam(group, tag, param, value)
		%% Procedure %%
		set(UI.FindObject(group, tag), param, value);
	end
	function EnableControl(group, tag, en)
		%% Arugment Defaults %%
		if(nargin < 3), en = 'on'; end
		
		%% Procedure %%
		UI.SetControlParam(group, tag, 'Enable', en);
	end
end

%% PERSISTENT VARIABLE METHODS %%
methods(Static)	
	% Axes %
	function [value] = axs(val, write)
		persistent axs;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)					% Spit out the entire array %
			value = axs;
		elseif(nargin < 2 || ~write)	% Read out the specific index given by val % 
			value = axs(val);
		elseif(write)					% Write to the value
			axs = val;
		end
	end
	function axs_add(data)
		% Get the previous value of this property %
		prev = UI.axs;
		
		% Check if it was empty %
		if(isempty(prev))
			% If so, set it to the new value %
			UI.axs(data, true);
		else
			% Else append it to the list %
			UI.axs([prev, data], true); 
		end
	end
	
	% Menus %
	function [value] = menus(val, write)
		persistent menus;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)					% Spit out the entire array %
			value = menus;
		elseif(nargin < 2 || ~write)	% Read out the specific index given by val % 
			value = menus(val);
		elseif(write)					% Write to the value
			menus = val;
		end
	end
	function menus_add(data)
		% Get the previous value of this property %
		prev = UI.menus;
		
		% Check if it was empty %
		if(isempty(prev))
			% If so, set it to the new value %
			UI.menus(data, true); 
		else
			% Else append it to the list %
			UI.menus([prev, data], true); 
		end
	end
	
	% Controls %
	function [value] = ctrls(val, write)
		% Establish the persistent variable %
		persistent ctrls;
		
		% Change behavior based on the number of input arguments - 1D arrays only %
		if(nargin == 0)					% Spit out the entire array %
			value = ctrls;
		elseif(nargin < 2 || ~write)	% Read out the specific index given by val % 
			value = ctrls(val);
		elseif(write)					% Write to the value
			ctrls = val;
		end
	end
	function ctrls_add(data)
		% Get the previous value of this property %
		prev = UI.ctrls;
		
		% Check if it was empty %
		if(isempty(prev))
			% If so, set it to the new value %
			UI.ctrls(data, true); 
		else
			% Else append it to the list %
			UI.ctrls([prev, data], true); 
		end
	end
end
end