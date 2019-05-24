classdef UI
% A static class for creating and mutating UI controls

%% STATIC VARIABLES %%
%	> axs:		[Axes]
%	> menus:	[Menu]
%	> ctrls:	[UIControl]
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
	
	%% REFRESH %%
	function REFRESH()
	% Clears all static variables of their values and prepares the class for use
	
		%% Refresh Controls %%
		UI.axs([], true);	% Axes %
		UI.menus([], true);	% Menus %
		UI.ctrls([], true);	% UIControls %
	end
end

%% STATIC METHODS %%
methods(Static)
	%% CONTROL CREATION %%
	function [menu] = MakeMenu(mainwin, parent, txt, cb, arg, chk, sep)
		%% Argument Defaults %%
		if(nargin < 5), arg = 0; end		% Default to no argument %
		if(nargin < 6), chk = false; end	% Default to unchecked %
		if(nargin < 7), sep = false; end	% Default to not a separator item %

		%% Initialize %%
		menu = uimenu(parent, 'Text', txt, 'Tag', join(["menu:", txt]));

		% Is this menuitem checked? %
		if(chk)
			menu.Checked = 'on'; 
		else
			menu.Checked = 'off'; 
		end
		
		% Is this a separator menuitem? %
		if(sep)
			menu.Separator = 'on'; 
		else
			menu.Separator = 'off'; 
		end
		
		% Callback assignment %
		if(nargin > 3)
			menu.Callback = @(src, event) cb(mainwin, src, arg);
		end
		
		% Call the callback if there's an option that this is modifying (given away
		% by the fact that arg ~= 0)
		if(arg ~= 0), cb(mainwin, menu, arg); end
		
		%% UI Update %%
		% Concatenate this menu to the list of menus %
		UI.menus_add(menu);
		
		%% Null Output %%
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
		
		%% Null Output %%
		if(nargout == 0), clear ax; end
	end

	function [btn] = MakeButton(parent, pos, string, cb, en, tog)
	% Makes a button UI control in 'parent' with normalized position 'position'.  The
	% button contains text specified by 'string', and calls 'callback' when pressed

		%% Argument Defaults %%
		if(nargin < 5), en = true; end		% Defaults to enabled buttons %
		if(nargin < 6), tog = false; end	% Defaults to PushButtons %

		%% Initialization %%
		if(tog), buttonType = 'Togglebutton'; else, buttonType = 'Pushbutton'; end

		%% Create the Button %%
		% Create a uicontrol that is a button %
		btn = uicontrol(parent, 'Style', buttonType, 'Fontsize', 12, ...
			'Units', 'normalized', 'Position', pos, ...
			'String', string, 'Tag', join(["btn:", string]));

		% Determine if the callback is a character array or not %
		if(ischar(cb))
			% Then call the callback function without arguments - probably built in %
			btn.Callback = cb;
		else
			% Call the callback function with the arguments given by varargin and 
			% pass in a reference to parent, which is where we store relevant
			% information
			if(tog)
				btn.Callback = @(src, evargs) cb(parent, btn);
			else
				btn.Callback = @(src, evargs) cb(parent);
			end
		end
		
		% Determine if we need to disable this control for now %
		if(en)
			btn.Enable = 'on';
		else
			btn.Enable = 'off';
		end
		
		%% UI Update %%
		% Concatenate this control to the list of controls %
		UI.ctrls_add(btn);
		
		%% Null Output %%
		if(nargout == 0), clear btn; end
	end

	function [sld, numlbl] = MakeFrameSlider(parent, pos, tag, vals, cb, vis)
		%% Create the Slider %%
		sld = uicontrol(parent, 'Style', 'slider', 'Fontsize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2) + pos(4)/2, pos(3), pos(4)/2], ...
			'Tag', join(["sld:", tag]), ...
			'Min', vals(1), 'Value', vals(2), 'Max', vals(3));
		if(length(vals) == 5), sld.SliderStep = vals(4:5); end

		%% Create the Associated Numeric Label %%
		numlbl = uicontrol(parent, 'Style', 'text', 'Fontsize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2), pos(3), pos(4)/2], ...
			'String', join(["Frame:", sld.Value]), ...
			'Tag', join(["numlbl:", tag]), 'HorizontalAlignment', 'center');

		% Set up the OnValueChanged callback for the slider %
		sld.Callback = @(src, event) cb(parent, src, numlbl);

		%% MainWin Update %%
		% Are these controls visible? %
		if(vis)
			sld.Visible = 'on'; 
			numlbl.Visible = 'on'; 
		else
			sld.Visible = 'off'; 
			numlbl.Visible = 'off'; 
		end
		
		% Refresh the slider and its label %
		cb(parent, sld);
		
		%% UI Update %%
		% Concatenate these controls to the list of controls %
		UI.ctrls_add(sld);
		UI.ctrls_add(numlbl);
		
		%% Null Output %%
		if(nargout == 0), clear sld numlbl; end
	end
	function [sld, numlbl] = MakeParamSlider(parent, pos, str, vals, cb, ...
			arg, rnd, fcode, valfxn)
	% Function that makes a slider + label combination corresponding to a window
	% parameter value.  (Static variable in Frame)
	%
	%	Argument Definitions:
	%	> parent:
	%	> pos:
	%	> str:
	%	> vals:
	%	> cb:
	%	~ arg:
	%	~ rnd:
	%	~ fcode:	('chr')	A character code that determines how to
	%		present the numerical value encapsulated by this parameter
	%	~ valfxn:
	
		%% Default Arguments %%
		if(nargin < 6), arg = 0; end				% Default to no behavior %
		if(nargin < 7), rnd = false; end			% Default to not rounding %
		if(nargin < 8), fcode = '%0.f';	end			% Default to no formatting %
		if(nargin < 9), valfxn = @(val) val; end	% Default to the identity fxn %
		
		%% Create the Slider %%
		sld = uicontrol(parent, 'Style', 'slider', 'Fontsize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2) + pos(4)/2, pos(3), pos(4)/2], ...
			'Tag', join(["psld:", str]), ...
			'Min', vals(1), 'Value', vals(2), 'Max', vals(3));
		
		% Adjust the slider step if need be %
		if(length(vals) == 5), sld.SliderStep = vals(4:5); end

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
		sld.UserData.arg = arg;	% Argument value that this slider adjusts %
		sld.UserData.rnd = rnd;	% Is the value intended to be an integer? %
		
		numlbl.UserData.fcode = fcode;		% What formatting do we want? %
		numlbl.UserData.valfxn = valfxn;	% How do we want the values to vary? %
		
		% Set up the OnValueChanged callback %
		sld.Callback = @(src, event) cb(parent, src, numlbl);

		%% MainWin Update %%
		% Refresh the slider and its label %
		cb(parent, sld, numlbl);

		%% UI Update %%
		% Concatenate these controls to the list of controls %
		UI.ctrls_add(sld);
		UI.ctrls_add(numlbl);
		
		%% Null Output %%
		if(nargout == 0), clear sld numlbl; end
	end

	function [lbx, ttllbl] = MakeListbox(parent, pos, str, cb, en)
		%% Argument Defaults %%
		if(nargin < 5), en = true; end		% By default, enable the listbox %
		
		%% Create the ListBox %%
		lbx = uicontrol(parent, 'Style', 'listbox', 'FontSize', 12, ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2), pos(3), pos(4) - 0.02], ...
			'Tag', join(["lbx:", str]));
		
		% Attach a callback %
		lbx.Callback = @(src, evargs) cb(parent, src);
		
		% Determine if we need to disable this listbox or not %
		if(en)
			lbx.Enable = 'on';
		else
			lbx.Enable = 'off';
		end

		%% Create the Title Label %%
		ttllbl = uicontrol(parent, 'Style', 'text', 'FontSize', 12, 'FontWeight', 'bold', ...
			'Units', 'normalized', ...
			'Position', [pos(1), pos(2) + pos(4) - 0.02, pos(3), 0.02], ...
			'String', str, 'Tag', join(["ttllbx:", str]), 'HorizontalAlignment', 'center');

		%% UI Update %%
		% Concatenate these controls to the list of controls %
		UI.ctrls_add(lbx);
		UI.ctrls_add(ttllbl);
		
		%% Null Output %%
		if(nargout == 0), clear lbx ttllbl; end
	end

	%% CONTROL LOCATION %%
	function [obj] = FindObject(tag, group)
	% Finds an object in 'group' based on the Tag property.  'group' could be any
	% array or cell of UIControls/Axes/Menus, but it is preferred to use the
	% persistent variables in UI as groups.
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> tag:		("str") The exact string to look for.  Must be complete.
	%
	%	~ group:	[GraphicsPlaceholder] A group of objects with a 'Tag' property
	%				(~) Defaults to 'UI.ctrls', the most commonly requested group.
	%	------------------------------------------------------------------------
	%	Outputs:
	%	< obj:		[GraphicsPlaceholder] The UI object requested, if available
	
		%% Arugment Defaults %%
		if(nargin < 2), group = UI.ctrls; end
	
		%% Procedure %%
		% Find and return the object specified, if applicable %
		obj = findobj(group, 'flat', 'Tag', tag);
	end
	
	%% CONTROL MUTATION %%
	function Ctrl_Set(tag, group, param, value)
	% Finds an object in 'group' based on the Tag property, and sets the parameter
	% value given by 'param' to the value specified by 'value'.
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> tag:		("str") The exact string to look for.  Must be complete
	%	> group:	[GraphicsPlaceholder] A group of objects with a 'Tag' property
	%	> param:	('chr') The exact character array for the property in question
	%	> value:	(?) The value given to the parameter specified in 'param'

		%% Procedure %%
		% Find the object in question and set its parameter value appropriately %
		set(UI.FindObject(tag, group), param, value);
	end
	function [value] =  Ctrl_Get(tag, group, param)
	% Finds an object in 'group' based on the Tag property, and gets the value of the
	% parameter 'param'
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> tag:		("str") The exact string to look for.  Must be complete
	%	> group:	[GraphicsPlaceholder] A group of objects with a 'Tag' property
	%	> param:	('chr') The exact character array for the property in question
		
		%% Procedure %%
		% Find the object in question and get its parameter value %
		value = get(UI.FindObject(tag, group), param);
	end
	
	function Ctrl_Enable(tag, group, en)
	% Finds an object in 'group' based on the Tag property, and sets the 'Enable'
	% parameter to a value specified by 'en'.  Note 'en' is a boolean.
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> tag:		("str") The exact string to look for.  Must be complete
	%
	%	~ group:	[GraphicsPlaceholder] A group of objects with a 'Tag' property
	%				(~) Defaults to 'UI.ctrls', the most commonly requested group.
	%
	%	~ en:		(bool) A boolean to determine the visibility of this control
	%				(~) Defaults to 'true', as suggested by the function name.
		%% Arugment Defaults %%
		if(nargin < 2), group = UI.ctrls; end
		if(nargin < 3), en = true; end
		
		%% Procedure %%
		% Set the 'Enable' property based on the value of 'en' %
		if(en)
			UI.Ctrl_Set(tag, group, 'Enable', 'on');
		else
			UI.Ctrl_Set(tag, group, 'Enable', 'off');
		end
	end
	function Ctrl_Show(tag, group, vis)
	% Finds an object in 'group' based on the Tag property, and sets the 'Visible'
	% parameter to a value specified by 'vis'.  Note 'vis' is a boolean.
	%	------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> tag:		("str") The exact string to look for.  Must be complete
	%
	%	~ group:	[GraphicsPlaceholder] A group of objects with a 'Tag' property
	%				(~) Defaults to 'UI.ctrls', the most commonly requested group.
	%
	%	~ vis:		(bool) A boolean to determine the visibility of this control
	%				(~) Defaults to 'true', as suggested by the function name.
		%% Arugment Defaults %%
		if(nargin < 2), group = UI.ctrls; end
		if(nargin < 3), vis = 'on'; end
		
		%% Procedure %%
		% Set the 'Visbile' property based on the value of 'vis' %
		if(vis)
			UI.Ctrl_Set(tag, group, 'Visible', 'on');
		else
			UI.Ctrl_Set(tag, group, 'Visible', 'off');
		end
	end
end
end