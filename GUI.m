classdef GUI < handle
% A static class that contains references for all the user interface objects like
% axes, controls, or menus.

%% STATIC METHODS %%
methods(Static)
	%% Control Creation %%
	% These methods involve creation of new uicontrols, often with common parameters
	% as arguments to be input.  Controls that can be built in this manner include:
	%	
	%	- Labels
	%	- Buttons
	%	- Sliders (with dynamic numeric labels)
	%	- Input Fields (with static unit labels)
	%	- Axes
	%	- Menus
	function [m] = New_Menu(host, txt, cb, idx, sep)
	% Creates a new uimenu whose host is 'host' and contains text 'txt'.  On click,
	% it calls the callback method 'cb'.  This usually modifies a boolean array, 
	% which is indexed by 'idx'.  It can also be set as a separator menu item using 
	% 'sep'.
	%	------------------------------------------------------------------------
	%	Arugment Defaults:
	%	> host:	(GraphicsPlaceholder)	The host of this menu item.  Usually is
	%		either MainWin or another menu item (for the case of nested trees)
	%	
	%	> txt:	("str")		The displayed contents of this menu.
	%	
	%	> cb:	(@f(mw, src, arg))	The callback function for clicking this menu item
	%
	%	~ idx:	(i)		An integer corresponding to the index of a value in a 
	%		boolean array to modify.  The boolean array is specified by the callback
	%		function, which will be called to set the checked status of this menu.
	%			(~)		Default: 0 (Does not modify)
	%
	%	~ sep:	(T/F)	Does this menu item serve as a separator?
	%			(~)		Default: false (Does not separate)
	
		%% Argument Defaults %%
		if(nargin < 4), idx = 0; end
		if(nargin < 5), sep = false; end
		
		%% Initialize %%
		% Construct the base uimenu item and attach it to the host %
		m = uimenu(host, 'Text', txt, 'Tag', join(["m:", txt]));
		
		% Determine if the menu item is a separator or not %
		if(sep), m.Separator = 'on';
		else,	 m.Separator = 'off';
		end

		%% Instantiate Behaviors %%
		% Instantiate the callback function %
		if(nargin > 2)
			% We need MainWin, this menu item, and the argument it supplied for what
			% we're concerned about calling.  MainWin is for accessing Frames, this
			% menu item to change the 'Checked' property, and 'idx' to change the
			% parameter saught for, if applicable.  If 'idx' is zero, then the
			% callback function shouldn't use it.
			m.Callback = @(src, ev) cb(mw, src, idx);
		end
		
		%% Determine State %%		
		% Call the callback function in order to set the checked state to the default
		% state.  It seems rather ineffiecient, but it allows for this function to
		% hold many different groups of functions in mind easily.
		if(idx > 0), cb(mw, m, idx, true); end
	end
end

end