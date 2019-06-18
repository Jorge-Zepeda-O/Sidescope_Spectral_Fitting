classdef CALL < handle
% A static class to hold all of the callback functions used for uicontrols and menus
% in MainWin.  This is mostly so main.m doesn't get bloated with methods, and allows
% for tighter organization.

%% STATIC METHODS %%
methods(Static)
	%% Menus %%
	function Menu_imload(mw, ~, ~, ~)
	% Callback function that prompts the user to select one or more TIFF files from a
	% folder, then parses through them creating multiple Frame objects.  It selects the
	% first Frame and displays it in the 'Original Image' axes.
	%	----------------------------------------------------------------------------	
	%	Argument Definitions:
	%	> parent:	(MainWin) A reference to the Figure handle MainWin.  Useful for
	%		storing and keeping track of many different instance variables.
	end
	function Menu_opmode(~, src, idx, init)
	% Callback function for the menus dealing with operation mode.  Changes the value
	% of the vector 'OPT.opmode' at index 'idx'.  If 'init' is true, it will set the
	% value of 'OPT.opmode(idx)' to the value determined by the preferences file, 
	% hence initializing the menu item.
	%
	% Due to the nature of these options often changing the very fabric of how the
	% program works, we won't be doing anything with these in-function.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	! mw:	(MainWin)	A reference to the Figure handle MainWin.
	%	> obj:	(Menu)		The menu handle that called this callback function.
	%	> idx:	(#)			The index to use in the array 'OPT.opmode'
	%	~ init:	(T/F)		Determines if we should use the value in preferences
	%			(~)			Default: false (toggle the current value)
	
		%% Arugment Defaults %%
		if(nargin < 4), init = false; end
		
		%% OPT Update %%
		% Check if we are initializing this value or not %
		if(~init)
			% If not, toggle the value of the parameter in OPT %
			OPT.opmode(~OPT.opmode(idx), idx)
		end
		
		%% MainWin Update %%
		% Assert the state of OPT.opmode(idx) into the checked state of the menu %
		if(OPT.opmode(idx)), src.Checked = 'on';
		else,				 src.Checked = 'off';
		end
	end
	function Menu_visual(mw, src, idx, init)
	% Callback function for the menus dealing with visualization options.  Changes
	% the value of the vector 'OPT.visual' at index 'idx'.  If 'init' is true, it
	% will set the value of 'OPT.visual(idx)' to the value determined by the
	% preferences file, hence initializing the menu item.
	%
	% Due to the nature of most of these options changing minor things, we will be
	% redrawing the relevant plots whenever one of these is changed.
	%	------------------------------------------------------------------------
	%	Argument Definitions:
	%	> mw:	(MainWin)	A reference to the Figure handle MainWin.
	%	> obj:	(Menu)		The menu handle that called this callback function.
	%	> idx:	(#)			The index to use in the array 'OPT.visual'
	%	~ init:	(T/F)		Determines if we should use the value in preferences
	%			(~)			Default: false (toggle the current value)
	
		%% Arugment Defaults %%
		if(nargin < 4), init = false; end
		
		%% OPT Update %%
		% Check if we are initializing this value or not %
		if(~init)
			% If not, toggle the value of the parameter in OPT %
			OPT.opmode(~OPT.opmode(idx), idx)
		end
		
		%% MainWin Update %%
		% Assert the state of OPT.opmode(idx) into the checked state of the menu %
		if(OPT.opmode(idx)), src.Checked = 'on';
		else,				 src.Checked = 'off';
		end
		
		%% UI Update %%
	end
end
end