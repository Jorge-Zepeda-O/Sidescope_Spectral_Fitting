% Main run file for Sidescope Pro.  Within this file, we design the GUI and implement
% various callback functions to encapsulate all the interesting functionality we
% seek.  Many of the compartmentalized functions corresponding to various objects are
% separated into their own classes (or static classes for more general functions.)
%
% - Written by Jorge Zepeda O (2019 - jz48@rice.edu)

%% Refresh %%
% Clear the static variables lying around %
%GUI.S_REFRESH();	% Clears the UI components %

% Load in the preferences %
OPT.S_Load();

%% Design MainWin %%
% MainWin is the name for the gui that the user will be controlling for the duration
% of their time with Sidescope Pro.  It is so named for being the main window used.
% That said, in its initialization, we need to get the screen dimensions and aspect
% ratio to use for our gui.
screen = get(0, 'screensize');	% Gets the screen size [0, 0, width, height]	%
mw_dim = [1600, 800];			% Go for a 2:1 aspect ratio, image and spectrum %

% Create the figure that we'll be using %
MainWin = figure('Name', 'Sidescope Pro', ...
				 'NumberTitle', 'off', ...
				 'Menubar', 'none', ...
				 'DoubleBuffer', 'on', ...
				 'Position', [screen(3:4) - mw_dim, 2*mw_dim]/2);
			 
%% Design Menus %%
% Menus are included in MainWin for the sake of toggling certain features on or off
% or performing functions that only require a click, but would be bothersome to place
% as a button.  In this manner, features that are usually set once can be categorized
% neatly and kept out of the way.  We will go menu by menu:

% - File Menu -
% Allows for loading one or more images for analysis
menu_file = uimenu(MainWin, 'Text', "File");
	GUI.New_Menu(menu_file, "Load Image", @menu_load_OnClick);
	
% - Operation Mode Menu -
menu_opmode = uimenu(MainWin, 'Text', "Operation");
	GUI.New_Menu(menu_opmode, "Illumination Correction",				 @Menu_opmode_OnClick, OPT.opmode, 1);
	GUI.New_Menu(menu_opmode, "Automate background subtraction",		 @Menu_opmode_OnClick, OPT.opmode, 2);
	GUI.New_Menu(menu_opmode, "Allow Lorentzian-Gaussian Hybridization", @Menu_opmode_OnClick, OPT.opmode, 3, true);
	GUI.New_Menu(menu_opmode, "Automate number of peaks to fit",		 @Menu_opmode_OnClick, OPT.opmode, 4);

% - Visualization Options Menu -


%% Callbacks %%
