classdef OPT < handle
% A static class that holds all the options and tunable values available to the user.
% Additionally, it contains some methods useful for loading and saving preferences 
% for higher throughput, as they will not need to redo their settings each time.

%% CONSTANT PROPERTIES %%
% The only constant properties in this class are the default settings.  Values dubbed
% with a '+' indicate that the default value is 'true', while those with a '-' will
% default to 'false'.  Parameters that have a numeric value are denoted with '(#)'.
properties(Constant)
	%% Operation Modes %%
	% + Illumination correction
	% - Automate background subtraction (Experimental)
	% -----------------------------------------------------
	% + Use Lorentzian-Gaussian Hybridization (L-G Hyb.)
	% + Automate peak fitting (Experimental)
	DEFAULT_OPMODE = [1, 0, 1, 1];
	
	%% Visualization Options %%
	% + Show scale bars
	% - Show selection filter range
	% -----------------------------------------------------
	% - Use eVs when plotting (else use nm)
	% + Show signal spectrum
	% + Show fit decomposition
	% + Show hybridization colors
	DEFAULT_VISUAL = [1, 0, 0, 1, 1, 1, 1];
	
	%% Parameter Values %%
	% (10)	Window radius (in px)
	% ( 3)	Filter standard deviation (in px)
	% ( 2)	Number of peaks to fit
	DEFAULT_PARVAL = [10, 3, 2];
end

%% STATIC PROPERTIES %%
%	> opmode:	[T/F]	A boolean vector that determines how the program will operate
%	> visual:	[T/F]	A boolean vector that determines how things appear in figures
%	> parval:	[#]		A numeric vector that determines several values to use
methods(Static)
	function [value] = opmode(val, write)	% Operation Mode %
		% Keep track of the value throughout calls to this method - it's the closest
		% thing we have to static variables in MATLAB...
		persistent static
		
		%% Determine Behavior %%
		% If no arguments are given, return the entire value of 'opmode'.  If one
		% argument is given, read out the index given by that value.  If both
		% arguments are given, overwrite the value of 'opmode' with the values given
		% in the first argument.  If the second argument is not a logical, then 
		% overwrite the indicies given in 'write'
		if(nargin == 0)
			value = static;
		elseif(nargin == 1)
			value = static(val);
		else
			if(islogical(write))
				static = val;
			else
				prev = static;
				prev(write) = val;
				static = prev;
			end
		end
	end
	function [value] = visual(val, write)	% Visualization Options %
		% Keep track of the value throughout calls to this method - it's the closest
		% thing we have to static variables in MATLAB...
		persistent static
		
		%% Determine Behavior %%
		% If no arguments are given, return the entire value of 'opmode'.  If one
		% argument is given, read out the index given by that value.  If both
		% arguments are given, overwrite the value of 'opmode' with the values given
		% in the first argument.  If the second argument is not a logical, then 
		% overwrite the indicies given in 'write'
		if(nargin == 0)
			value = static;
		elseif(nargin == 1)
			value = static(val);
		else
			if(islogical(write))
				static = val;
			else
				prev = static;
				prev(write) = val;
				static = prev;
			end
		end
	end
	function [value] = parval(val, write)	% Parameter Values %
		% Keep track of the value throughout calls to this method - it's the closest
		% thing we have to static variables in MATLAB...
		persistent static
		
		%% Determine Behavior %%
		% If no arguments are given, return the entire value of 'opmode'.  If one
		% argument is given, read out the index given by that value.  If both
		% arguments are given, overwrite the value of 'opmode' with the values given
		% in the first argument.  If the second argument is not a logical, then 
		% overwrite the indicies given in 'write'
		if(nargin == 0)
			value = static;
		elseif(nargin == 1)
			value = static(val);
		else
			if(islogical(write))
				static = val;
			else
				prev = static;
				prev(write) = val;
				static = prev;
			end
		end
	end
end

%% STATIC METHODS %%
methods(Static)
	%% File Handling %%
	function S_Load()
	% This function looks to see if a preferences file exists, and if so, loads it
	% into memory, then assigns the relevant parameters to static parameters in OPT.
	% If there is no such file, then it runs with the default settings.
	
		%% Load Preferences %%
		% Attempt to load the preferences file; if at any point it fails, then we
		% have to run with the defaults with an error message
		try
			% Load the values from that preference file %
			load('preferences.mat');
			
			% Assign those values %
			OPT.opmode(pref.opmode, true);
			OPT.visual(pref.visual, true);
			OPT.parval(pref.parval, true);
		catch
			% Display an warning to console %
			if(exist('preferences.mat', 'file'))
				% The file doesn't exist %
				warning("No preferences file exists... " + ...
					"Creating one with default values...");
			else
				% There was something wrong with the file %
				warning("There is something wrong with your 'preferences.mat' file... " + ...
					"Reverting to default preferences...");
			end
			
			% Run with the defaults %
			OPT.opmode(OPT.DEFAULT_OPMODE, true);
			OPT.visual(OPT.DEFAULT_VISUAL, true);
			OPT.parval(OPT.DEFAULT_PARVAL, true);
			
			% Write the defaults to file so we have a working 'pref.mat' file for
			% later use.
			OPT.S_Save();
		end
	end
	function S_Save()
	% This function saves the current preferences in a file called 'pref.mat' and
	% overwrites any previous preferences.  Essentially, just copy down the values
	% into a struct and save the struct.
	
		pref.opmode = OPT.opmode;
		pref.visual = OPT.visual;
		pref.parval = OPT.parval;
		save('preferences.mat', 'pref');
	end
end
end