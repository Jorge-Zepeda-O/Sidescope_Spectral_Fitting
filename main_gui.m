% Help goes here
% Blah blah

%% GLOBAL CONSTANTS %%
NUM_FRAME = 50;

THRESHOLD = 4;
CUT_OFF = 1;	% ???

BOX_SIZE = 13;
BOX_SPEC = 250;

% Selection of appropriate boxes %
SEL_RAD  = 16;	% Selection Radius %
SPEC_OFF = 350;	% Spectrum Offset (713 for 2048, ~350 for 1200) %
SPEC_RAD = 107;	% Spectrum Radius (Based on nm_grid %

% For spectral viewing %
HC = (6.626E-34 * 2.998E8) * (1E9/1.602E-19);	% For hw (eV) = hc / lambda (nm) %
EV_RNG	= [1.5, 3.0];			% in eV %
NM_RNG	= HC ./ flip(EV_RNG);	% in nm - flipped so (1) < (2) %

%% INITIALIZATION %%
% Set up the figure we will be using %
MainWin = figure('Name', 'sCMOS_Fit', 'Menubar', 'none', ...
				 'NumberTitle', 'off', 'DoubleBuffer', 'on', ...
				 'Position', [100, 100, 1000, 800]);

%% UI CONTROLS %%
% Read Image %
but_readimg = MakeButton(MainWin, [0.75, 0.95, 0.20, 0.05], "Load Image", @ReadImage);

% Zooming / Panning %
but_zoomim	= MakeButton(MainWin, [0.75, 0.85, 0.20, 0.05], "Zoom Image", 'zoom');
but_panimg	= MakeButton(MainWin, [0.75, 0.80, 0.20, 0.05], "Pan Image", 'pan');

% Particle Selection %
but_selpt	= MakeButton(MainWin, [0.75, 0.70, 0.20, 0.05], "Select Particle", ...
						@Select.Particle, SEL_RAD, SPEC_OFF, SPEC_RAD);
					
% Spectrum Fitting %
but_specfit = MakeButton(MainWin, [0.75, 0.65, 0.20, 0.05], "Fit Spectrum", 'FitSpectrum');

% Fit Parameters (input) %
lbl_Fitparam = MakeLabel(MainWin, [0.75, 0.60, 0.20, 0.03], "Fit Parameters:", 'center', 'bold');

lbl_selsig   = MakeLabel(MainWin, [0.75, 0.56, 0.12, 0.03], ['Window ', char(963), ' (px)'], 'right');
txt_selsig   = MakeTextbox(MainWin, [0.89, 0.56, 0.06, 0.03], "4");

lbl_selthr   = MakeLabel(MainWin, [0.75, 0.52, 0.12, 0.03], "Threshold (%)", 'right');
txt_selthr   = MakeTextbox(MainWin, [0.89, 0.52, 0.06, 0.03], "15");

lbl_correct  = MakeLabel(MainWin, [0.75, 0.48, 0.12, 0.03], "Corrections", 'right');
txt_correct  = MakeTextbox(MainWin, [0.89, 0.48, 0.06, 0.03], "10");

tog_useevs	 = MakeTogglebutton(MainWin, [0.80, 0.44, 0.10, 0.03], "Use eVs");

% Fit Values (output) %
lbl_Fitvals  = MakeLabel(MainWin, [0.75, 0.40, 0.20, 0.03], "Fit Values:", 'center', 'bold');

lbl_Avals(1) = MakeLabel(MainWin, [0.75, 0.36, 0.03, 0.03], "A=", 'right');
lbl_Avals(2) = MakeLabel(MainWin, [0.78, 0.36, 0.085, 0.03], '>A(1)', 'center');
lbl_Avals(3) = MakeLabel(MainWin, [0.87, 0.36, 0.085, 0.03], '>A(2)', 'center');

% \lambda = char(955) - Character values found thanks to Unicode %
lbl_Xvals(1) = MakeLabel(MainWin, [0.75, 0.32, 0.03, 0.03], [char(955), '='], 'right');
lbl_Xvals(2) = MakeLabel(MainWin, [0.78, 0.32, 0.085, 0.03], '>x_0(1)', 'center');
lbl_Xvals(3) = MakeLabel(MainWin, [0.87, 0.32, 0.085, 0.03], '>x_0(2)', 'center');

% \Gamma = char(915) %
lbl_Gvals(1) = MakeLabel(MainWin, [0.75, 0.28, 0.03, 0.03], [char(915), '='], 'right');
lbl_Gvals(2) = MakeLabel(MainWin, [0.78, 0.28, 0.085, 0.03], '>Gamma(1)', 'center');
lbl_Gvals(3) = MakeLabel(MainWin, [0.87, 0.28, 0.085, 0.03], '>Gamma(2)', 'center');

%% HELPER FUNCTIONS %%
function [] = ReadImage(parent, ~)
	%% READ IMAGE %%
	% Obtain the .tif / .mptiff file to analyze %
	[FileName, FileFolder] = uigetfile({'*.tif', '*.mptiff'});
	FilePath = [FileFolder, FileName]; % Concatenate the name and folder into the path %

	% Read the .tif / .mptiff file and take only the first frame %
	Img = LoadTIFF(FilePath, 1);

	% Output the image to the figure %
	if(~isfield(parent.UserData, 'ImgAxes'))
		ImgAxes = axes('OuterPosition', [0.00, 0.25, 0.75, 0.75]);
	else
		ImgAxes = parent.UserData.ImgAxes;
	end
	Select.PlotImg(ImgAxes, Img, "Original Image");

	% Save the image to the figure's UserData %
	parent.UserData.Img = Img;
	parent.UserData.ImgAxes = ImgAxes;
end

function [but] = MakeButton(parent, position, string, callback, varargin)
	but = uicontrol(parent, 'Style', 'Pushbutton', 'FontSize', 12, ...
				'Units', 'normalized', 'Position', position, ...
				'String', string, 'Tag', string);
	if(ischar(callback))
		but.Callback = callback;
	else
		but.Callback = @(source, eventargs) callback(parent, varargin{:});
	end
end
function [lbl] = MakeLabel(parent, position, string, align, weight)
	if(nargin < 4), align = 'center'; end
	if(nargin < 5), weight = 'normal'; end
	
	if(string(1) == '>')
		lbl = uicontrol(parent, 'Style', 'text', 'FontSize', 12, ...
				'Units', 'normalized', 'Position', position, ...
				'HorizontalAlignment', align, 'FontWeight', weight, ...
				'Tag', string(2:end));
	else
		lbl = uicontrol(parent, 'Style', 'text', 'FontSize', 12, ...
				'Units', 'normalized', 'Position', position, ...
				'HorizontalAlignment', align, 'FontWeight', weight, ...
				'String', string, 'Tag', string);
	end
end
function [txt] = MakeTextbox(parent, position, string)
	txt = uicontrol(parent, 'Style', 'edit', 'FontSize', 12, ...
				'Units', 'normalized', 'Position', position, ...
				'String', string, 'Tag', string);
end
function [tog] = MakeTogglebutton(parent, position, string)
	tog = uicontrol(parent, 'Style', 'ToggleButton', 'FontSize', 12, ...
				'Units', 'normalized', 'Position', position, ...
				'String', string, 'Tag', string);
end