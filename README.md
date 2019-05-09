# Sidescope_Spectral_Fitting
MATLAB code allowing for spectral analysis using the Landes Lab's Sidescope - Based on existing code written for the group.


CHANGELOG:
-- v0.3:

Refactored the code (phew) and added a whole bunch of functionality.  Notably:

- You have to follow the process, the program won't let you skip ahead

- You can select a region of the image and the program will find and locate all particles in that region (mostly)

- You can select the maximum number of Lorentzians you want to use when fitting

- You can also select the number of "fit iterations" to use

- All parameters have default (suggested) values and are on a spectrum

- The bottom graph now shows *either*:
   + The image of the spectrum with lines marking the center and 1 standard deviation (of the filter) away
   + The background-subtracted spectrum plot with the Lorentzians on top of it
   + You can swap between these when available using the "Spec Plot" togglebutton

- The spectrum plot now isolates the "best Lorentzian" based on closeness to 700nm and slightly on intensity.  needs work in all honesty.

- As the program finds particles, it updates a listbox in the bottom right
   + Selecting a particle will display that spectrum in the bottom plot
   + Additionally, a *GREEN* box around the particle and its spectrum appears in the main image instead of a red one.  This could really be done better in the code, as I'm just drawing another red box on top of the previous one, but oh well..

- When finding or fitting, the listbox entries update to show progress (even with a cute checkmark when fit!)
