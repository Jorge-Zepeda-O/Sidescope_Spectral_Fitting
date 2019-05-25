# Sidescope_Spectral_Fitting
MATLAB code allowing for spectral analysis using the Landes Lab's Sidescope - Based on existing code written for the group.

CHANGELOG:
-- v0.5.5:

Refactored the code (again) and allowed for Frames to host their Particles independent of each other.  Full functionality is on the way.  That said, some notable additions:

- Background correction is now handled properly, running over the entire image and obtaining the intensity values of the spectrum where there isn't any, so we have the background illumination.  We then subtract this from the selection and outlier images and get a much flatter signal.

- The spectrum plot looks *gorgeous* now.  The signal has an "error region" according to the SNR provided, as well as the ability to show the Amplitude Thresholds (the point where a fit's amplitude must exceed in order to be considered a fit) and a decomposition of the Lorentzian fits along with the total.

- Everything is a part of objects now.  Life is organized, if a little slower.

- DOCUMENTATION!  The files are now more green than not green.  It's a work in progress, but hopefully it's understandable what each function does.

- Introduced the Fit class, which can also be a general repository for fitting things other than Lorentzians.  The class is written rather generally in order to encapsulate the future uses of it.  For example, you could additionally fit a Gaussian could be done using another instance of the same class.

- Read Conventions.md for a list of rules I try to follow.  I need to update a few things in the code before it's actually correct.

That said, look forward to proper multi-Frame handling in the next update!  This includes multi-Frame selection and fitting, as well as exporting, and coming up with a clearer way to handle multiple selections.  The functionality is *mostly* built in, but it will definitely need some tweaking.

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
