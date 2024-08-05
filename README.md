# Code to reproduce the analysis in "Spatio-temporal functional permutation tests for comparing observed climate behavior to climate model projections" by Joshua P. French, Piotr S. Kokoszka, and Seth McGinnis.

ascmo_2024.zip contains the code used to produce the results of this paper using R 4.3.1. The data files are not provided due to their large size. Users will have to download the files manually, which limits the reproducibility of this analysis. However, we have provided the code we used to analyze the data once the data were downloaded. The folders and other non-zip files in the asco_2024 folder are the information contained in ascmo_2024.zip.

Ensure that "./analysis" is the current working directory in R. Opening the analysis.Rproj file should set up a suitable environment for analysis.

The analyses should be run in order starting with the files in:

a_download_data
b_format_data
c_analysis
d_simulation
e_plots

Within each folder, the files should be run in order.

The folders starting with "_" are used to organize results.

The fcmc_*.R scripts contain functions needed to perform the analysis.

All code is licensed under the GNU General Public License v3.0.
