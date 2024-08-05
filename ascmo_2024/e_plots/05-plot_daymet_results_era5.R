# change this to where your processed data are placed
processed_data_dir = "~/OneDrive - The University of Colorado Denver/Research/In Progress/functional-cm-comparison/analysis/processed_data/"

# source directory with fcmc.functions.R
source_dir = "~/OneDrive - The University of Colorado Denver/Research/In Progress/functional-cm-comparison/"

# results directories
results_dir_Daymet = "~/OneDrive - The University of Colorado Denver/Research/In Progress/functional-cm-comparison/analysis/results_Daymet"
results_dir_subDaymet = "~/OneDrive - The University of Colorado Denver/Research/In Progress/functional-cm-comparison/analysis/results_sub_Daymet"
results_plot = "~/OneDrive - The University of Colorado Denver/Research/In Progress/functional-cm-comparison/analysis/results_plots/"

# read functions file
setwd(source_dir)
source('fcmc_functions.R')

setwd(processed_data_dir)
# read lon and lat coordinates for Daymet data
lon = readRDS(file = "lon_nacordex_44i_Daymet.rds")
lat = readRDS(file = "lat_nacordex_44i_Daymet.rds")

# distributional equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_de_Daymet_era5.rda")
load(file = "sp_test_de_Daymet_era5.rda")
std_Daymet = std_perm_de_Daymet_era5
sp_Daymet = sp_test_de_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_de_Daymet_era5.rda")
load(file = "sp_test_de_Daymet_era5.rda")
std_subDaymet = std_perm_de_Daymet_era5
sp_subDaymet = sp_test_de_Daymet_era5

setwd(results_plot)
pdf("de_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of distributional equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# mean equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_test_mean_Daymet_era5.rda")
load(file = "sp_test_mean_Daymet_era5.rda")
std_Daymet = std_perm_test_mean_Daymet_era5
sp_Daymet = sp_test_mean_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_mean_Daymet_era5.rda")
load(file = "sp_test_mean_Daymet_era5.rda")
std_subDaymet = std_perm_test_mean_Daymet_era5
sp_subDaymet = sp_test_mean_Daymet_era5

setwd(results_plot)
pdf("mean_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year mean equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# sd equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_test_sd_Daymet_era5.rda")
load(file = "sp_test_sd_Daymet_era5.rda")
std_Daymet = std_perm_test_sd_Daymet_era5
sp_Daymet = sp_test_sd_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_sd_Daymet_era5.rda")
load(file = "sp_test_sd_Daymet_era5.rda")
std_subDaymet = std_perm_test_sd_Daymet_era5
sp_subDaymet = sp_test_sd_Daymet_era5

setwd(results_plot)
pdf("sd_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year sd equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# iqr equality tests
setwd(results_dir_Daymet)
load(file = "std_perm_test_iqr_Daymet_era5.rda")
load(file = "sp_test_iqr_Daymet_era5.rda")
std_Daymet = std_perm_test_iqr_Daymet_era5
sp_Daymet = sp_test_iqr_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_iqr_Daymet_era5.rda")
load(file = "sp_test_iqr_Daymet_era5.rda")
std_subDaymet = std_perm_test_iqr_Daymet_era5
sp_subDaymet = sp_test_iqr_Daymet_era5

setwd(results_plot)
pdf("iqr_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year iqr equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# q05 equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_test_q05_Daymet_era5.rda")
load(file = "sp_test_q05_Daymet_era5.rda")
std_Daymet = std_perm_test_q05_Daymet_era5
sp_Daymet = sp_test_q05_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_q05_Daymet_era5.rda")
load(file = "sp_test_q05_Daymet_era5.rda")
std_subDaymet = std_perm_test_q05_Daymet_era5
sp_subDaymet = sp_test_q05_Daymet_era5

setwd(results_plot)
pdf("q05_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year q05 equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# q25 equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_test_q25_Daymet_era5.rda")
load(file = "sp_test_q25_Daymet_era5.rda")
std_Daymet = std_perm_test_q25_Daymet_era5
sp_Daymet = sp_test_q25_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_q25_Daymet_era5.rda")
load(file = "sp_test_q25_Daymet_era5.rda")
std_subDaymet = std_perm_test_q25_Daymet_era5
sp_subDaymet = sp_test_q25_Daymet_era5

setwd(results_plot)
pdf("q25_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year q25 equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# q50 equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_test_q50_Daymet_era5.rda")
load(file = "sp_test_q50_Daymet_era5.rda")
std_Daymet = std_perm_test_q50_Daymet_era5
sp_Daymet = sp_test_q50_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_q50_Daymet_era5.rda")
load(file = "sp_test_q50_Daymet_era5.rda")
std_subDaymet = std_perm_test_q50_Daymet_era5
sp_subDaymet = sp_test_q50_Daymet_era5

setwd(results_plot)
pdf("q50_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year q50 equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# q75 equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_test_q75_Daymet_era5.rda")
load(file = "sp_test_q75_Daymet_era5.rda")
std_Daymet = std_perm_test_q75_Daymet_era5
sp_Daymet = sp_test_q75_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_q75_Daymet_era5.rda")
load(file = "sp_test_q75_Daymet_era5.rda")
std_subDaymet = std_perm_test_q75_Daymet_era5
sp_subDaymet = sp_test_q75_Daymet_era5

setwd(results_plot)
pdf("q75_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year q75 equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()

# q95 equality tests

setwd(results_dir_Daymet)
load(file = "std_perm_test_q95_Daymet_era5.rda")
load(file = "sp_test_q95_Daymet_era5.rda")
std_Daymet = std_perm_test_q95_Daymet_era5
sp_Daymet = sp_test_q95_Daymet_era5
setwd(results_dir_subDaymet)
load(file = "std_perm_test_q95_Daymet_era5.rda")
load(file = "sp_test_q95_Daymet_era5.rda")
std_subDaymet = std_perm_test_q95_Daymet_era5
sp_subDaymet = sp_test_q95_Daymet_era5

setwd(results_plot)
pdf("q95_test_plots_Daymet_era5.pdf", height = 7, width = 7)
plot_test_objects(lon, lat, list(std_Daymet, sp_Daymet, std_subDaymet, sp_subDaymet), alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard all", "(b) stratified all", "(c) standard sub", "(d) stratified sub"),
                  outer.title = "test of 26-year q95 equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues")
dev.off()
