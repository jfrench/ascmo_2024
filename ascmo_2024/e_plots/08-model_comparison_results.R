source('fcmc_functions.R')

results_dir_gridMET = "./_results_gridMET"
results_dir_Daymet = "./_results_Daymet"
processed_data_dir = "./_processed_data"
plot_dir = "./_plots"

lon = readRDS(file = file.path(processed_data_dir,
                               "lon_nacordex_44i_gridMET.rds"))
lat = readRDS(file = file.path(processed_data_dir,
                               "lat_nacordex_44i_gridMET.rds"))

### distributional equality

load(file.path(results_dir_gridMET, "std_perm_de_gridMET_era5.rda"))
tail(std_perm_de_gridMET_era5$tstats_global, 1)
std_perm_de_gridMET_era5$pvalue_global

load(file.path(results_dir_gridMET, "sp_test_de_gridMET_era5.rda"))
tail(sp_test_de_gridMET_era5$tstats_global, 1)
sp_test_de_gridMET_era5$pvalue_global

load(file.path(results_dir_Daymet, "std_perm_de_Daymet_era5.rda"))
tail(std_perm_de_Daymet_era5$tstats_global, 1)
std_perm_de_Daymet_era5$pvalue_global

load(file.path(results_dir_Daymet, "sp_test_de_Daymet_era5.rda"))
tail(sp_test_de_Daymet_era5$tstats_global, 1)
sp_test_de_Daymet_era5$pvalue_global

# perform BY-adjustment
sp_test_de_gridMET_era5_by = sp_test_de_gridMET_era5
sp_test_de_gridMET_era5_by$pvalues = p.adjust(sp_test_de_gridMET_era5_by$pvalues,
                                              method = "BY")
sp_test_de_Daymet_era5_by = sp_test_de_Daymet_era5
sp_test_de_Daymet_era5_by$pvalues = p.adjust(sp_test_de_Daymet_era5_by$pvalues,
                                              method = "BY")

pdf(file.path(plot_dir, "de_test_plots_uncorrected.pdf"),
    height = 7, width = 7)
plot_test_objects(lon, lat, 
                  list(std_perm_de_gridMET_era5,
                       sp_test_de_gridMET_era5,
                       std_perm_de_Daymet_era5,
                       sp_test_de_Daymet_era5),
                  alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) standard gridMET", "(b) stratified gridMET",
                           "(c) standard Daymet", "(d) stratified Daymet"),
                  outer.title = "Tests of distributional equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues",
                  lratio = 0.2)
dev.off()

pdf(file.path(plot_dir, "de_test_plots.pdf"),
    height = 4, width = 7)
plot_test_objects(lon, lat, 
                  list(sp_test_de_gridMET_era5_by,
                       sp_test_de_Daymet_era5_by),
                  alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) gridMET", "(b) Daymet"),
                  outer.title = "Adjusted tests of distributional equality",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues",
                  lratio = 0.2)
dev.off()


### 55-year mean

### distributional equality

load(file.path(results_dir_gridMET, "sp_test_mean_gridMET_era5.rda"))
load(file.path(results_dir_Daymet, "sp_test_mean_Daymet_era5.rda"))

# perform BY-adjustment
sp_test_mean_gridMET_era5_by = sp_test_mean_gridMET_era5
sp_test_mean_gridMET_era5_by$pvalues = p.adjust(sp_test_mean_gridMET_era5_by$pvalues,
                                              method = "BY")
sp_test_mean_Daymet_era5_by = sp_test_mean_Daymet_era5
sp_test_mean_Daymet_era5_by$pvalues = p.adjust(sp_test_mean_Daymet_era5_by$pvalues,
                                             method = "BY")

### 55-year median

load(file.path(results_dir_gridMET, "sp_test_q50_gridMET_era5.rda"))
load(file.path(results_dir_Daymet, "sp_test_q50_Daymet_era5.rda"))

# perform BY-adjustment
sp_test_q50_gridMET_era5_by = sp_test_q50_gridMET_era5
sp_test_q50_gridMET_era5_by$pvalues = p.adjust(sp_test_q50_gridMET_era5_by$pvalues,
                                               method = "BY")
sp_test_q50_Daymet_era5_by = sp_test_q50_Daymet_era5
sp_test_q50_Daymet_era5_by$pvalues = p.adjust(sp_test_q50_Daymet_era5_by$pvalues,
                                              method = "BY")
pdf(file.path(plot_dir, "center_test_plots.pdf"),
    height = 7, width = 7)
plot_test_objects(lon, lat, 
                  list(sp_test_mean_gridMET_era5_by,
                       sp_test_mean_Daymet_era5_by,
                       sp_test_q50_gridMET_era5_by,
                       sp_test_q50_Daymet_era5_by),
                  alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) mean gridMET", "(b) mean Daymet",
                           "(c) median gridMET", "(b) median Daymet"),
                  outer.title = "Adjusted tests for measures of center",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues",
                  lratio = 0.2)
dev.off()

### 55-year sd

load(file.path(results_dir_gridMET, "sp_test_sd_gridMET_era5.rda"))
load(file.path(results_dir_Daymet, "sp_test_sd_Daymet_era5.rda"))

# perform BY-adjustment
sp_test_sd_gridMET_era5_by = sp_test_sd_gridMET_era5
sp_test_sd_gridMET_era5_by$pvalues = p.adjust(sp_test_sd_gridMET_era5_by$pvalues,
                                                method = "BY")
sp_test_sd_Daymet_era5_by = sp_test_sd_Daymet_era5
sp_test_sd_Daymet_era5_by$pvalues = p.adjust(sp_test_sd_Daymet_era5_by$pvalues,
                                               method = "BY")

### 55-year iqr

load(file.path(results_dir_gridMET, "sp_test_iqr_gridMET_era5.rda"))
load(file.path(results_dir_Daymet, "sp_test_iqr_Daymet_era5.rda"))

# perform BY-adjustment
sp_test_iqr_gridMET_era5_by = sp_test_iqr_gridMET_era5
sp_test_iqr_gridMET_era5_by$pvalues = p.adjust(sp_test_iqr_gridMET_era5_by$pvalues,
                                              method = "BY")
sp_test_iqr_Daymet_era5_by = sp_test_iqr_Daymet_era5
sp_test_iqr_Daymet_era5_by$pvalues = p.adjust(sp_test_iqr_Daymet_era5_by$pvalues,
                                             method = "BY")

pdf(file.path(plot_dir, "spread_test_plots.pdf"),
    height = 7, width = 7)
plot_test_objects(lon, lat, 
                  list(sp_test_sd_gridMET_era5_by,
                       sp_test_sd_Daymet_era5_by,
                       sp_test_iqr_gridMET_era5_by,
                       sp_test_iqr_Daymet_era5_by),
                  alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) sd gridMET", "(b) sd Daymet",
                           "(c) iqr gridMET", "(b) iqr Daymet"),
                  outer.title = "Adjusted tests for measures of spread",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues",
                  lratio = 0.2)
dev.off()

### 55-year smooth

load(file.path(results_dir_gridMET, "sp_test_smooth_gridMET_era5.rda"))
load(file.path(results_dir_Daymet, "sp_test_smooth_Daymet_era5.rda"))

# perform BY-adjustment
sp_test_smooth_gridMET_era5_by = sp_test_smooth_gridMET_era5
sp_test_smooth_gridMET_era5_by$pvalues = p.adjust(sp_test_smooth_gridMET_era5_by$pvalues,
                                               method = "BY")
sp_test_smooth_Daymet_era5_by = sp_test_smooth_Daymet_era5
sp_test_smooth_Daymet_era5_by$pvalues = p.adjust(sp_test_smooth_Daymet_era5_by$pvalues,
                                              method = "BY")

pdf(file.path(plot_dir, "smooth_test_plots.pdf"),
    height = 4, width = 7)
plot_test_objects(lon, lat, 
                  list(sp_test_smooth_gridMET_era5_by,
                       sp_test_smooth_Daymet_era5_by),
                  alpha = 0.10, map = "world",
                  xlab = "longitude", ylab = "latitude",
                  main = c("(a) gridMET", "(b) Daymet"),
                  outer.title = "Adjusted tests of equality of basis coefficients",
                  breaks = c(0, 0.001, 0.01, 0.05, 0.10),
                  col = viridisLite::viridis(4),
                  type = "pvalues",
                  lratio = 0.2)
dev.off()
