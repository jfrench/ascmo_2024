source("fcmc_functions_v2.R")

# change this to where your processed data are placed
processed_data_dir = "./_processed_data"

# directory to save results
results_dir = "./_results_Daymet"

# determine if running on cluster
os <- Sys.info()["sysname"]
attr(os, "names") <- NULL
mc_cores <- ifelse(os == "Windows", 1L, 20L)

load(file.path(processed_data_dir, "Daymet_data.rda"))

xsta$starray = xsta$starray[1:127,,]
xsta$idx = xsta$idx[1:127,]

B = 29 # number of simulations

fpath = file.path(results_dir,
                  "std_perm_tests_de_Daymet_era5.rda")

perm_idxs = prepare_perm_idxs(dim(xsta$starray)[3], 1)

set.seed(55)
if (!file.exists(fpath)) {

  std_perm_de_Daymet_era5 = 
    perm_test_de_sta(xsta, perm_idxs, cl = mc_cores)
  
  save(std_perm_de_Daymet_era5,
       file = fpath,
       compress = "xz")

}
load(fpath)
std_perm_de_Daymet_era5$tstats_global
std_perm_de_Daymet_era5$pvalue_global

set.seed(56)
fpath = file.path(results_dir, "std_perm_test_mean_Daymet_era5.rda")
if (!file.exists(fpath)) {

  std_perm_test_mean_Daymet_era5 =
    perm_test_sta(xsta, stat_func = mean,
                  perm_idxs = perm_idxs,
                  alternative = "alternative",
                  cl = mc_cores)

  save(std_perm_test_mean_Daymet_era5,
       file = fpath,
       compress = "xz")
}

set.seed(57)

fpath = file.path(results_dir, "std_perm_test_q50_Daymet_era5.rda")
if (!file.exists(fpath)) {

  std_perm_test_q50_Daymet_era5 =
    perm_test_sta(xsta, stat_func = median,
                  perm_idxs = perm_idxs,
                  alternative = "alternative",
                  cl = mc_cores)

  save(std_perm_test_q50_Daymet_era5,
       file = fpath,
       compress = "xz")
}

set.seed(58)
fpath = file.path(results_dir, "std_perm_test_sd_Daymet_era5.rda")
if (!file.exists(fpath)) {

  std_perm_test_sd_Daymet_era5 =
    perm_test_sta(xsta, stat_func = sd,
                  perm_idxs = perm_idxs,
                  alternative = "alternative",
                  cl = mc_cores)

  save(std_perm_test_sd_Daymet_era5,
       file = fpath,
       compress = "xz")
}

set.seed(59)
fpath = file.path(results_dir, "std_perm_test_iqr_Daymet_era5.rda")
if (!file.exists(fpath)) {

  std_perm_test_iqr_Daymet_era5 =
    perm_test_sta(xsta, stat_func = iqr,
                  perm_idxs = perm_idxs,
                  alternative = "alternative",
                  cl = mc_cores)

  save(std_perm_test_iqr_Daymet_era5,
       file = fpath,
       compress = "xz")
}

set.seed(60)
fpath = file.path(results_dir, "std_perm_test_smooth_Daymet_era5.rda")
if (!file.exists(fpath)) {

  std_perm_test_smooth_Daymet_era5 =
    perm_test_bs_coefs_sta(xsta, perm_idxs = perm_idxs, cl = mc_cores)

  save(std_perm_test_smooth_Daymet_era5,
       file = fpath,
       compress = "xz")
}

set.seed(61)
fpath = file.path(results_dir, "sp_test_de_Daymet_era5.rda")
if(!file.exists(fpath)) {

  sp_test_de_Daymet_era5 =
    strat_perm_test_de_sta(xsta, nsim = B, cl = mc_cores)

  save(sp_test_de_Daymet_era5,
       file = fpath,
       compress = "xz")


}
load(fpath)
sp_test_de_Daymet_era5$tstats_global
sp_test_de_Daymet_era5$pvalue_global

set.seed(62)
fpath = file.path(results_dir, "sp_test_mean_Daymet_era5.rda")
if (!file.exists(fpath)) {
  
  sp_test_mean_Daymet_era5 = 
    strat_perm_test_sta(xsta, stat_func = mean,
                        alternative = "two.sided",
                        nsim = B, cl = mc_cores)

  save(sp_test_mean_Daymet_era5,
       file = fpath,
       compress = "xz")
}

set.seed(63)
fpath = file.path(results_dir, "sp_test_q50_Daymet_era5.rda")
if (!file.exists(fpath)) {

  sp_test_q50_Daymet_era5 = 
    strat_perm_test_sta(xsta, stat_func = median,
                        alternative = "two.sided",
                        nsim = B, cl = mc_cores)

  save(sp_test_q50_Daymet_era5,
       file = fpath,
       compress = "xz")
  
}

set.seed(64)
fpath = file.path(results_dir, "sp_test_sd_Daymet_era5.rda")
if (!file.exists(fpath)) {

  sp_test_sd_Daymet_era5 = 
    strat_perm_test_sta(xsta, stat_func = sd,
                        alternative = "two.sided",
                        nsim = B, cl = mc_cores)
 
  save(sp_test_sd_Daymet_era5,
       file = fpath,
       compress = "xz")
}

set.seed(65)
fpath = file.path(results_dir, "sp_test_iqr_Daymet_era5.rda")
if (!file.exists(fpath)) {

  sp_test_iqr_Daymet_era5 = 
    strat_perm_test_sta(xsta, stat_func = iqr,
                        alternative = "two.sided",
                        nsim = B, cl = mc_cores)

  save(sp_test_iqr_Daymet_era5,
       file = fpath,
       compress = "xz")
}


set.seed(66)
fpath = file.path(results_dir, "sp_test_smooth_Daymet_era5.rda")
if (!file.exists(fpath)) {

  sp_test_smooth_Daymet_era5 = 
    strat_perm_test_bs_coefs_sta(xsta, nsim = B, cl = mc_cores)
  
  save(sp_test_smooth_Daymet_era5,
       file = fpath,
       compress = "xz")
}