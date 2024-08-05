set.seed(55)

# change this to where your processed data are placed
processed_data_dir = "./_processed_data"

# source directory with fcmc.functions.R
source_dir = "./"

# directory to save results
results_dir = "./_results_sub_Daymet"

# number of desired time steps
desired_length = 660

# determine if running on cluster
os <- Sys.info()["sysname"]
attr(os, "names") <- NULL
mc_cores <- ifelse(os == "Windows", 1L, 20L)

# list of all nacordex 44i files
m_files = c(
  "tmax.hist.CanESM2.RCA4.mon.NAM-44i.mbcn-Daymet.rds",
  #  "tmax.hist.CanESM2.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.CanESM2.CanRCM4.mon.NAM-44i.mbcn-Daymet.rds",
  #  "tmax.hist.EC-EARTH.RCA4.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.EC-EARTH.HIRHAM5.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.GEMatm-Can.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  #  "tmax.hist.GEMatm-MPI.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  #  "tmax.hist.MPI-ESM-LR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  #  "tmax.hist.MPI-ESM-MR.CRCM5-UQAM.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.GFDL-ESM2M.RegCM4.mon.NAM-44i.mbcn-Daymet.rds",
  #  "tmax.hist.GFDL-ESM2M.WRF.mon.NAM-44i.mbcn-Daymet.rds",
  #  "tmax.hist.HadGEM2-ES.WRF.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.HadGEM2-ES.RegCM4.mon.NAM-44i.mbcn-Daymet.rds",
  "tmax.hist.MPI-ESM-LR.WRF.mon.NAM-44i.mbcn-Daymet.rds"
  # "tmax.hist.MPI-ESM-LR.RegCM4.mon.NAM-44i.mbcn-Daymet.rds"
)

r_files = c(
  "era5_monthly_tmax_nacordex_44i_Daymet.rds"
)

# create list to store objects
x = vector("list", length(m_files) + length(r_files))
# create grouping variable
grp = factor(rep(c("m", "r"), times = c(length(m_files), length(r_files))))
# create stratifying variable
strata = factor(rep(1950:2004, each = 12))

for (i in seq_along(m_files)) {
  x[[i]] = readRDS(file.path(processed_data_dir, m_files[i]))
  dim3 = dim(x[[i]])[3]
  if (dim3 != desired_length) {
    desired_start = dim3 - desired_length + 1
    x[[i]] = x[[i]][,,seq(desired_start, dim3, by = 1)]
  }
}

# read reanalysis data
# adjust from Kelvin to Celsius
for (i in seq_along(r_files)) {
  x[[i + length(m_files)]] = readRDS(file.path(processed_data_dir,
                                               r_files[i]))
  # adjust if temperature in Kelvin not Celsius
  if (max(x[[i + length(m_files)]], na.rm = TRUE) > 150) {
    x[[i + length(m_files)]] = x[[i + length(m_files)]] - 273.15
  }

  dim3 = dim(x[[i + length(m_files)]])[3]

  if (dim3 != desired_length) {
    desired_start = dim3 - desired_length + 1
    x[[i + length(m_files)]] = x[[i + length(m_files)]][,,seq(desired_start, dim3, by = 1)]
  }
}

# read lon and lat coordinates for Daymet data
lon = readRDS(file = file.path(processed_data_dir,
                               "lon_nacordex_44i_Daymet.rds"))
lat = readRDS(file = file.path(processed_data_dir,
                               "lat_nacordex_44i_Daymet.rds"))

# read na information
namat = readRDS(file = file.path(processed_data_dir,
                                 "mask_nacordex_44i_Daymet.rds"))
idx = which(!is.na(namat), arr.ind = TRUE)

# read functions file
source('fcmc_functions.R')

# generate exact permutations for this test
# number of total files choose number of r files combinations
r_combn = combn(seq_along(x), length(r_files))
r_pos = length(m_files) + seq_along(r_files)
perm_idxs = matrix(nrow = length(x), ncol = ncol(r_combn))
# place them in last (length(r_files) rows)
perm_idxs[r_pos, ] = r_combn
for (i in seq_len(ncol(perm_idxs))) {
  tmp_r_idx = perm_idxs[r_pos, i]
  perm_idxs[seq_along(m_files), i] = setdiff(seq_along(x), tmp_r_idx)
}

if (!file.exists(
  file.path(results_dir, "std_perm_tests_de_Daymet_era5_subtest.pdf")
)) {
B = ncol(perm_idxs)
std_perm_de_Daymet_era5 = perm_test_de(x, grp = grp, nsim = B,
                                        idx = idx,
                                        cl = mc_cores,
                                        perm_idxs = perm_idxs)
save(std_perm_de_Daymet_era5,
     file = file.path(results_dir, "std_perm_de_Daymet_era5_subtest.rda"),
     compress = "xz")

std_perm_de_Daymet_era5$tstats_global
std_perm_de_Daymet_era5$pvalue_global

pdf(file.path(results_dir, "std_perm_tests_de_Daymet_era5_subtest.pdf"))
plot_test_object(lon, lat, std_perm_de_Daymet_era5, alpha = 0.10, map = "world")
title("mean")
dev.off()
}

set.seed(56)

B = 999

if(!file.exists(file.path(results_dir, "sp_test_de_Daymet_era5_subtest.pdf"))) {
  sp_test_de_Daymet_era5 = strat_perm_test_de(x, grp = grp, strata = strata,
                                               nsim = B, idx = idx,
                                               cl = mc_cores)
  save(sp_test_de_Daymet_era5,
       file = file.path(results_dir, "sp_test_de_Daymet_era5_subtest.rda"),
       compress = "xz")

  sp_test_de_Daymet_era5$tstats_global
  sp_test_de_Daymet_era5$pvalue_global
  
  pdf(file.path(results_dir, "sp_test_de_Daymet_era5_subtest.pdf"))
  plot_test_object(lon, lat, sp_test_de_Daymet_era5, alpha = 0.10, map = "world")
  title("mean")
  dev.off()
}

set.seed(57)

if (!file.exists(file.path(results_dir, "std_perm_tests_Daymet_era5_subtest.pdf"))) {
B = ncol(perm_idxs)
std_perm_test_mean_Daymet_era5 = perm_test(x, grp, stat_func = mean,
                                            alternative = "two.sided",
                                            nsim = B, idx = idx,
                                            perm_idxs = perm_idxs,
                                            cl = mc_cores)

std_perm_test_sd_Daymet_era5 = perm_test(x, grp, stat_func = sd,
                                          alternative = "two.sided",
                                          nsim = B, idx = idx,
                                          perm_idxs = perm_idxs,
                                          cl = mc_cores)

# std_perm_test_q05_Daymet_era5 = perm_test(x, grp, stat_func = quantile,
#                                            alternative = "two.sided",
#                                            nsim = B, idx = idx,
#                                            prob = 0.05, perm_idxs = perm_idxs,
#                                            cl = mc_cores)
# 
# std_perm_test_q25_Daymet_era5 = perm_test(x, grp, stat_func = quantile,
#                                            alternative = "two.sided",
#                                            nsim = B, idx = idx,
#                                            prob = 0.25, perm_idxs = perm_idxs,
#                                            cl = mc_cores)

std_perm_test_q50_Daymet_era5 = perm_test(x, grp, stat_func = quantile,
                                           alternative = "two.sided",
                                           nsim = B, idx = idx,
                                           prob = 0.50, perm_idxs = perm_idxs,
                                           cl = mc_cores)

# std_perm_test_q75_Daymet_era5 = perm_test(x, grp, stat_func = quantile,
#                                            alternative = "two.sided",
#                                            nsim = B, idx = idx,
#                                            prob = 0.75, perm_idxs = perm_idxs,
#                                            cl = mc_cores)
# 
# std_perm_test_q95_Daymet_era5 = perm_test(x, grp, stat_func = quantile,
#                                            alternative = "two.sided",
#                                            nsim = B, idx = idx,
#                                            prob = 0.95, perm_idxs = perm_idxs,
#                                            cl = mc_cores)

std_perm_test_iqr_Daymet_era5 = perm_test(x, grp, stat_func = iqr,
                                           alternative = "two.sided",
                                           nsim = B, idx = idx, perm_idxs = perm_idxs,
                                           cl = mc_cores)

save(std_perm_test_mean_Daymet_era5,
     file = file.path(results_dir, "std_perm_test_mean_Daymet_era5_subtest.rda"),
     compress = "xz")
save(std_perm_test_sd_Daymet_era5,
     file = file.path(results_dir, "std_perm_test_sd_Daymet_era5_subtest.rda"),
     compress = "xz")
# save(std_perm_test_q05_Daymet_era5,
#      file = file.path(results_dir, "std_perm_test_q05_Daymet_era5_subtest.rda"),
#      compress = "xz")
# save(std_perm_test_q25_Daymet_era5,
#      file = file.path(results_dir, "std_perm_test_q25_Daymet_era5_subtest.rda"),
#      compress = "xz")
save(std_perm_test_q50_Daymet_era5,
     file = file.path(results_dir, "std_perm_test_q50_Daymet_era5_subtest.rda"),
     compress = "xz")
# save(std_perm_test_q75_Daymet_era5,
#      file = file.path(results_dir, "std_perm_test_q75_Daymet_era5_subtest.rda"),
#      compress = "xz")
# save(std_perm_test_q95_Daymet_era5,
#      file = file.path(results_dir, "std_perm_test_q95_Daymet_era5_subtest.rda"),
#      compress = "xz")
save(std_perm_test_iqr_Daymet_era5,
     file = file.path(results_dir, "std_perm_test_iqr_Daymet_era5_subtest.rda"),
     compress = "xz")

pdf(file.path(results_dir, "std_perm_tests_Daymet_era5_subtest.pdf"))
plot_test_object(lon, lat, std_perm_test_mean_Daymet_era5, alpha = 0.10, map = "world")
title("mean")
# plot_test_object(lon, lat, std_perm_test_q05_Daymet_era5, alpha = 0.10, map = "world")
# title("0.05 quantile")
# plot_test_object(lon, lat, std_perm_test_q25_Daymet_era5, alpha = 0.10, map = "world")
# title("0.25 quantile")
plot_test_object(lon, lat, std_perm_test_q50_Daymet_era5, alpha = 0.10, map = "world")
title("0.50 quantile")
# plot_test_object(lon, lat, std_perm_test_q75_Daymet_era5, alpha = 0.10, map = "world")
# title("0.75 quantile")
# plot_test_object(lon, lat, std_perm_test_q95_Daymet_era5, alpha = 0.10, map = "world")
# title("0.95 quantile")
# plot_test_object(lon, lat, std_perm_test_sd_Daymet_era5, alpha = 0.10, map = "world")
title("sd")
plot_test_object(lon, lat, std_perm_test_iqr_Daymet_era5, alpha = 0.10, map = "world")
title("iqr")
dev.off()
}

B = 999

set.seed(58)

if (!file.exists(file.path(results_dir, "sp_test_mean_Daymet_era5_subtest.rda"))) {
start_time = Sys.time()
sp_test_mean_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = mean,
                                            alternative = "two.sided",
                                            nsim = B, idx = idx, cl = mc_cores)
Sys.time() - start_time
save(sp_test_mean_Daymet_era5,
     file = file.path(results_dir, "sp_test_mean_Daymet_era5_subtest.rda"),
     compress = "xz")
}

set.seed(59)

if (!file.exists(
  file.path(results_dir, "sp_test_sd_Daymet_era5_subtest.rda")
)) {

sp_test_sd_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = sd,
                                          alternative = "two.sided",
                                          nsim = B, idx = idx, cl = mc_cores)
save(sp_test_sd_Daymet_era5,
     file = file.path(results_dir, "sp_test_sd_Daymet_era5_subtest.rda"),
     compress = "xz")
}

set.seed(60)

if (!file.exists(
  file.path(results_dir, "sp_test_iqr_Daymet_era5_subtest.rda")
)) {
  start_time = Sys.time()
  sp_test_iqr_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = iqr,
                                             alternative = "two.sided",
                                             nsim = B, idx = idx, cl = mc_cores)
  Sys.time() - start_time
  save(sp_test_iqr_Daymet_era5,
       file = file.path(results_dir, "sp_test_iqr_Daymet_era5_subtest.rda"),
       compress = "xz")
}

set.seed(61)

# if (!file.exists(
#   file.path(results_dir, "sp_test_q05_Daymet_era5_subtest.rda")
#   )) {
#   sp_test_q05_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = quantile,
#                                              alternative = "two.sided",
#                                              nsim = B, idx = idx, cl = mc_cores,
#                                              prob = 0.05)
#   save(sp_test_q05_Daymet_era5,
#        file = file.path(results_dir, "sp_test_q05_Daymet_era5_subtest.rda"),
#        compress = "xz")
#   
# }

set.seed(62)

# if (!file.exists(
#   file.path(results_dir, "sp_test_q25_Daymet_era5_subtest.rda")
# )) {
#   sp_test_q25_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = quantile,
#                                              alternative = "two.sided",
#                                              nsim = B, idx = idx, cl = mc_cores,
#                                              prob = 0.25)
#   save(sp_test_q25_Daymet_era5,
#        file = file.path(results_dir, "sp_test_q25_Daymet_era5_subtest.rda"),
#        compress = "xz")
#   
# }

set.seed(63)

if (!file.exists(
  file.path(results_dir, "sp_test_q50_Daymet_era5_subtest.rda")
)) {
  sp_test_q50_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = quantile,
                                             alternative = "two.sided",
                                             nsim = B, idx = idx, cl = mc_cores,
                                             prob = 0.50)
  save(sp_test_q50_Daymet_era5,
       file = file.path(results_dir, "sp_test_q50_Daymet_era5_subtest.rda"),
       compress = "xz")
  
}

set.seed(64)

# if (!file.exists(
#   file.path(results_dir, "sp_test_q75_Daymet_era5_subtest.rda")
# )) {
#   sp_test_q75_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = quantile,
#                                              alternative = "two.sided",
#                                              nsim = B, idx = idx, cl = mc_cores,
#                                              prob = 0.75)
#   save(sp_test_q75_Daymet_era5,
#        file = file.path(results_dir, "sp_test_q75_Daymet_era5_subtest.rda"),
#        compress = "xz")
#   
# }

set.seed(65)

# if (!file.exists(
#   file.path(results_dir, "sp_test_q95_Daymet_era5_subtest.rda")
# )) {
#   sp_test_q95_Daymet_era5 = strat_perm_test(x, grp, strata, stat_func = quantile,
#                                              alternative = "two.sided",
#                                              nsim = B, idx = idx, cl = mc_cores,
#                                              prob = 0.95)
#   save(sp_test_q95_Daymet_era5,
#        file = file.path(results_dir, "sp_test_q95_Daymet_era5_subtest.rda"),
#        compress = "xz")
#   
# }

set.seed(67)

if (!file.exists(
  file.path(results_dir, "sp_test_smooth_Daymet_era5_subtest.rda"))) {
  sp_test_smooth_Daymet_era5 = 
    strat_perm_test_bs_coefs(x, grp, strata, nsim = B,
                             idx = idx, cl = mc_cores)
  save(sp_test_smooth_Daymet_era5,
       file = file.path(results_dir, "sp_test_smooth_Daymet_era5_subtest.rda"),
       compress = "xz")
}

set.seed(68)

if (!file.exists(
  file.path(results_dir, "std_perm_test_smooth_Daymet_era5_subtest.rda"))) {
  
  std_perm_test_smooth_Daymet_era5 = 
    perm_test_bs_coefs(x, grp = grp, nsim = ncol(perm_idxs),
                       idx = idx,
                       cl = mc_cores, perm_idxs = perm_idxs)
  save(std_perm_test_smooth_Daymet_era5,
       file = file.path(results_dir, "std_perm_test_smooth_Daymet_era5_subtest.rda"),
       compress = "xz")
}
